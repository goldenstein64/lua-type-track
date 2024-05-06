---a collection of classes describing how all types are constructed

local muun = require("type-track.muun")
local Inheritable = require("type-track.Inheritable")
local permute = require("type-track.permute")

---a multi-value type. Importantly, it describes how arguments and return
---values are structured.
---@class type-track.Tuple.Class
---@field default_var_arg type-track.Type?
---@overload fun(types: type-track.Type[], var_arg?: type-track.Type): type-track.Tuple
local Tuple

---the base structural type
---@class type-track.Operator.Class
---@overload fun(op: string, params: type-track.Type, returns: type-track.Type): type-track.Operator
local Operator

---an aggregation declaring at least one type lives in its value
---@class type-track.Union.Class
---@overload fun(types: type-track.Type[]): type-track.Union
local Union

---an aggregation declaring all types live in its value
---@class type-track.Intersection.Class
---@overload fun(types: type-track.Type[]): type-track.Intersection
local Intersection

---a type that contains exactly one value
---@class type-track.Literal.Class
---@overload fun(value: unknown, ops: type-track.Type): type-track.Literal
local Literal

---@class type-track.Type.Class
---@field __base any
---@overload fun(): type-track.Type
local Type

---allows separating a type's declaration from its definition
---@class type-track.Free.Class
---@overload fun(): type-track.Free
local Free

---the top type. It behaves like a union of all types.
---@type type-track.Unknown
local Unknown

---@type type-track.Tuple
local unknown_var

---the bottom type. It behaves like an intersection of all types.
---@type type-track.Never
local Never

---@alias type-track.GenericOperator.derive_fn fun(type_params: type-track.Type): (params: type-track.Type?, returns: type-track.Type?)
---@alias type-track.GenericOperator.infer_fn fun(params: type-track.Type, returns: type-track.Type?): (type_params: type-track.Type?)

---an operator that captures its types on usage
---
---```lua
---a: <T>(T) -> T -- a generic call operator
---b: (string) -> string
---
----- valid
---b = a
---
----- NOT valid
---a = b
---```
---
---The constructor takes a `result_of` and `params_of` argument.
---
---- `result_of` takes its type parameters and returns its usage.
---
---- `params_of` takes the parameters from its usage and returns its type
---  parameters.
---
---Example:
---
---```lua
----- <T>(T, T) -> (T?)
---local example = GenericOperator(
---  'call',
---  function(type_params) -- derive
---    local T = type_params:at(1)
---    if not T then return nil end
---
---    return Tuple({T, T}), T + _nil
---  end,
---  function(params, returns) -- infer
---    return params:at(1)
---  end
---)
---```
---
---generating the type functions programmatically has not been explored.
---@class type-track.GenericOperator.Class
---@overload fun(op: string, derive_fn: type-track.GenericOperator.derive_fn, infer_fn: type-track.GenericOperator.infer_fn): type-track.GenericOperator
local GenericOperator

---@type fun(actual: type-track.Type, list: type-track.Type[], i?: integer, j?: integer): boolean
local is_subset_of_any

---@type fun(actual: type-track.Type, list: type-track.Type[], i?: integer, j?: integer): boolean
local is_subset_of_all

---@type fun(list: type-track.Type[], expected: type-track.Type, i?: integer, j?: integer): boolean
local all_are_subset

---@type fun(list: type-track.Type[], expected: type-track.Type, i?: integer, j?: integer): boolean
local any_are_subset

---determines whether `subtype` is a subset of `supertype`. This operation is
---supported for every type in this file.
---
---It is essentially a test to see if this assignment passes:
---
---```lua
---x: subset
---y: superset
---y = x
---```
---@param subset type-track.Type
---@param superset type-track.Type
---@return boolean
local function is_subset(subset, superset)
	-- Tuple, Union, Intersection, Operator, Literal
	-- 25 combinations
	-- sub-super
	-- TT TU TI TO TL
	-- UT UU UI UO UL
	-- IT IU II IO IL
	-- OT OU OI OO OL
	-- LT LU LI LO LL

	-- If both types have the same class, their respective compare method is
	-- used. Otherwise, coerce the types to something else and compare that?

	subset = subset:unify()
	superset = superset:unify()

	while true do
		local sub_cls = subset.__class
		if sub_cls == Free then
			---@cast subset type-track.Free
			subset = subset:unwrap()
		elseif sub_cls == GenericOperator then
			---@cast subset type-track.GenericOperator
			local new_subset = subset:match(superset)
			if not new_subset then
				return false
			end
			subset = new_subset
		else
			break
		end
	end

	while true do
		local super_cls = superset.__class
		if super_cls == Free then
			---@cast superset type-track.Free
			superset = superset:unwrap()
		elseif super_cls == GenericOperator then
			---@cast superset type-track.GenericOperator
			superset = assert(superset.derive_fn(unknown_var))
		else
			break
		end
	end

	if subset == Never or superset == Unknown then
		return true
	end

	if subset == Unknown or superset == Never then
		return false
	end

	local sub_cls = subset.__class
	local super_cls = superset.__class
	if sub_cls == super_cls then
		return sub_cls.is_subset(subset, superset)
	end

	-- supertype comparisons
	if super_cls == Tuple then
		---@cast superset type-track.Tuple
		---@cast subset type-track.Operator | type-track.Intersection | type-track.Union | type-track.Literal
		local superset_len = #superset.types
		if superset_len == 0 then
			return not superset.var_arg or is_subset(subset, superset.var_arg)
		elseif superset_len == 1 then
			return is_subset(subset, superset.types[1])
		elseif Tuple.default_var_arg then
			for i = 2, superset_len do
				local super_elem = superset.types[i]
				if not is_subset(Tuple.default_var_arg, super_elem) then
					return false
				end
			end

			return is_subset(subset, superset.types[1])
		end

		return false
	elseif super_cls == Union then
		---@cast superset type-track.Union
		---@cast subset type-track.Tuple | type-track.Intersection | type-track.Operator | type-track.Literal
		return is_subset_of_any(subset, superset.types)
	elseif super_cls == Intersection then
		---@cast superset type-track.Intersection
		---@cast subset type-track.Tuple | type-track.Union | type-track.Operator | type-track.Literal
		return is_subset_of_all(subset, superset.types)
	end

	-- subtype comparisons
	if sub_cls == Tuple then
		---@cast subset type-track.Tuple
		---@cast superset type-track.Operator | type-track.Literal
		return Tuple.is_subset(subset, Tuple({ superset }))
	elseif sub_cls == Union then
		---@cast subset type-track.Union
		---@cast superset type-track.Operator | type-track.Literal
		-- the use cases for this are not significant either
		return all_are_subset(subset.types, superset)
	elseif sub_cls == Intersection then
		---@cast subset type-track.Intersection
		---@cast superset type-track.Operator | type-track.Literal
		return any_are_subset(subset.types, superset)
	elseif sub_cls == Literal then
		---@cast subset type-track.Literal
		---@cast superset type-track.Operator
		local subset_ops = subset.ops
		if subset_ops then
			return is_subset(subset_ops, superset)
		end
	elseif sub_cls == Operator then
		---@cast subset type-track.Operator
		---@cast superset type-track.Literal
		return false
	end

	return false
end

---accepts a type `actual` if it is a subset of all types in `list`
---@param actual type-track.Type
---@param list type-track.Type[]
---@param i? integer
---@param j? integer
---@return boolean
function is_subset_of_all(actual, list, i, j)
	for k = i or 1, j or #list do
		local expected = list[k]
		if not is_subset(actual, expected) then
			return false
		end
	end

	return true
end

---accepts a type `actual` if it is a subset of any type in `list`
---@param actual type-track.Type
---@param list type-track.Type[]
---@param i? integer
---@param j? integer
---@return boolean
function is_subset_of_any(actual, list, i, j)
	for k = i or 1, j or #list do
		local expected = list[k]
		if is_subset(actual, expected) then
			return true
		end
	end

	return false
end

---@param list type-track.Type[]
---@param expected type-track.Type
---@param i? integer
---@param j? integer
---@return boolean
function all_are_subset(list, expected, i, j)
	for k = i or 1, j or #list do
		local actual = list[k]
		if not is_subset(actual, expected) then
			return false
		end
	end

	return true
end

---@param list type-track.Type[]
---@param expected type-track.Type
---@param i? integer
---@param j? integer
---@return boolean
function any_are_subset(list, expected, i, j)
	for k = i or 1, j or #list do
		local actual = list[k]
		if is_subset(actual, expected) then
			return true
		end
	end

	return false
end

do -- Type
	---@class type-track.Type : Inheritable
	---@field unified type-track.Type?
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local TypeInst = muun("Type", Inheritable)

	Type = TypeInst --[[@as type-track.Type.Class]]

	function Type.is_subset(subset, superset)
		return false
	end

	---attempts to evaluate `op` on this type with `params`
	---
	---If `nil` is returned, the operation wasn't compatible. Otherwise, a type
	---is returned. A `Tuple` is used for multiple return types.
	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function TypeInst:eval(op, params)
		error("not implemented")
	end

	---returns what parameters this type supports for `op`
	---
	---If `nil` is returned, the operation isn't compatible. Otherwise, a type is
	---returned. A `Tuple` is used for multiple parameter types.
	---@param op string
	---@return type-track.Type?
	function TypeInst:get_params(op)
		return nil
	end

	---returns the `i`th element in this type
	---
	---This implementation is typically moot for types of a single value, like
	---`Callable` or `Object`, but useful for `Tuple`s or `Tuple`-containing
	---types.
	---@param i integer
	---@return type-track.Type?
	function TypeInst:at(i)
		return i == 1 and self or Tuple.default_var_arg
	end

	---converts a type into its simplest form. It always returns a new Type.
	---
	---Note: If you plan to change the behavior of this method, override
	---`Type:_unify()`.
	---@return type-track.Type
	function TypeInst:unify()
		if self.unified then
			return self.unified
		end

		local proxy = Free()
		proxy.unified = proxy
		self.unified = proxy
		local result = self:_unify()
		proxy.value = result
		proxy.unified = result
		self.unified = result
		return result
	end

	---defines the algorithm for converting a type into its simplest form. This
	---is a protected method that implements `Type:unify()`. It must always
	---return a new Type unless it is already in its simplest form.
	---
	---The public method makes sure to call `_unify` only when the type hasn't
	---been unified before and sets `unified` to a proxy `Free` type before
	---calling.
	---@return type-track.Type unified
	function TypeInst:_unify()
		return self
	end

	---returns a union of its operands. This will concatenate unions when
	---possible.
	---@param other type-track.Type
	---@return type-track.Union
	function TypeInst:__add(other)
		if self.__class == Free then
			---@cast self type-track.Free
			self = self:unwrap()
		end

		if other.__class == Free then
			---@cast other type-track.Free
			other = other:unwrap()
		end

		local values = {}
		if self.__class == Union then
			---@cast self type-track.Union
			for _, type in ipairs(self.types) do
				table.insert(values, type)
			end
		else
			table.insert(values, self)
		end

		if other.__class == Union then
			---@cast other type-track.Union
			for _, type in ipairs(other.types) do
				table.insert(values, type)
			end
		else
			table.insert(values, other)
		end

		return Union(values)
	end

	---returns an intersection of its operands. This will concatenate
	---intersections when possible.
	---@param other type-track.Type
	---@return type-track.Intersection
	function TypeInst:__mul(other)
		if self.__class == Free then
			---@cast self type-track.Free
			self = self:unwrap()
		end

		if other.__class == Free then
			---@cast other type-track.Free
			other = other:unwrap()
		end

		local values = {}
		if self.__class == Intersection then
			---@cast self type-track.Intersection
			for _, type in ipairs(self.types) do
				table.insert(values, type)
			end
		else
			table.insert(values, self)
		end

		if other.__class == Intersection then
			---@cast other type-track.Intersection
			for _, type in ipairs(other.types) do
				table.insert(values, type)
			end
		else
			table.insert(values, other)
		end

		return Intersection(values)
	end

	---@param visited { [type-track.Type]: number?, n: number }
	---@return string
	function TypeInst:sub_visited(visited)
		local visit_id = visited[self]
		if visit_id then
			return string.format("<%d>", visit_id)
		else
			visited.n = visited.n + 1
			visit_id = visited.n
			visited[self] = visit_id
			return string.format("<%d>%s", visit_id, self:__tostring(visited))
		end
	end

	---every type is serializable. The expectation is, if a type serializes to
	---the same string as another type, they are the same type.
	---@param visited { [type-track.Type]: number?, n: number }?
	---@return string
	function TypeInst:__tostring(visited)
		error("__tostring metamethod is not implemented on this type")
	end

	function Type:__inherited(cls)
		local super_base = self.__base
		local base = cls.__base

		-- set metamethods
		base.__add = super_base.__add
		base.__mul = super_base.__mul
	end
end

do -- Tuple
	---@class type-track.Tuple : type-track.Type
	---@field types type-track.Type[]
	---@field var_arg type-track.Type?
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local TupleInst = muun("Tuple", Type)

	Tuple = TupleInst --[[@as type-track.Tuple.Class]]

	---@param self type-track.Tuple
	---@param types type-track.Type[]
	---@param var_arg? type-track.Type
	function Tuple:new(types, var_arg)
		self.types = types
		self.var_arg = var_arg or Tuple.default_var_arg
	end

	--- compares two tuples
	---
	---```lua
	---local a, b = x, y
	---
	----- a, b is the superset
	----- x, y is the subset
	---```
	---@param subset type-track.Tuple
	---@param superset type-track.Tuple
	---@return boolean
	function Tuple.is_subset(subset, superset)
		local sub_var_arg = subset.var_arg
		local super_var_arg = superset.var_arg

		-- by default, latter elements in a tuple are discarded, meaning anything
		-- can be entered there without an error
		if sub_var_arg and not super_var_arg then
			return false
		end

		-- calling a function with less arguments than required is illegal
		if
			not sub_var_arg
			and super_var_arg
			and #subset.types > #superset.types
		then
			return false
		end

		-- calling a function with mismatching var-args is illegal
		if
			sub_var_arg
			and super_var_arg
			and not is_subset(sub_var_arg, super_var_arg)
		then
			return false
		end

		-- compare element-wise
		for i, super_type in ipairs(superset.types) do
			local sub_type = subset:at(i)
			if not sub_type or not is_subset(sub_type, super_type) then
				return false
			end
		end

		-- compare var-arg
		if super_var_arg then
			---@cast sub_var_arg type-track.Type
			for i = #superset.types + 1, #subset.types do
				local sub_type = subset.types[i]
				if not is_subset(sub_type, super_var_arg) then
					return false
				end
			end
		end

		return true
	end

	---@param i integer
	---@return type-track.Type?
	function TupleInst:at(i)
		return self.types[i] or self.var_arg
	end

	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function TupleInst:eval(op, params)
		local first_elem = self:at(1)
		if first_elem then
			return first_elem:eval(op, params)
		else
			return nil
		end
	end

	---@param op string
	---@return type-track.Type?
	function TupleInst:get_params(op)
		local first_elem = self:at(1)
		if first_elem then
			return first_elem:get_params(op)
		else
			return nil
		end
	end

	---@return type-track.Type
	function TupleInst:_unify()
		local unified_args = {}
		for i, elem in ipairs(self.types) do
			unified_args[i] = elem:unify()
		end

		local unified_var_args = nil
		if self.var_arg then
			unified_var_args = self.var_arg:unify()
		end

		return Tuple(unified_args, unified_var_args)
	end

	local VAR_STR = "...%s"

	---@param visited { [type-track.Type]: number?, n: number }?
	---@return string
	function TupleInst:__tostring(visited)
		visited = visited or { n = 0 }

		local strings = {} ---@type string[]
		for _, t in ipairs(self.types) do
			table.insert(strings, t:sub_visited(visited))
		end

		if self.var_arg ~= Tuple.default_var_arg and self.var_arg then
			table.insert(strings, VAR_STR:format(self.var_arg:sub_visited(visited)))
		end

		return string.format("(%s)", table.concat(strings, ", "))
	end
end

do -- Operator
	---represents a possible operation on a value, e.g. addition, concatenation,
	---etc.
	---@class type-track.Operator : type-track.Type
	---@field params type-track.Type
	---@field returns type-track.Type
	---@field op string
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local OperatorInst = muun("Operator", Type)

	Operator = OperatorInst --[[@as type-track.Operator.Class]]

	---@param self type-track.Operator
	---@param op string
	---@param params type-track.Type
	---@param returns type-track.Type
	function Operator:new(op, params, returns)
		self.params = params
		self.returns = returns
		self.op = op
	end

	---@param subset type-track.Operator
	---@param superset type-track.Operator
	---@return boolean
	function Operator.is_subset(subset, superset)
		return subset.op == superset.op
			and is_subset(subset.params, superset.params)
			and is_subset(superset.returns, subset.returns)
	end

	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function OperatorInst:eval(op, params)
		if op ~= self.op then
			return nil
		end

		if is_subset(params, self.params) then
			return self.returns
		else
			return nil
		end
	end

	---@param op string
	---@return type-track.Type? params
	function OperatorInst:get_params(op)
		if op == self.op then
			return self.params
		else
			return nil
		end
	end

	function OperatorInst:_unify()
		return Operator(self.op, self.params:unify(), self.returns:unify())
	end

	---@param visited { [type-track.Type]: number?, n: number }
	---@return string
	function OperatorInst:__tostring(visited)
		visited = visited or { n = 0 }

		return string.format(
			"{ %s: %s -> %s }",
			self.op,
			self.params:sub_visited(visited),
			self.returns:sub_visited(visited)
		)
	end
end

do -- Union
	---@class type-track.Union : type-track.Type
	---@field types type-track.Type[]
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local UnionInst = muun("Union", Type)

	Union = UnionInst --[[@as type-track.Union.Class]]

	---@param self type-track.Union
	---@param types type-track.Type[]
	function Union:new(types)
		assert(#types >= 2, "unions must have at least two items")
		self.types = types
		-- I'll worry about optimizing this later
	end

	--[[
	A: "A"
	B: "B"
	C: "C"
	AB: A | B
	ABC: A | B | C

	ABC = AB -- good, is_subset(AB, ABC) passes
	AB = ABC -- bad, is_subset(ABC, AB) fails
	]]
	---@param subset type-track.Union
	---@param superset type-track.Union
	---@return boolean
	function Union.is_subset(subset, superset)
		for _, subtype in ipairs(subset.types) do
			if not is_subset_of_any(subtype, superset.types) then
				return false
			end
		end

		return true
	end

	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function UnionInst:eval(op, params)
		---@type type-track.Type[]
		local all_returns = {}

		-- if any element doesn't support it, the entire union doesn't support it
		for _, type in ipairs(self.types) do
			local returns = type:eval(op, params)
			if returns then
				table.insert(all_returns, returns)
			else
				return nil
			end
		end

		assert(#all_returns >= 2, "evaluation on union did not have enough types")
		return Union(all_returns)
	end

	---@param op string
	---@return type-track.Type?
	function UnionInst:get_params(op)
		---@type type-track.Type[]
		local all_params = {}
		for _, type in ipairs(self.types) do
			local params = type:get_params(op)
			if params then
				table.insert(all_params, params)
			else
				return nil
			end
		end

		assert(#all_params >= 2)
		return Intersection(all_params)
	end

	---@param types type-track.Type[]
	local function flatten_union(types)
		local result = {} ---@type type-track.Type[]
		for _, elem in ipairs(types) do
			elem = elem:unify()
			if elem == Unknown then
				return { Unknown }
			elseif elem.__class == Union then
				---@cast elem type-track.Union
				local elem_types = elem.types
				table.move(elem_types, 1, #elem_types, #result + 1, result)
			elseif not is_subset_of_any(elem, result) then
				for i = #result, 1, -1 do
					local type2 = result[i]
					if is_subset(type2, elem) then
						table.remove(result, i)
					end
				end
				table.insert(result, elem)
			end
		end
		return result
	end

	---@return type-track.Type
	function UnionInst:_unify()
		local flattened = flatten_union(self.types)

		assert(#flattened > 0, "no types to unify?")
		if #flattened == 1 then
			return flattened[1]
		else
			return Union(flattened)
		end
	end

	---@param i integer
	---@return type-track.Type?
	function UnionInst:at(i)
		local all_indexes = {} ---@type type-track.Type[]

		for _, type in ipairs(self.types) do
			local elem = type:at(i)
			if elem then
				table.insert(all_indexes, elem)
			end
		end

		if #all_indexes == 0 then
			return nil
		elseif #all_indexes == 1 then
			return all_indexes[1]
		else
			return Union(all_indexes)
		end
	end

	---@param visited { [type-track.Type]: number?, n: number }
	function UnionInst:__tostring(visited)
		visited = visited or { n = 0 }

		local strings = {} ---@type string[]
		for _, t in ipairs(self.types) do
			table.insert(strings, t:sub_visited(visited))
		end
		table.sort(strings)
		return string.format("[%s]", table.concat(strings, " | "))
	end
end

do -- Intersection
	---@class type-track.Intersection : type-track.Type
	---@field types type-track.Type[]
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local IntersectionInst = muun("Intersection", Type)

	---@class type-track.Intersection.Class
	Intersection = IntersectionInst --[[@as type-track.Intersection.Class]]

	---@param self type-track.Intersection
	---@param types type-track.Type[]
	function Intersection:new(types)
		assert(#types >= 2, "intersections must have at least two items")
		self.types = types
		-- I'll worry about optimizing this later
	end

	---@param subset type-track.Intersection
	---@param superset type-track.Intersection
	---@return boolean
	function Intersection.is_subset(subset, superset)
		for _, supertype in ipairs(superset.types) do
			if not is_subset_of_any(supertype, subset.types) then
				return false
			end
		end

		return true
	end

	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function IntersectionInst:eval(op, params)
		---@type type-track.Type[]
		local all_returns = {}

		-- it's okay if some calls are not supported here
		for _, type in ipairs(self.types) do
			local returns = type:eval(op, params)
			if returns then
				table.insert(all_returns, returns)
			end
		end

		if #all_returns == 0 then
			return nil
		elseif #all_returns == 1 then
			return all_returns[1]
		else
			return Intersection(all_returns)
		end
	end

	---@param op string
	---@return type-track.Type?
	function IntersectionInst:get_params(op)
		---@type type-track.Type[]
		local all_params = {}
		for _, type in ipairs(self.types) do
			local params = type:get_params(op)
			if params then
				table.insert(all_params, params)
			end
		end

		if #all_params == 0 then
			return nil
		elseif #all_params == 1 then
			return all_params[1]
		else
			return Union(all_params)
		end
	end

	---@param i integer
	---@return type-track.Type?
	function IntersectionInst:at(i)
		local all_indexes = {} ---@type type-track.Type[]

		for _, type in ipairs(self.types) do
			local elem = type:at(i)
			if elem then
				table.insert(all_indexes, elem)
			end
		end

		if #all_indexes == 0 then
			return nil
		elseif #all_indexes == 1 then
			return all_indexes[1]
		else
			return Intersection(all_indexes)
		end
	end

	---@param types type-track.Type[]
	---@return type-track.Type[]
	local function flatten_intersection(types)
		local result = {} ---@type type-track.Type[]
		for _, elem in ipairs(types) do
			elem = elem:unify()
			if elem == Never then
				return { Never }
			elseif elem.__class == Intersection then
				---@cast elem type-track.Intersection
				local elem_types = elem.types
				table.move(elem_types, 1, #elem_types, #result + 1, result)
			elseif not any_are_subset(result, elem) then
				for i = #result, 1, -1 do
					local type2 = result[i]
					if is_subset(elem, type2) then
						table.remove(result, i)
					end
				end
				table.insert(result, elem)
			end
		end

		return result
	end

	---takes all the types that are to be intersected and returns all the types
	---that are to be unioned
	---@param types type-track.Type[]
	---@return type-track.Type[]
	local function distribute_unions(types)
		local all_unions = {} ---@type type-track.Type[][]
		local has_unions = false
		for _, type in ipairs(types) do
			if type.__class == Free then
				---@cast type type-track.Free
				type = type:unwrap()
			end

			if type.__class == Union then
				---@cast type type-track.Union
				table.insert(all_unions, type.types)
				has_unions = true
			else
				table.insert(all_unions, { type })
			end
		end

		if not has_unions then
			if #types == 1 then
				return types[1]
			else
				return Intersection(types)
			end
		end

		local result = {} ---@type type-track.Type[][]
		---@param permutation type-track.Type[]
		for permutation in permute(all_unions) do
			table.insert(result, Intersection(permutation))
		end

		return Union(result):unify()
	end

	-- since unions are more prevalent, intersections should look for unions in
	-- their sub-elements and push them to the top of the type expression
	-- A & (B | C) -> (A & B) | (A & C)
	-- (A | B) & (C | D) -> (A & C) | (A & D) | (B & C) | (B & D)
	-- (...union1) & (...union2) & ... & (unionN)
	--   -> (union1[1] & union2[1] & ... & unionN[1])
	--    | (union1[1] & union2[1] & ... & unionN[2])
	--    | ...
	--    | (union1[1] & union2[1] & ... & unionN[nn])
	--    | (union1[1] & union2[1] & ... & unionN_1[2] & unionN[1])
	--    | (union1[1] & union2[1] & ... & unionN_1[2] & unionN[2])
	--    | ...
	--    | (union1[1] & union2[1] & ... & unionN_1[2] & unionN[n])
	--    | ...
	--    | (union1[n1] & union2[n2] & ... & unionN[nn])
	---@return type-track.Type
	function IntersectionInst:_unify()
		local flattened = flatten_intersection(self.types)
		assert(#flattened > 0, "no types to unify?")
		if #flattened == 1 then
			return flattened[1]
		end

		return distribute_unions(flattened)
	end

	function IntersectionInst:__tostring(visited)
		visited = visited or { n = 0 }

		local strings = {}
		for _, t in ipairs(self.types) do
			table.insert(strings, t:sub_visited(visited))
		end
		table.sort(strings)
		return string.format("[%s]", table.concat(strings, " & "))
	end
end

do -- Literal
	---@class type-track.Literal : type-track.Type
	---@field value unknown
	---@field ops type-track.Type?
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local LiteralInst = muun("Literal", Type)

	Literal = LiteralInst --[[@as type-track.Literal.Class]]

	---@param self type-track.Literal
	---@param value unknown
	---@param ops? type-track.Type
	function Literal:new(value, ops)
		self.value = value
		self.ops = ops
	end

	---@param subset type-track.Literal
	---@param superset type-track.Literal
	---@return boolean
	function Literal.is_subset(subset, superset)
		return subset.value == superset.value
	end

	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function Literal:eval(op, params)
		if self.ops then
			return self.ops:eval(op, params)
		else
			return nil
		end
	end

	function LiteralInst:__tostring(visited)
		visited = visited or { n = 0 }
		if self.ops then
			return string.format(
				'"%s: %s"',
				self.value,
				self.ops:sub_visited(visited)
			)
		else
			return string.format('"%s"', self.value)
		end
	end
end

do -- Free
	---@class type-track.Free : type-track.Type
	---@field value type-track.Type?
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local FreeInst = muun("Free", Type)

	Free = FreeInst --[[@as type-track.Free.Class]]

	---@return type-track.Type
	function FreeInst:unwrap()
		local value = assert(self.value, "attempt to use an empty Free type")
		if value.__class == Free then
			---@cast value type-track.Free
			return value:unwrap()
		else
			return value
		end
	end

	---@return type-track.Type? returns
	function FreeInst:eval(...)
		return self:unwrap():eval(...)
	end

	---@return type-track.Type?
	function FreeInst:get_params(...)
		return self:unwrap():get_params(...)
	end

	---@return type-track.Type?
	function FreeInst:at(...)
		return self:unwrap():at(...)
	end

	---@return type-track.Type
	function FreeInst:_unify()
		return self:unwrap():unify()
	end

	function FreeInst:__tostring(...)
		return self:unwrap():__tostring(...)
	end
end

do -- Unknown
	---@class type-track.Unknown.Class
	---@overload fun(): type-track.Unknown
	local UnknownClass = muun("Unknown", Type) --[[@as type-track.Unknown.Class]]

	---@class type-track.Unknown : type-track.Type
	local UnknownInst = UnknownClass --[[@as type-track.Unknown]]

	function UnknownInst:__tostring()
		return "unknown"
	end

	---Evaluations are never valid on `Unknown`.
	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function UnknownInst:eval(op, params)
		return nil
	end

	Unknown = UnknownClass()
	unknown_var = Tuple({}, Unknown)
end

do -- Never
	---@class type-track.Never.Class
	---@overload fun(): type-track.Never
	local NeverClass = muun("Never", Type)

	---@class type-track.Never : type-track.Type
	local NeverInst = NeverClass --[[@as type-track.Never]]

	function NeverInst:__tostring()
		return "never"
	end

	---Evaluations are always valid on `Never` and returns itself.
	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function NeverInst:eval(op, params)
		return Never
	end

	---@param i number
	---@return type-track.Type?
	function NeverInst:at(i)
		return Tuple.default_var_arg
	end

	Never = NeverClass()
end

do -- GenericOperator
	---@class type-track.GenericOperator : type-track.Type
	---@field op string
	---@field derive_fn type-track.GenericOperator.derive_fn
	---@field infer_fn type-track.GenericOperator.infer_fn
	local GenericOperatorInst = muun("GenericOperator", Type)

	GenericOperator = GenericOperatorInst --[[@as type-track.GenericOperator.Class]]

	---@param op string
	---@param derive_fn type-track.GenericOperator.derive_fn
	---@param infer_fn type-track.GenericOperator.infer_fn
	function GenericOperator:new(op, derive_fn, infer_fn)
		self.op = op
		self.derive_fn = derive_fn
		self.infer_fn = infer_fn
	end

	---@param subset type-track.GenericOperator
	---@param superset type-track.GenericOperator
	---@return boolean
	function GenericOperator.is_subset(subset, superset)
		local subset_params, subset_returns = subset.derive_fn(unknown_var)
		if not subset_params or not subset_returns then
			return false
		end

		local superset_params, superset_returns = superset.derive_fn(unknown_var)
		if not superset_params or not superset_returns then
			return false
		end

		return subset.op == superset.op
			and is_subset(subset_params, superset_params)
			and is_subset(superset_returns, subset_returns)
	end

	---@param op string
	---@param params type-track.Type
	---@return type-track.Type?
	function GenericOperatorInst:eval(op, params)
		if op ~= self.op then
			return nil
		end

		local type_params = self.infer_fn(params)
		if not type_params then
			return nil
		end

		local self_params, self_returns = self.derive_fn(type_params)
		if self_params and is_subset(params, self_params) then
			return self_returns
		else
			return nil
		end
	end

	---@param op string
	---@return type-track.Type?
	function GenericOperatorInst:get_params(op)
		if op ~= self.op then
			return nil
		end

		local self_params = self.derive_fn(unknown_var)
		return self_params
	end

	---returns a concrete `Operator` that is a subset of `superset` for this
	---type's `op`
	---
	---If `nil` is returned, then `supertype` could not be matched for this
	---operator.
	---@param superset type-track.Type
	---@return type-track.Type? concrete
	function GenericOperatorInst:match(superset)
		local super_params = superset:get_params(self.op)
		if not super_params then
			return nil
		end

		local super_returns = superset:eval(self.op, super_params)
		local type_params = self.infer_fn(super_params, super_returns)
		if not type_params then
			return nil
		end

		local self_params, self_returns = self.derive_fn(type_params)
		if not self_params then
			return nil
		end

		return Operator(self.op, self_params, self_returns)
	end

	---@param visited { [type-track.Type]: true? }
	function GenericOperatorInst:__tostring(visited)
		visited = visited or { n = 0 }
		visited[self] = true
		return string.format("{ %s(): %s }", self.op, self.derive_fn(unknown_var))
	end
end

return {
	Tuple = Tuple,
	Operator = Operator,
	Union = Union,
	Intersection = Intersection,
	Literal = Literal,
	Free = Free,
	Never = Never,
	Unknown = Unknown,
	GenericOperator = GenericOperator,

	Type = Type,

	is_subset = is_subset,
}
