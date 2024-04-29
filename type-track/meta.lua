---a collection of classes describing how all types are constructed

local muun = require("type-track.muun")

---@class Inheritable
---@field __class any
local Inheritable = muun("Inheritable")

---@param self unknown
---@param cls unknown
---@return boolean
function Inheritable.is_instance(self, cls)
	if type(self) ~= "table" or not cls then
		return false
	end

	local obj_cls = self.__class
	while obj_cls and obj_cls ~= cls do
		obj_cls = obj_cls.__parent
	end

	return obj_cls == cls
end

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
---@class type-track.LazyRef.Class
---@overload fun(): type-track.LazyRef
local LazyRef

---the top type. It behaves like a union of all types.
---@type type-track.Unknown
local Unknown

---@type type-track.Tuple
local unknown_var

---the bottom type. It behaves like an intersection of all types.
---@type type-track.Never
local Never

---@type fun(actual: type-track.Type, list: type-track.Type[]): boolean
local is_subset_of_any

---@type fun(actual: type-track.Type, list: type-track.Type[], i?: integer, j?: integer): boolean
local is_subset_of_all

---@type fun(list: type-track.Type[], expected: type-track.Type, i?: integer, j?: integer): boolean
local all_are_subset

---@type fun(list: type-track.Type[], expected: type-track.Type, i?: integer, j?: integer): boolean
local any_are_subset

---determines whether `subtype` is a subset of `supertype`. This operation is
---available across all
---@param subset type-track.Type
---@param superset type-track.Type
---@return boolean
local function is_subset(subset, superset)
	-- Tuple, Union, Intersection, Operator, Literal
	-- 36 combinations
	-- sub-super
	-- TT TU TI TO TL
	-- UT UU UI UO UL
	-- IT IU II IO IL
	-- OT OU OI OO OL
	-- LT LU LI LO LL

	-- If both types have the same class, their respective compare method is
	-- used. Otherwise, coerce the types to something else and compare that?

	---@diagnostic disable-next-line: undefined-field, need-check-nil
	while subset.__class == LazyRef and subset.value do
		---@cast subset type-track.LazyRef
		subset = subset.value
	end
	---@cast subset type-track.Type

	---@diagnostic disable-next-line: undefined-field, need-check-nil
	while superset.__class == LazyRef and superset.value do
		---@cast superset type-track.LazyRef
		superset = superset.value
	end
	---@cast superset type-track.Type

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
		return is_subset(subset.ops, superset)
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
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local TypeInst = muun("Type", Inheritable)

	Type = TypeInst --[[@as type-track.Type.Class]]

	function Type.is_subset(subset, superset)
		return false
	end

	---attempts to evaluate an operation on this type
	---
	---If `nil` is returned, the call wasn't compatible. Otherwise, a return type
	---is expected. Use a `Tuple` for multiple parameters or return types.
	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function TypeInst:eval(op, params)
		error("not implemented")
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

	---defines the algorithm for converting a type into its
	---simplest form
	---@return type-track.Type
	function TypeInst:unify()
		return self
	end

	---returns a union of its operands. This will concatenate unions when
	---possible.
	---@param other type-track.Type
	---@return type-track.Union
	function TypeInst:__add(other)
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
		base.__div = super_base.__div
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

	---@param visited { [type-track.Type]: number?, n: number }?
	---@return string
	function TupleInst:__tostring(visited)
		visited = visited or { n = 0 }

		local strings = {} ---@type string[]
		for _, t in ipairs(self.types) do
			table.insert(strings, t:sub_visited(visited))
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

		if params.__class ~= Tuple then
			params = Tuple({ params })
		end

		if is_subset(params, self.params) then
			return self.returns
		else
			return nil
		end
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

		-- if any operation is unsupported, the entire union doesn't support it
		for _, type in ipairs(self.types) do
			local returns = type:eval(op, params)
			if returns then
				table.insert(all_returns, returns)
			else
				return nil
			end
		end

		-- if we get here, the call has succeeded...
		-- what should the return types be?
		--[[
			a: () -> string
			b: () -> number
			f: a | b

			c = f() -- it could be a string or a number...
		]]
		-- I guess return a union of the return values
		return Union(all_returns)
	end

	---@param i integer
	---@return type-track.Type?
	function UnionInst:at(i)
		local all_indexes = {} ---@type type-track.Type[]

		for _, type in ipairs(self.types) do
			local elem = type:at(i)
			if elem ~= nil then
				table.insert(all_indexes, elem)
			end
		end

		if #all_indexes == 0 then
			-- basically return never
			return Tuple({})
		elseif #all_indexes == 1 then
			return all_indexes[1]
		else
			return Union(all_indexes)
		end
	end

	---@param visited { [type-track.Type]: number?, n: number }
	function UnionInst:__tostring(visited)
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
	---@field is_collected boolean
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
		self.is_collected = false
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
				table.insert(all_returns)
			end
		end

		if #all_returns == 0 then
			-- all_returns may be empty
			return nil
		elseif #all_returns == 1 then
			-- if we get here, at least one call succeeded
			-- if only one call succeeded, return just that
			return all_returns[1]
		else
			-- if we get here, at least two calls succeeded
			-- I suppose just return an intersection of the returns
			return Intersection(all_returns)
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
			return Tuple({})
		elseif #all_indexes == 1 then
			return all_indexes[1]
		else
			return Intersection(all_indexes)
		end
	end

	-- (A | B) & (A | C) == A | (B & C)
	---@param collected_types type-track.Type[]?
	---@return type-track.Intersection
	function IntersectionInst:unify(collected_types)
		if self.is_collected then
			return self
		end

		collected_types = collected_types or {}
		for _, type in ipairs(self.types) do
			if type.__class == Intersection then
				---@cast type type-track.Intersection
				type:unify(collected_types)
			else
				table.insert(collected_types, type)
			end
		end

		local result = Intersection(collected_types)
		result.is_collected = true
		return result
	end

	function IntersectionInst:__tostring(visited)
		local strings = {}
		for _, t in ipairs(self.types) do
			table.insert(strings, tostring(t))
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

	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function Literal:eval(op, params)
		return self.ops:eval(op, params)
	end

	---@param subset type-track.Literal
	---@param superset type-track.Literal
	---@return boolean
	function Literal.is_subset(subset, superset)
		return subset.value == superset.value
	end

	function LiteralInst:__tostring()
		return string.format(
			'"%s: %s"',
			tostring(self.value),
			tostring(self.ops)
		)
	end
end

do -- LazyRef
	---@class type-track.LazyRef : type-track.Type
	---@field value type-track.Type?
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local LazyRefInst = muun("LazyRef", Type)

	LazyRef = LazyRefInst --[[@as type-track.LazyRef.Class]]

	---@return type-track.Type
	function LazyRefInst:unwrap()
		local value = assert(self.value, "attempt to use an empty LazyRef")
		return value
	end

	---@param op string
	---@param params type-track.Type
	---@return type-track.Type? returns
	function LazyRefInst:eval(op, params)
		return self:unwrap():eval(op, params)
	end

	---@param i integer
	---@return type-track.Type?
	function LazyRefInst:at(i)
		return self:unwrap():at(i)
	end

	---@param visited { [type-track.Type]: true? }
	---@return type-track.Type
	function LazyRefInst:unify(visited)
		return self:unwrap():unify(visited)
	end

	function LazyRefInst:__tostring()
		return self:unwrap():__tostring()
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

return {
	Tuple = Tuple,
	Operator = Operator,
	Union = Union,
	Intersection = Intersection,
	Literal = Literal,
	LazyRef = LazyRef,

	Type = Type,

	is_subset = is_subset,
}
