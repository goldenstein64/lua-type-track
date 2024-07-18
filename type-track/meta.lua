---a collection of classes describing how all types are constructed
local meta = {}

local muun = require("type-track.muun")
local Inheritable = require("type-track.Inheritable")
local permute = require("type-track.permute")

---a multi-value type. Importantly, it describes how arguments and return
---values are structured.
---@class type-track.Tuple.Class
---@overload fun(elements: type-track.Type[], var?: type-track.Type): type-track.Tuple
local Tuple

---the base structural type
---@class type-track.Operation.Class
---@overload fun(op: string, domain: type-track.Type, range: type-track.Type): type-track.Operation
local Operation

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
---@overload fun(value: unknown, of: type-track.Type): type-track.Literal
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

---@alias type-track.GenericOperation.derive_fn fun(type_params: type-track.Type): (domain: type-track.Type?, range: type-track.Type?)
---@alias type-track.GenericOperation.infer_fn fun(domain: type-track.Type, range: type-track.Type?): (type_params: type-track.Type?)

---an operation that captures its types on usage
---
---```lua
---a: <T>(T) -> T -- a generic call operation
---b: (string) -> string
---
----- valid
---b = a
---
----- NOT valid
---a = b
---```
---
---The constructor takes a `derive_fn` and `infer_fn` argument.
---
---- `derive_fn` takes its type parameters and returns its domain and
---  range.
---
---- `infer_fn` takes the domain and optional range from its usage and
---  returns the type parameters it inferred.
---
---Example:
---
---```lua
----- <T>(T, T) -> (T?)
---local example = GenericOperation(
---  'call',
---  function(type_params) -- derive
---    local T = type_params:at(1)
---    if not T then return nil end
---
---    return Tuple({T, T}), T + _nil
---  end,
---  function(domain, range) -- infer
---    local T1, T2 = domain:at(1), domain:at(2)
---    local T3 = range:at(1) - _nil -- not implemented lol
---    return T1 * T2 * T3
---  end
---)
---```
---
---generating the type functions programmatically has not been explored.
---@class type-track.GenericOperation.Class
---@overload fun(op: string, derive_fn: type-track.GenericOperation.derive_fn, infer_fn: type-track.GenericOperation.infer_fn): type-track.GenericOperation
local GenericOperation

---@type fun(subset: type-track.Type, superset_list: type-track.Type[], i?: integer, j?: integer): boolean
local is_subset_of_any

---@type fun(subset: type-track.Type, superset_list: type-track.Type[], i?: integer, j?: integer): boolean
local is_subset_of_all

---@type fun(subset_list: type-track.Type[], superset: type-track.Type, i?: integer, j?: integer): boolean
local all_are_subset

---@type fun(subset_list: type-track.Type[], superset: type-track.Type, i?: integer, j?: integer): boolean
local any_are_subset

--#region is_subset

---determines whether `subset` is a subset of `superset`
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
	-- If both types have the same class, their respective compare method is
	-- used. Otherwise, coerce the types to something else and compare that?

	if meta.DEBUG then
		print(subset, "<:", superset)
	end

	if rawequal(subset, superset) then
		return true
	end

	do
		local normalized = subset:normalize()
		if not normalized then
			return false
		end
		subset = normalized
	end
	---@cast subset type-track.Type

	do
		local normalized = superset:normalize()
		if not normalized then
			return false
		end
		superset = normalized
	end
	---@cast superset type-track.Type

	if rawequal(subset, superset) then
		return true
	end

	while subset.__class == GenericOperation do
		---@cast subset type-track.GenericOperation
		local new_subset = subset:match(superset)
		if not new_subset then
			return false
		end
		subset = new_subset
	end

	while superset.__class == GenericOperation do
		---@cast superset type-track.GenericOperation
		superset = assert(superset.derive_fn(unknown_var))
	end

	if subset == Never or superset == Unknown then
		return true
	end

	if subset == Unknown or superset == Never then
		return false
	end

	local sub_cls = subset.__class
	local super_cls = superset.__class
	assert(
		sub_cls ~= Free and super_cls ~= Free,
		"attempt to compute is_subset of Free type"
	)

	if sub_cls == super_cls then
		return sub_cls.is_subset(subset, superset)
	end

	-- supertype comparisons
	if super_cls == Tuple then
		---@cast superset type-track.Tuple
		local superset_len = #superset.elements
		if superset_len == 0 then
			return not superset.var or is_subset(subset, superset.var)
		elseif superset_len == 1 then
			return is_subset(subset, superset.elements[1])
		end

		return false
	elseif super_cls == Union then
		---@cast superset type-track.Union
		if sub_cls == Operation then
			---@cast subset type-track.Operation
			local superset_domain = superset:get_domain(subset.op)
			if not superset_domain then
				return false
			end
			local superset_range = superset:eval(subset.op, superset_domain)
			if not superset_range then
				return false
			end

			-- handles (A & C) -> (B | D) <: (A -> B) | (C -> D)
			return is_subset(superset_domain, subset.domain)
				and is_subset(subset.range, superset_range)
		end

		return is_subset_of_any(subset, superset.types)
	elseif super_cls == Intersection then
		---@cast superset type-track.Intersection
		return is_subset_of_all(subset, superset.types)
	end

	-- subtype comparisons
	if sub_cls == Tuple then
		---@cast subset type-track.Tuple
		---@cast superset type-track.Operation | type-track.Literal
		return Tuple.is_subset(subset, Tuple({ superset }))
	elseif sub_cls == Union then
		---@cast subset type-track.Union
		---@cast superset type-track.Operation | type-track.Literal
		-- the use cases for this are not significant
		return all_are_subset(subset.types, superset)
	elseif sub_cls == Intersection then
		---@cast subset type-track.Intersection
		---@cast superset type-track.Operation | type-track.Literal
		return any_are_subset(subset.types, superset)
	elseif sub_cls == Literal then
		---@cast subset type-track.Literal
		---@cast superset type-track.Operation
		return is_subset(subset.of, superset)
	elseif sub_cls == Operation then
		---@cast subset type-track.Operation
		---@cast superset type-track.Literal
		return false
	end

	return false
end

---accepts a type `subset` if it is a subset of all types in `superset_list`
---@param subset type-track.Type
---@param superset_list type-track.Type[]
---@param i? integer
---@param j? integer
---@return boolean
function is_subset_of_all(subset, superset_list, i, j)
	for k = i or 1, j or #superset_list do
		local expected = superset_list[k]
		if not is_subset(subset, expected) then
			return false
		end
	end

	return true
end

---accepts a type `subset` if it is a subset of any type in `superset_list`
---@param subset type-track.Type
---@param superset_list type-track.Type[]
---@param i? integer
---@param j? integer
---@return boolean
function is_subset_of_any(subset, superset_list, i, j)
	for k = i or 1, j or #superset_list do
		local expected = superset_list[k]
		if is_subset(subset, expected) then
			return true
		end
	end

	return false
end

---accepts a type `superset` if it is a superset of all types in `subset_list`
---@param subset_list type-track.Type[]
---@param superset type-track.Type
---@param i? integer
---@param j? integer
---@return boolean
function all_are_subset(subset_list, superset, i, j)
	for k = i or 1, j or #subset_list do
		local actual = subset_list[k]
		if not is_subset(actual, superset) then
			return false
		end
	end

	return true
end

---accepts a type `superset` if it is a superset of any type in `subset_list`
---@param subset_list type-track.Type[]
---@param superset type-track.Type
---@param i? integer
---@param j? integer
---@return boolean
function any_are_subset(subset_list, superset, i, j)
	for k = i or 1, j or #subset_list do
		local actual = subset_list[k]
		if is_subset(actual, superset) then
			return true
		end
	end

	return false
end

--#endregion

do -- Type
	---@class type-track.Type : Inheritable
	---@field normalized type-track.Type?
	---@field debug_name string?
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local TypeInst = muun("Type", Inheritable)

	Type = TypeInst --[[@as type-track.Type.Class]]

	---determines whether `subset` is a subset of `superset`, where both types
	---are of the same class
	---
	---Lua's simplest semantic test for `is_subset` is assignment.
	---
	---```lua
	---a: A
	---b: B
	---
	---b = a -- `is_subset(A, B)` must be true to assign `a` to `b`
	---```
	---
	---Generally, based on its element relationship:
	---- Covariance - every element of `subset` must be a subset of its
	---  corresponding element in `superset`
	---- Contravariance - every element in `superset` must be a subset of its
	---  corresponding element in `subset`
	---- Invariance - every element in `subset` must be equal to its
	---  corresponding element in `superset`
	---
	---For each class:
	---- `Operation`s have contravariant domains and covariant ranges
	---- `Tuple`s have covariant elements
	---- `Union`s don't have a notion of variance
	---- `Intersection`s don't have a notion of variance
	---- `Literal`s have invariant values and covariant supporting operators
	---- `Free` types are covariant
	---- `GenericOperation`s' variance are determined by their inference function
	---- `Never` and `Unknown` are not parameterized
	---@param subset type-track.Type
	---@param superset type-track.Type
	---@return boolean
	function Type.is_subset(subset, superset)
		return false
	end

	---attempts to evaluate `op` on this type with `domain`
	---
	---If `nil` is returned, the operation wasn't compatible. Otherwise, a type
	---is returned. A `Tuple` is used for multiple return types.
	---@param op string
	---@param domain type-track.Type
	---@return type-track.Type? range
	function TypeInst:eval(op, domain)
		error("not implemented")
	end

	---returns what parameters this type supports for `op`
	---
	---If `nil` is returned, the operation isn't compatible. Otherwise, a type is
	---returned. A `Tuple` is used for multiple parameter types.
	---@param op string
	---@return type-track.Type? domain
	function TypeInst:get_domain(op)
		return nil
	end

	---returns the `i`th element in this type. If there is no element defined
	---there, it returns `nil`.
	---
	---This implementation is typically moot for types of a single value, like
	---`Operation` or `Literal`, but useful for `Tuple`s or `Tuple`-containing
	---types.
	---@param i integer
	---@return type-track.Type?
	function TypeInst:at(i)
		return i == 1 and self or nil
	end

	---converts a type into its simplest form.
	---
	---- If unification succeeds, it returns a `Type`. It may be itself if it was
	---  already normalized.
	---
	---- Otherwise, it returns `nil`. Typically, this means the type is invalid.
	---
	---Note: If you plan to change the behavior of this method, override
	---`Type:_normalize()`.
	---@param visited? { [type-track.Type]: true? }
	---@return type-track.Type?
	function TypeInst:normalize(visited)
		if self.normalized then
			return self.normalized
		end

		if not visited then
			visited = {}
		elseif visited[self] then
			return nil
		end

		visited[self] = true

		local result = self:_normalize(visited)
		if not result then
			return nil
		end

		self.normalized = result
		result.normalized = result

		return result
	end

	---defines the algorithm for converting a type into its simplest form. This
	---is a protected method that implements `Type:normalize()`. It must always
	---return either a `Type` or `nil`.
	---
	---- If the type is already in its simplest form, it can return itself, but
	---  it's not required.
	---- If unification succeeds, it returns a new `Type`.
	---- Otherwise, it returns `nil`.
	---
	---The public method makes sure to call `_normalize` only when the type hasn't
	---been normalized before and the type wasn't already visited during a parent
	---unification.
	---@param visited { [type-track.Type]: true? }
	---@return type-track.Type? normalized
	function TypeInst:_normalize(visited)
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

	---@param visited { [type-track.Type]: any }
	function TypeInst:debug_subdata(visited)
		local found = visited[self]
		if not found then
			if self.debug_name then
				found = self.debug_name
				visited[self] = found
			else
				found = {}
				visited[self] = found
				self:debug_data(found, visited)
			end
		end
		return found
	end

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function TypeInst:debug_data(ref, visited)
		error("not implemented")
	end

	---every type is serializable. The expectation is, if a type serializes to
	---the same string as another type, they are the same type.
	---@return string
	function TypeInst:__tostring()
		local ok, inspect = pcall(require, "inspect")
		assert(ok, "inspect module is required to call tostring(Type)")

		local data = self:debug_subdata({})
		return inspect(data)
	end

	function Type:__inherited(cls)
		local super_base = self.__base
		local base = cls.__base

		-- set metamethods
		base.__add = super_base.__add
		base.__mul = super_base.__mul
		base.__tostring = super_base.__tostring
	end
end

do -- Operation
	---represents a possible operation on a value, e.g. addition, concatenation,
	---etc.
	---@class type-track.Operation : type-track.Type
	---@field domain type-track.Type
	---@field range type-track.Type
	---@field op string
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local OperationInst = muun("Operation", Type)

	Operation = OperationInst --[[@as type-track.Operation.Class]]

	---@param self type-track.Operation
	---@param op string
	---@param domain type-track.Type
	---@param range type-track.Type
	function Operation:new(op, domain, range)
		self.domain = domain
		self.range = range
		self.op = op

		if domain.__class == Free then
			---@cast domain type-track.Free
			domain.dependencies[self] = true
		end
		if range.__class == Free then
			---@cast range type-track.Free
			range.dependencies[self] = true
		end
	end

	---@param subset type-track.Operation
	---@param superset type-track.Operation
	---@return boolean
	function Operation.is_subset(subset, superset)
		return subset.op == superset.op
			and is_subset(superset.domain, subset.domain)
			and is_subset(subset.range, superset.range)
	end

	---@param op string
	---@param domain type-track.Type
	---@return type-track.Type? range
	function OperationInst:eval(op, domain)
		assert(op ~= nil, "op cannot be nil")
		if op ~= self.op then
			return nil
		end

		if is_subset(domain, self.domain) then
			return self.range
		else
			return nil
		end
	end

	---@param op string
	---@return type-track.Type? domain
	function OperationInst:get_domain(op)
		if op == self.op then
			return self.domain
		else
			return nil
		end
	end

	---@param visited { [type-track.Type]: true? }
	---@return type-track.Type?
	function OperationInst:_normalize(visited)
		local domain = self.domain:normalize(visited)
		if not domain then
			return nil
		end

		local range = self.range:normalize(visited)
		if not range then
			return nil
		end

		return Operation(self.op, domain, range)
	end

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function OperationInst:debug_data(ref, visited)
		ref._type = "Operation"
		ref.op = self.op
		ref.domain = self.domain:debug_subdata(visited)
		ref.range = self.range:debug_subdata(visited)
	end
end

do -- Tuple
	---@class type-track.Tuple : type-track.Type
	---@field elements type-track.Type[]
	---@field var type-track.Type?
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local TupleInst = muun("Tuple", Type)

	Tuple = TupleInst --[[@as type-track.Tuple.Class]]

	---@param self type-track.Tuple
	---@param elements type-track.Type[]
	---@param var? type-track.Type
	function Tuple:new(elements, var)
		self.elements = elements
		self.var = var

		for _, type in ipairs(elements) do
			if type.__class == Free then
				---@cast type type-track.Free
				type.dependencies[self] = true
			end
		end
		if var and var.__class == Free then
			---@cast var type-track.Free
			var.dependencies[self] = true
		end
	end

	---compares two tuples
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
		-- Subsets are always of equal or longer length than supersets.
		-- `var` is not counted because there may be 0 args supplied in that
		-- portion.
		-- (T, T) </: (T, T, T)
		-- (T, T, ...T) </: (T, T, T, ...T)
		-- (T, T, ...T) </: (T, T, T)
		-- (T, T) </: (T, T, T, ...T)
		if #subset.elements < #superset.elements then
			return false
		end

		local sub_var = subset.var or Never
		local super_var = superset.var or Unknown

		-- A </: B => (...A) </: (...B)
		-- A </: B => (B, ...A) </: (...B)
		-- (A, ...T) <: (A)
		if not is_subset(sub_var, super_var) then
			return false
		end

		-- compare element-wise
		-- given A <: B and B </: A
		-- (A, A) <: (B, B)
		-- (A, A, A) <: (B, B)
		-- (A, A, A) <: (B, B, ...B)
		-- (B, B) </: (A, A)
		for i, super_type in ipairs(superset.elements) do
			local sub_type = subset.elements[i]
			if not is_subset(sub_type, super_type) then
				return false
			end
		end

		-- compare vars
		-- given A <: B and B </: A
		-- (A) <: (...B)
		-- (B) </: (...A)
		for i = #superset.elements + 1, #subset.elements do
			local sub_type = subset.elements[i]
			if not is_subset(sub_type, super_var) then
				return false
			end
		end

		return true
	end

	---@param i integer
	---@return type-track.Type?
	function TupleInst:at(i)
		return self.elements[i] or self.var
	end

	---@param op string
	---@param domain type-track.Type
	---@return type-track.Type? range
	function TupleInst:eval(op, domain)
		local first_elem = self:at(1)
		if first_elem then
			return first_elem:eval(op, domain)
		else
			return nil
		end
	end

	---@param op string
	---@return type-track.Type? domain
	function TupleInst:get_domain(op)
		local first_elem = self:at(1)
		if first_elem then
			return first_elem:get_domain(op)
		else
			return nil
		end
	end

	---@param visited { [type-track.Type]: true? }
	---@return type-track.Type?
	function TupleInst:_normalize(visited)
		local arg_count = #self.elements
		local has_args = arg_count > 0
		local self_var = self.var
		local normalized_var = nil ---@type type-track.Type?
		if self_var then
			normalized_var = self_var:normalize(visited)
			if not normalized_var then
				return nil
			end
		else
			-- 0 values is NOT the same as Never
			-- but a unit tuple unifies to `Tuple.Unit`
			if arg_count == 0 and not self_var then
				return Tuple.Unit
			elseif arg_count == 1 then
				return self.elements[1]:normalize(visited)
			end
		end

		local normalized_args = {} ---@type type-track.Type[]
		if has_args then
			for i = 1, arg_count - 1 do
				local elem = self.elements[i]
				local normalized = elem:normalize(visited)
				if not normalized then
					return nil
				end

				normalized_args[i] = normalized:at(1)
			end

			local last_elem = self.elements[arg_count]
			local last_normalized = last_elem:normalize(visited)
			if not last_normalized then
				return nil
			end

			if last_normalized.__class == Tuple then
				---@cast last_normalized type-track.Tuple
				local last_var = last_normalized.var
				if self_var and last_var then
					if not is_subset(last_var, self_var) then
						return nil
					end
				elseif last_var then
					normalized_var = last_var
				end

				for _, sub_elem in ipairs(last_normalized.elements) do
					table.insert(normalized_args, sub_elem)
				end
			else
				table.insert(normalized_args, last_normalized)
			end
		end

		return Tuple(normalized_args, normalized_var)
	end

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function TupleInst:debug_data(ref, visited)
		local elements_data = {}
		for _, t in ipairs(self.elements) do
			table.insert(elements_data, t:debug_subdata(visited))
		end

		ref._type = "Tuple"
		ref.elements = elements_data
		ref.var = self.var and self.var:debug_subdata(visited)
	end

	-- the unit type, `()`
	Tuple.Unit = Tuple({})
	Tuple.Unit.debug_name = "()"
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

		for _, type in ipairs(types) do
			if type.__class == Free then
				---@cast type type-track.Free
				type.dependencies[self] = true
			end
		end
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
	---@param domain type-track.Type
	---@return type-track.Type? range
	function UnionInst:eval(op, domain)
		---@type type-track.Type[]
		local all_ranges = {}

		-- if any element doesn't support it, the entire union doesn't support it
		for _, type in ipairs(self.types) do
			local range = type:eval(op, domain)
			if range then
				table.insert(all_ranges, range)
			else
				return nil
			end
		end

		assert(#all_ranges >= 2, "evaluation on union did not have enough types")
		return Union(all_ranges)
	end

	---@param op string
	---@return type-track.Type?
	function UnionInst:get_domain(op)
		---@type type-track.Type[]
		local all_domains = {}
		for _, type in ipairs(self.types) do
			local domain = type:get_domain(op)
			if domain then
				table.insert(all_domains, domain)
			else
				return nil
			end
		end

		assert(#all_domains >= 2)
		return Intersection(all_domains)
	end

	---@param union type-track.Type[]
	---@param elem type-track.Type
	local function union_insert(union, elem)
		-- if result already contains its superset, don't insert it
		if is_subset_of_any(elem, union) then
			return
		end

		-- otherwise, remove all of its subsets
		for i = #union, 1, -1 do
			local elem2 = union[i]
			if is_subset(elem2, elem) then
				table.remove(union, i)
			end
		end

		-- and insert it into the union
		table.insert(union, elem)
	end

	---@param self type-track.Union
	---@param types type-track.Type[]
	---@param visited { [type-track.Type]: true? }
	---@return type-track.Type?
	local function flatten_union(self, types, visited)
		local result = {} ---@type type-track.Type[]
		for _, elem in ipairs(types) do
			assert(
				elem.__class ~= Free,
				"unbound Free type found while normalizing union"
			)

			if rawequal(elem, self) then
				-- A | A = A, so we can skip itself
				goto continue
			end

			local normalized = elem:normalize(visited)
			if not normalized then
				return nil
			elseif normalized == Unknown then
				return { Unknown }
			elseif normalized.__class == Union then
				---@cast normalized type-track.Union
				for _, sub_elem in ipairs(normalized.types) do
					union_insert(result, sub_elem)
				end
			else
				union_insert(result, normalized)
			end

			::continue::
		end

		return result
	end

	---refines this type to all the elements that are a subtype of `constraint`
	---
	---```lua
	---local fooBar: { type: "foo", foo: "value" } | { type: "bar", bar: "value" }
	---
	---if fooBar.type == "foo" then
	---  -- fooBar undergoes fooBar:refine(
	---  --   Operation("index", Literal("type"), Literal("foo"))
	---  -- )
	---end
	---```
	---@param constraint type-track.Type
	---@return type-track.Type
	function UnionInst:refine(constraint)
		local result = {}
		for _, elem in ipairs(self.types) do
			if is_subset(elem, constraint) then
				table.insert(result, elem)
			end
		end

		if #result == 0 then
			return Never
		elseif #result == 1 then
			return result[1]
		else
			return Union(result)
		end
	end

	---@param visited { [type-track.Type]: true? }
	---@return type-track.Type?
	function UnionInst:_normalize(visited)
		local flattened = flatten_union(self, self.types, visited)
		if not flattened then
			return nil
		end

		assert(#flattened > 0, "no types to normalize?")
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

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function UnionInst:debug_data(ref, visited)
		local types_data = {}
		for _, t in ipairs(self.types) do
			local subdata = t:debug_subdata(visited)
			table.insert(types_data, subdata)
		end

		ref._type = "Union"
		ref.types = types_data
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

		for _, type in ipairs(types) do
			if type.__class == Free then
				---@cast type type-track.Free
				type.dependencies[self] = true
			end
		end
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
	---@param domain type-track.Type
	---@return type-track.Type? range
	function IntersectionInst:eval(op, domain)
		---@type type-track.Type[]
		local all_ranges = {}

		-- it's okay if some calls are not supported here
		for _, type in ipairs(self.types) do
			local range = type:eval(op, domain)
			if range then
				table.insert(all_ranges, range)
			end
		end

		if #all_ranges == 0 then
			return nil
		elseif #all_ranges == 1 then
			return all_ranges[1]
		else
			return Intersection(all_ranges)
		end
	end

	---@param op string
	---@return type-track.Type? domain
	function IntersectionInst:get_domain(op)
		---@type type-track.Type[]
		local all_domains = {}
		for _, type in ipairs(self.types) do
			local domain = type:get_domain(op)
			if domain then
				table.insert(all_domains, domain)
			end
		end

		if #all_domains == 0 then
			return nil
		elseif #all_domains == 1 then
			return all_domains[1]
		else
			return Union(all_domains)
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

	---@param intersection type-track.Type[]
	---@param elem type-track.Type
	local function intersection_insert(intersection, elem)
		if any_are_subset(intersection, elem) then
			return
		end

		for i = #intersection, 1, -1 do
			local elem2 = intersection[i]
			if is_subset(elem, elem2) then
				table.remove(intersection, i)
			end
		end

		table.insert(intersection, elem)
	end

	---@param self type-track.Intersection
	---@param types type-track.Type[]
	---@param visited { [type-track.Type]: true? }
	---@return type-track.Type[]? flattened
	local function flatten_intersection(self, types, visited)
		local result = {} ---@type type-track.Type[]
		for _, elem in ipairs(types) do
			assert(
				elem.__class ~= Free,
				"unbound Free type found while normalizing intersection"
			)

			if rawequal(elem, self) then
				-- A & A = A, so we can skip itself
				goto continue
			end

			local normalized = elem:normalize(visited)
			if not normalized then
				return nil
			elseif normalized == Never then
				return { Never }
			elseif normalized.__class == Intersection then
				---@cast normalized type-track.Intersection
				for _, sub_elem in ipairs(normalized.types) do
					-- `sub_elem` is already normalized here, because that's what
					-- `Intersection:normalize()` does
					intersection_insert(result, sub_elem)
				end
			else
				intersection_insert(result, normalized)
			end
			::continue::
		end

		return result
	end

	-- since unions are more prevalent, intersections should look for unions in
	-- their sub-elements and push them to the top of the type expression
	-- - `A & (B | C)` -> `(A & B) | (A & C)`
	-- - `(A | B) & (C | D)` -> `(A & C) | (A & D) | (B & C) | (B & D)`
	---@param types type-track.Type[]
	---@param visited { [type-track.Type]: true? }
	---@return type-track.Type? distributed
	local function distribute_unions(types, visited)
		local all_unions = {} ---@type type-track.Type[][]
		local has_unions = false
		for i, elem in ipairs(types) do
			assert(elem.__class ~= Free, "Free type found while distributing unions")

			if elem.__class == Union then
				---@cast elem type-track.Union
				table.insert(all_unions, elem.types)
				has_unions = true
			else
				table.insert(all_unions, i) -- insert the index into `all_unions`
			end
		end

		if not has_unions then
			-- `types` is already normalized here by `flatten_intersection`
			return Intersection(types)
		end

		-- turn all indexes into their corresponding values from `types`
		for i, union in ipairs(all_unions) do
			if type(union) == "number" then
				all_unions[i] = { types[i] }
			end
		end

		local result = {} ---@type type-track.Type[][]
		---@param permutation type-track.Type[]
		for permutation in permute(all_unions) do
			table.insert(result, Intersection(permutation))
		end

		return Union(result):normalize(visited)
	end

	---@param visited { [type-track.Type]: true? }
	---@return type-track.Type?
	function IntersectionInst:_normalize(visited)
		local flattened = flatten_intersection(self, self.types, visited)
		if not flattened then
			return nil
		end
		assert(#flattened > 0, "no types to normalize?")
		if #flattened == 1 then
			return flattened[1]
		end

		return distribute_unions(flattened, visited)
	end

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function IntersectionInst:debug_data(ref, visited)
		local types_data = {}
		for _, t in ipairs(self.types) do
			table.insert(types_data, t:debug_subdata(visited))
		end

		ref._type = "Intersection"
		ref.types = types_data
	end
end

do -- Literal
	---@class type-track.Literal : type-track.Type
	---@field value unknown
	---@field of type-track.Type
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local LiteralInst = muun("Literal", Type)

	Literal = LiteralInst --[[@as type-track.Literal.Class]]

	---@param self type-track.Literal
	---@param value unknown
	---@param of? type-track.Type
	function Literal:new(value, of)
		self.value = value
		self.of = of or Unknown

		if of and of.__class == Free then
			---@cast of type-track.Free
			of.dependencies[self] = true
		end
	end

	---@param subset type-track.Literal
	---@param superset type-track.Literal
	---@return boolean
	function Literal.is_subset(subset, superset)
		return subset.value == superset.value and is_subset(subset.of, superset.of)
	end

	---@param op string
	---@param domain type-track.Type
	---@return type-track.Type? range
	function LiteralInst:eval(op, domain)
		return self.of:eval(op, domain)
	end

	---@param op string
	---@return type-track.Type? domain
	function LiteralInst:get_domain(op)
		return self.of:get_domain(op)
	end

	---@param visited? { [type-track.Type]: true? }
	---@return type-track.Type?
	function LiteralInst:_normalize(visited)
		local normalized_of = self.of:normalize(visited)
		if not normalized_of then
			return nil
		end

		return Literal(self.value, normalized_of)
	end

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function LiteralInst:debug_data(ref, visited)
		ref._type = "Literal"
		ref.value = self.value
		ref.of = self.of:debug_subdata(visited)
	end
end

do -- Unknown
	---@class type-track.Unknown.Class
	---@overload fun(): type-track.Unknown
	local UnknownClass = muun("Unknown", Type) --[[@as type-track.Unknown.Class]]

	---@class type-track.Unknown : type-track.Type
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local UnknownInst = UnknownClass --[[@as type-track.Unknown]]

	UnknownInst.debug_name = "unknown"

	---Evaluations are never valid on `Unknown`.
	---@param op string
	---@param domain type-track.Type
	---@return type-track.Type? range
	function UnknownInst:eval(op, domain)
		return nil
	end

	---@param i number
	---@return type-track.Type?
	function UnknownInst:at(i)
		return Unknown
	end

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function UnknownInst:debug_data(ref, visited)
		ref._type = "Unknown"
	end

	Unknown = UnknownClass()
	unknown_var = Tuple({}, Unknown)
end

do -- Never
	---@class type-track.Never.Class
	---@overload fun(): type-track.Never
	local NeverClass = muun("Never", Type)

	---@class type-track.Never : type-track.Type
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local NeverInst = NeverClass --[[@as type-track.Never]]

	NeverInst.debug_name = "never"

	---Evaluations are always valid on `Never` and return itself.
	---@param op string
	---@param domain type-track.Type
	---@return type-track.Type? range
	function NeverInst:eval(op, domain)
		return Never
	end

	---@param i number
	---@return type-track.Type?
	function NeverInst:at(i)
		return nil
	end

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function NeverInst:debug_data(ref, visited)
		ref._type = "Never"
	end

	Never = NeverClass()
end

do -- GenericOperation
	---@class type-track.GenericOperation : type-track.Type
	---@field op string
	---@field derive_fn type-track.GenericOperation.derive_fn
	---@field infer_fn type-track.GenericOperation.infer_fn
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local GenericOperationInst = muun("GenericOperation", Type)

	GenericOperation = GenericOperationInst --[[@as type-track.GenericOperation.Class]]

	---@param self type-track.GenericOperation
	---@param op string
	---@param derive_fn type-track.GenericOperation.derive_fn
	---@param infer_fn type-track.GenericOperation.infer_fn
	function GenericOperation:new(op, derive_fn, infer_fn)
		self.op = op
		self.derive_fn = derive_fn
		self.infer_fn = infer_fn
	end

	---@param subset type-track.GenericOperation
	---@param superset type-track.GenericOperation
	---@return boolean
	function GenericOperation.is_subset(subset, superset)
		local subset_domain, subset_range = subset.derive_fn(unknown_var)
		if not subset_domain or not subset_range then
			return false
		end

		local superset_domain, superset_range = superset.derive_fn(unknown_var)
		if not superset_domain or not superset_range then
			return false
		end

		return subset.op == superset.op
			and is_subset(subset_domain, superset_domain)
			and is_subset(superset_range, subset_range)
	end

	---@param op string
	---@param domain type-track.Type
	---@return type-track.Type? range
	function GenericOperationInst:eval(op, domain)
		if op ~= self.op then
			return nil
		end

		local type_params = self.infer_fn(domain)
		if not type_params then
			return nil
		end

		local self_domain, self_range = self.derive_fn(type_params)
		if self_domain and is_subset(domain, self_domain) then
			return self_range
		else
			return nil
		end
	end

	---@param op string
	---@return type-track.Type? domain
	function GenericOperationInst:get_domain(op)
		if op ~= self.op then
			return nil
		end

		local self_domain = self.derive_fn(unknown_var)
		return self_domain
	end

	---returns a concrete `Operation` that is a subset of `superset` for this
	---type's `op`
	---
	---If `nil` is returned, then `superset` could not be matched for this
	---operator.
	---@param superset type-track.Type
	---@return type-track.Type? concrete
	function GenericOperationInst:match(superset)
		local super_domain = superset:get_domain(self.op)
		if not super_domain then
			return nil
		end

		local super_range = superset:eval(self.op, super_domain)
		local type_params = self.infer_fn(super_domain, super_range)
		if not type_params then
			return nil
		end

		local self_domain, self_range = self.derive_fn(type_params)
		if not self_domain then
			return nil
		end

		return Operation(self.op, self_domain, self_range)
	end

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function GenericOperationInst:debug_data(ref, visited)
		ref._type = "GenericOperation"
		ref.op = self.op
		ref.derive = self.derive_fn
		ref.infer = self.infer_fn
	end
end

do -- Free
	---@class type-track.Free : type-track.Type
	---@field dependencies { [type-track.Type]: true? }
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator add(type-track.Type): type-track.Union
	local FreeInst = muun("Free", Type)

	Free = FreeInst --[[@as type-track.Free.Class]]

	function FreeInst:new()
		self.dependencies = {}
	end

	---@type fun(elem: type-track.Type, free: type-track.Free, replacement: type-track.Type): type-track.Type
	local sub_reify

	---@param elem type-track.Type
	---@param free type-track.Free
	---@param replacement type-track.Type
	---@return type-track.Type
	local function replace_if_eq(elem, free, replacement)
		if elem == free then
			return replacement
		else
			return elem
		end
	end

	---@type { [any]: fun(elem: type-track.Type, free: type-track.Free, replacement: type-track.Type) }
	local reify_class_handlers = {
		---@param elem type-track.Operation
		[Operation] = function(elem, free, replacement)
			elem.domain = replace_if_eq(elem.domain, free, replacement)
			elem.range = replace_if_eq(elem.range, free, replacement)
		end,
		---@param elem type-track.Tuple
		[Tuple] = function(elem, free, replacement)
			local elements = elem.elements
			for i, t in ipairs(elements) do
				elements[i] = replace_if_eq(t, free, replacement)
			end
			if elem.var then
				elem.var = replace_if_eq(elem.var, free, replacement)
			end
		end,
		---@param elem type-track.Union
		[Union] = function(elem, free, replacement)
			local types = elem.types
			for i, t in ipairs(types) do
				types[i] = replace_if_eq(t, free, replacement)
			end
		end,
		---@param elem type-track.Intersection
		[Intersection] = function(elem, free, replacement)
			local types = elem.types
			for i, t in ipairs(types) do
				types[i] = replace_if_eq(t, free, replacement)
			end
		end,
		---@param elem type-track.Literal
		[Literal] = function(elem, free, replacement)
			elem.of = replace_if_eq(elem.of, free, replacement)
		end,

		-- does nothing
		[Free] = function() end,
		[GenericOperation] = function() end,
	}

	---@param elem type-track.Type
	---@param free type-track.Free
	---@param replacement type-track.Type
	---@return type-track.Type
	function sub_reify(elem, free, replacement)
		assert(elem ~= nil, "elem is nil")

		if elem == free then
			return replacement
		elseif elem ~= Unknown and elem ~= Never then
			local handler =
				assert(reify_class_handlers[elem.__class], "unhandled type in redefine")
			if handler then
				handler(elem, free, replacement)
			end
		end

		return elem
	end

	---modifies all types that depend on this instance such that any instance of
	---`self` is replaced with `replacement`.
	---@param replacement type-track.Type
	function FreeInst:reify(replacement)
		for dep in pairs(self.dependencies) do
			sub_reify(dep, self, replacement)
		end
	end

	---@return type-track.Type? range
	function FreeInst:eval()
		error("unsupported method Free:eval()")
	end

	---@return type-track.Type? domain
	function FreeInst:get_domain()
		error("unsupported method Free:get_domain()")
	end

	---@return type-track.Type?
	function FreeInst:at()
		error("unsupported method Free:at()")
	end

	---@return type-track.Type? normalized
	function FreeInst:normalize()
		error("unsupported method Free:normalize()")
	end

	---@param ref table
	---@param visited { [type-track.Type]: any }
	function FreeInst:debug_data(ref, visited)
		local deps_data = {}
		for dep in pairs(self.dependencies) do
			table.insert(deps_data, dep:debug_subdata(visited))
		end

		ref._type = "Free"
		ref.dependencies = deps_data
	end
end

meta.Operation = Operation
meta.Tuple = Tuple
meta.Union = Union
meta.Intersection = Intersection
meta.Literal = Literal
meta.Never = Never
meta.Unknown = Unknown
meta.GenericOperation = GenericOperation
meta.Free = Free

meta.Type = Type

meta.is_subset = is_subset

---@param t unknown
---@return boolean
function meta.is_type(t)
	return Inheritable.is_instance(t, Type)
end

return meta
