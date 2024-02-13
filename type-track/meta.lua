---a collection of classes describing how all types are constructed

local muun = require("type-track.muun")

---@class Inheritable
---@field __class any
local Inheritable = muun("Inheritable")

---@param cls any
---@return boolean
function Inheritable:is_instance(cls)
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
---@class type-track.Callable.Class
---@overload fun(params: type-track.Type, returns: type-track.Type): type-track.Callable
local Callable

---describes how operations work
---@class type-track.Object.Class
---@field __base any
---@overload fun(ops?: type-track.Object.ops, datatype?: string): type-track.Object
local Object

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
---@field __super type-track.Object | fun(self: type-track.Object, ops?: type-track.Object.ops, datatype?: string): type-track.Object
---@overload fun(value: unknown, ops?: type-track.Object.ops, datatype?: string): type-track.Literal
local Literal

---@class type-track.Type.Class
---@field __base any
---@overload fun(): type-track.Type
local Type

---@type fun(actual: type-track.Type, list: type-track.Type[]): boolean
local any_types

---@type fun(actual: type-track.Type, list: type-track.Type[]): boolean
local all_types

---determines whether `subtype` is a subset of `supertype`. This operation is
---available across all
---@param subtype type-track.Type
---@param supertype type-track.Type
---@return boolean
local function is_subset(subtype, supertype)
	-- Tuple, Union, Intersection, Object, Callable, Literal
	-- 36 combinations
	-- sub-super
	-- TT TT TT TT TT TT
	-- UT UU UI UO UC UL
	-- IT IU II IO IC IL
	-- OT OU OI OO OC OL
	-- CT CU CI CO CC CL
	-- LT LU LI LO LC LL

	-- If both types have the same class, their respective compare method is
	-- used. Otherwise, coerce the types to something else and compare that?
	local sub_cls = subtype.__class
	local super_cls = supertype.__class
	if sub_cls == super_cls then
		return sub_cls.is_subset(subtype, supertype)
	elseif super_cls == Tuple then
		---@cast supertype type-track.Tuple
		if #supertype.types == 0 then
			return not supertype.var_arg or is_subset(subtype, supertype.var_arg)
		elseif #supertype.types == 1 then
			return is_subset(subtype, supertype.types[1])
		elseif Tuple.default_var_arg then
			for i = 2, #supertype.types do
				local super_elem = supertype.types[i]
				if not is_subset(Tuple.default_var_arg, super_elem) then
					return false
				end
			end

			return is_subset(subtype, supertype.types[1])
		end
	elseif super_cls == Union then
		---@cast supertype type-track.Union
		return any_types(subtype, supertype.types)
	elseif super_cls == Intersection then
		---@cast supertype type-track.Intersection
		return all_types(subtype, supertype.types)
	elseif sub_cls == Tuple then
		---@cast subtype type-track.Tuple
		local first_elem = subtype:at(1)
		if first_elem then
			return is_subset(first_elem, supertype)
		end
	elseif sub_cls == Union then
		---@cast subtype type-track.Union
		for _, t in ipairs(subtype.types) do
			if not is_subset(t, supertype) then
				return false
			end
		end

		return true
	elseif sub_cls == Intersection then
		---@cast subtype type-track.Intersection
		for _, t in ipairs(subtype.types) do
			if is_subset(t, supertype) then
				return true
			end
		end
	elseif sub_cls == Literal then
		---@cast subtype type-track.Literal
		if super_cls == Object then
			---@cast supertype type-track.Object
			return Object.is_subset(subtype, supertype)
		elseif super_cls == Callable then
			---@cast supertype type-track.Callable
			local call_impl = subtype.ops.call
			if call_impl then
				return is_subset(call_impl, supertype)
			end
		end
	elseif sub_cls == Object then
		---@cast subtype type-track.Object
		if super_cls == Callable then
			---@cast supertype type-track.Callable
			local call_impl = subtype.ops.call
			if call_impl then
				return is_subset(call_impl, supertype)
			end
		end
	elseif sub_cls == Callable then
		---@cast subtype type-track.Callable
		if super_cls == Object then
			---@cast supertype type-track.Object

			-- a Callable is essentially an object with only a call operation
			-- so if any of the supertype's operations aren't calls, it can't be a
			-- subset
			for op in pairs(supertype.ops) do
				if op ~= "call" then
					return false
				end
			end

			local call_impl = supertype.ops.call
			if call_impl then
				return is_subset(subtype, call_impl)
			else
				-- if the supertype doesn't have a call op, it's just a symbol, but
				-- Callables can be treated as symbols too.
				return true
			end
		end
	end

	return false
end

---accepts a type `actual` if it is a subset of all types in `list`
---@param actual type-track.Type
---@param list type-track.Type[]
---@return boolean
function all_types(actual, list)
	for _, expected in ipairs(list) do
		if not is_subset(actual, expected) then
			return false
		end
	end

	return true
end

---accepts a type `actual` if it is a subset of any type in `list`
---@param actual type-track.Type
---@param list type-track.Type[]
---@return boolean
function any_types(actual, list)
	for _, expected in ipairs(list) do
		if is_subset(actual, expected) then
			return true
		end
	end

	return false
end

do -- Type
	---@class type-track.Type : Inheritable
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator div(type-track.Type): type-track.Callable
	---@operator add(type-track.Type): type-track.Union
	local TypeInst = muun("Type", Inheritable)

	Type = TypeInst --[[@as type-track.Type.Class]]

	function Type.is_subset(subset, superset)
		return false
	end

	---attempts to call this type
	---
	---If `params` is a `Type`, it should be type-checked against. If it's `nil`,
	---a generic return value should be given.
	---
	---If `nil` is returned, the call wasn't compatible. Otherwise, a return type
	---is expected. Use a `Tuple` for multiple parameters or return types.
	---@param params type-track.Type?
	---@return type-track.Type? returns
	function TypeInst:call(params)
		error(":call() is not implemented")
	end

	---returns the `i`th element in this type
	---
	---This implementation is typically moot for types of a single value, like
	---`Callable` or `Object`, but useful for `Tuple`s or `Tuple`-containing
	---types.
	---@param i integer
	---@return type-track.Type?
	function TypeInst:at(i)
		return i == 1 and self or nil
	end

	---returns a union of its operands. This will concatenate unions when
	---possible.
	---@param other type-track.Type
	---@return type-track.Union
	function TypeInst:__add(other)
		local values = {}
		if self:is_instance(Union) then
			---@cast self type-track.Union
			for _, type in ipairs(self.types) do
				table.insert(values, type)
			end
		else
			table.insert(values, self)
		end

		if other:is_instance(Union) then
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
		if self:is_instance(Intersection) then
			---@cast self type-track.Intersection
			for _, type in ipairs(self.types) do
				table.insert(values, type)
			end
		else
			table.insert(values, self)
		end

		if other:is_instance(Intersection) then
			---@cast other type-track.Intersection
			for _, type in ipairs(other.types) do
				table.insert(values, type)
			end
		else
			table.insert(values, other)
		end

		return Intersection(values)
	end

	---returns a callable `(self) -> (other)`. An `__rsh` overload would look
	---best, but LuaJIT doesn't support it.
	---@param other type-track.Type
	---@return type-track.Callable
	function TypeInst:__div(other)
		return Callable(self, other)
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
	---@operator div(type-track.Type): type-track.Callable
	---@operator add(type-track.Type): type-track.Union
	local TupleInst = muun("Tuple", Type)

	Tuple = TupleInst --[[@as type-track.Tuple.Class]]

	---@param self type-track.Tuple
	---@param types type-track.Type[]
	---@param var_arg? type-track.Type
	function Tuple:new(types, var_arg)
		self.types = types
		self.var_arg = var_arg
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
		-- compare element-wise
		for i, super_type in ipairs(superset.types) do
			local sub_type = subset:at(i)
			if not sub_type or not is_subset(sub_type, super_type) then
				return false
			end
		end

		-- compare var-arg
		local super_var_arg = superset.var_arg
		if super_var_arg then
			local sub_var_arg = subset.var_arg
			if not sub_var_arg or not is_subset(sub_var_arg, super_var_arg) then
				return false
			end

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

	---@param params? type-track.Type
	---@return type-track.Type? returns
	function TupleInst:call(params)
		return self:at(1):call(params)
	end
end

do -- Callable
	---represents a value that can be called
	---@class type-track.Callable : type-track.Type
	---@field params type-track.Type
	---@field returns type-track.Type
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator div(type-track.Type): type-track.Callable
	---@operator add(type-track.Type): type-track.Union
	local CallableInst = muun("Callable", Type)

	Callable = CallableInst --[[@as type-track.Callable.Class]]

	---@param self type-track.Callable
	---@param params type-track.Type
	---@param returns type-track.Type
	function Callable:new(params, returns)
		self.params = params
		self.returns = returns
	end

	---@param subset type-track.Callable
	---@param superset type-track.Callable
	---@return boolean
	function Callable.is_subset(subset, superset)
		return is_subset(subset.params, superset.params)
			and is_subset(superset.returns, subset.returns)
	end

	---@param params? type-track.Type
	---@return type-track.Type? returns
	function CallableInst:call(params)
		if not params then return self.returns end

		if not params:is_instance(Tuple) then
			params = Tuple({params})
		end

		if is_subset(params, self.params) then
			return self.returns
		else
			return nil
		end
	end
end

do -- Object
	---represents a value that supports zero or more operations (including calls)
	---@class type-track.Object : type-track.Type
	---@field ops type-track.Object.ops
	---@field datatype? string
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator div(type-track.Type): type-track.Callable
	---@operator add(type-track.Type): type-track.Union
	local ObjectInst = muun("Object", Type)

	Object = ObjectInst --[[@as type-track.Object.Class]]

	---represents every operation permitted on a type.
	---@alias type-track.op
	---| "'add'" # `+`
	---| "'sub'" # `-`
	---| "'mul'" # `*`
	---| "'div'" # `/`
	---| "'idiv'" # `//`
	---| "'mod'" # `%`
	---| "'pow'" # `^`
	---| "'unm'" # `-` (unary)
	---| "'shl'" # `<<`
	---| "'shr'" # `>>`
	---| "'band'" # `&`
	---| "'bor'" # `|`
	---| "'bxor'" # `~` (binary)
	---| "'bnot'" # `~` (unary)
	---| "'concat'" # `..`
	---| "'len'" # `#`
	---| "'eq'" # `==`
	---| "'lt'" # `<`
	---| "'le'" # `<=`
	---| "'index'" # `. or []`
	---| "'newindex'" # `.= or []=`
	---| "'call'" # `()`

	---@class type-track.Object.ops
	---@field add? type-track.Type
	---@field sub? type-track.Type
	---@field mul? type-track.Type
	---@field div? type-track.Type
	---@field idiv? type-track.Type
	---@field mod? type-track.Type
	---@field pow? type-track.Type
	---@field unm? type-track.Type
	---@field shl? type-track.Type
	---@field shr? type-track.Type
	---@field band? type-track.Type
	---@field bor? type-track.Type
	---@field bxor? type-track.Type
	---@field bnot? type-track.Type
	---@field concat? type-track.Type
	---@field len? type-track.Type
	---@field eq? type-track.Type
	---@field lt? type-track.Type
	---@field le? type-track.Type
	---@field index? type-track.Type
	---@field newindex? type-track.Type
	---@field call? type-track.Type

	---@param self type-track.Object
	---@param ops? type-track.Object.ops
	---@param datatype? string
	function Object:new(ops, datatype)
		self.ops = ops or {}
		self.datatype = datatype
	end

	---@param subset type-track.Object
	---@param superset type-track.Object
	---@return boolean
	function Object.is_subset(subset, superset)
		if subset.datatype ~= superset.datatype then return false end

		for op, super_impl in pairs(superset.ops) do
			local impl = subset.ops[op]
			if not impl or not impl:is_subset(super_impl) then
				return false
			end
		end

		return true
	end

	---@param params? type-track.Type
	---@param op? type-track.op
	---@return type-track.Type? returns
	function ObjectInst:call(params, op)
		op = op or "call"
		local call_interface = self.ops[op] --[[@as type-track.Type?]]
		return call_interface and call_interface:call(params)
	end
end

do -- Union
	---@class type-track.Union : type-track.Type
	---@field types type-track.Type[]
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator div(type-track.Type): type-track.Callable
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

	---@param subset type-track.Union
	---@param superset type-track.Union
	---@return boolean
	function Union.is_subset(subset, superset)
		for _, subtype in ipairs(subset.types) do
			if not any_types(subtype, superset.types) then
				return false
			end
		end

		return true
	end

	---@param params type-track.Type
	---@return type-track.Type? returns
	function UnionInst:call(params)
		---@type type-track.Type[]
		local all_returns = {}

		-- if any call is unsupported, the entire union doesn't support it
		for _, type in ipairs(self.types) do
			local returns = type:call(params)
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
	---@param default type-track.Type
	---@return type-track.Type?
	function UnionInst:at(i, default)
		local all_indexes = {} ---@type type-track.Type[]

		for j, type in ipairs(self.types) do
			all_indexes[j] = type:at(i) or default
		end

		return Union(all_indexes)
	end
end

do -- Intersection
	---@class type-track.Intersection : type-track.Type
	---@field types type-track.Type[]
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator div(type-track.Type): type-track.Callable
	---@operator add(type-track.Type): type-track.Union
	local IntersectionInst = muun("Intersection", Type)

	---@class type-track.Intersection.Class
	Intersection = IntersectionInst --[[@as type-track.Intersection.Class]]

	---@param self type-track.Union
	---@param types type-track.Type[]
	function Intersection:new(types)
		assert(#types >= 2, "intersections must have at least two items")
		self.types = types
	end

	--[[
	A: "A"
	B: "B"
	C: "C"
	AB: A & B
	ABC: A & B & C

	-- superset = subset
	AB = ABC -- good, compare(ABC, AB) passes
	ABC = AB -- bad, compare(AB, ABC) fails
	]]
	---@param subset type-track.Intersection
	---@param superset type-track.Intersection
	---@return boolean
	function Intersection.is_subset(subset, superset)
		for _, supertype in ipairs(superset.types) do
			if not any_types(supertype, subset.types) then
				return false
			end
		end

		return true
	end

	function IntersectionInst:call(params)
		error("not implemented")
	end

	function IntersectionInst:at(i)
		error("not implemented")
	end
end

do -- Literal
	---@class type-track.Literal : type-track.Object
	---@field value unknown
	---@operator mul(type-track.Type): type-track.Intersection
	---@operator div(type-track.Type): type-track.Callable
	---@operator add(type-track.Type): type-track.Union
	local LiteralInst = muun("Literal", Object)

	Literal = LiteralInst --[[@as type-track.Literal.Class]]

	---@param self type-track.Literal
	---@param value unknown
	---@param ops? type-track.Object.ops
	---@param datatype? string
	function Literal:new(value, ops, datatype)
		Literal.__super(self, ops, datatype)
		self.value = value
	end

	---@param subset type-track.Literal
	---@param superset type-track.Literal
	---@return boolean
	function Literal.is_subset(subset, superset)
		return subset.value == superset.value
	end
end

return {
	Tuple = Tuple,
	Callable = Callable,
	Object = Object,
	Union = Union,
	Intersection = Intersection,
	Literal = Literal,

	Type = Type,

	is_subset = is_subset,
}
