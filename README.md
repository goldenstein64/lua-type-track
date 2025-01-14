# lua-type-track

> [!WARNING]
> *This project is de-funct!* Don't expect it to work. This is just
> documentation of work I've tried and failed to contribute with.

I stopped working on this project because operations aren't fundamentally
decomposable into smaller parts without breaking soundness.

```lua
--- this type is not decomposable:
--- (index(number) -> string) & (newindex(number, string) -> ())
```

A Lua library for implementing static analysis of a Lua-like duck type system.

## Tutorial

A duck-typed system represents types as sets of behaviors that a variable
supports.

For example, a function like `incr`, described below:

```lua
-- LuaLS
local incr ---@type fun(x: number): number

-- Teal
local incr: function(x: number): number

-- Luau
local incr: (x: number) -> number
```

supports the behavior of calling it with one number `x` and returning another
number `x + 1`.

The `number` type can be accessed from the `type-track.lua51` module.

```lua
local lua51 = require("type-track.lua51")
local number = lua51.number
```

In `type-track`, the behavior of `incr` being called this way is modeled as an
instance of `Operation`. Operations are a representation of any function call
interface, which includes metamethods like `__add` and `__len`.

```lua
local meta = require("type-track.meta")
local Operation = meta.Operation
```

An operation is composed of three properties:

1. an operator - A string representing what this operation runs over
2. `domain` - the parameter type it accepts
3. `range` - the return type it yields

Thus, the type of `incr` can be represented as,

```lua
local incr_type = Operation('call', number, number)
```

If we know that a variable holds exactly one value, like this variable `x`,

```lua
local x: 15 = 15
```

then it can be represented as an instance of `Literal`. Literals represent
exactly one value of its parent type. They are as unique as their value.

```lua
local Literal = meta.Literal
```

A literal is composed of two properties:

1. `value` - the actual value this literal represents
2. `of` (as in it's a literal of...) - the parent type this value belongs to

This, the type `x` can be represented as:

```lua
local x_type = Literal(15, number)
```

`incr_type` and `x_type` are both instances of `Type`, proven by the below
statements:

```lua
assert(is_type(incr_type))
assert(is_type(x_type))
```

All instances of `Type` provide a suite of methods for introspecting and
comparing it to other types. For example, `Type:eval()` can be used to make use
of an operation and return a result type.

```lua
local is_subset = meta.is_subset

local result = assert(incr_type:eval("call", x_type), "evaluation failed!")
assert(is_subset(result, number))
```

The `is_subset` function is used to compare two types. Most of the time, it's
used to check if one type is *assignable* to another. For example, if a
variable `n` is supposed to hold a number, the literal `15` can be used to
define it because it is a subtype of `number`.

```lua
-- local n: number
local n_type = number

-- n = 15
local n_definition = Literal(15, number)
assert(
  is_subset(n_definition, n_type),
  "This definition is not a subtype of 'number'!"
)
```

If one tried to assign, say, a string to `n`, the `is_subset` constraint would
fail because it's not a subtype of `number`.

The *assignability* constraint is used in more places than assignment. It's
also used to check whether a function can accept a parameter. For example, the
function `incr` cannot accept a variable of type `string`. It should return
`nil` because the evaluation failed.

```lua
local _string = lua51.string

local result2 = incr_type:eval("call", _string)
assert(result2 == nil)
```

Let's say we wanted to write a function that takes two numbers and returns
another number, like `sum`.

```lua
-- LuaLS
local sum ---@type fun(a: number, b: number): number

-- Teal
local sum: function(a: number, b: number): number

-- Luau
local sum: (a: number, b: number) -> number
```

This would require modeling an operation whose `domain` is a pair of two
numbers. This can be achieved using an instance of `Tuple`.

```lua
local Tuple = meta.Tuple

local sum_domain = Tuple({ number, number })
```

Tuples are used to represent an ordered list of types. They can also represent
a variadic number of types using the tuple constructor's second argument.

Tuples are composed of two properties:

- `elements` - an ordered list of types
- `var` - a type representing a variadic number of elements after the ordered
  list.

Therefore, the `sum` function could be defined as this operation.

```lua
local sum_type = Operation("call", Tuple({ number, number }), number)
```

The tuple's `var` property is used to present types that accept a variable
number of parameters. For example, the function `math.min` accepts at least one
number and any number of extra numbers. This can be modeled using the tuple
constructor's second argument.

```lua
local math_min_type = Operation("call", Tuple({ number }, number), number)
```

Sometimes, it may be useful to define a set of possible values a variable can
take. For example, a variable may be a number or `nil`.

```lua
-- LuaLS
local opt ---@type number?

-- Teal
local opt: number

-- Luau
local opt: number?
```

This type can be represented as a union. Unions are a collection of all the
possible types a variable can be.

```lua
local Union = meta.Union
local _nil = lua51["nil"]
```

Unions have only one property, `types`. It is an array of all the types the
union represents. The `opt` variable can be represented as this union:

```lua
local opt_type = Union({ number, _nil })

-- shorthand
local opt_type = number + _nil
```

`number` and `nil` types can be assigned to the `opt` variable, which is proven
by these subset relations:

```lua
-- opt = 23
assert(is_subset(Literal(23, number), opt_type))

-- opt = nil
assert(is_subset(_nil, opt_type))
```

Sometimes, it may be useful to say that a variable supports multiple
operations, such as a table with several fields.

```lua
-- LuaLS
local john ---@type { name: string, age: number }

-- Teal
local john: record
  name: string
  age: number
end

-- Luau
local john: { name: string, age: number }
```

This can be represented as an intersection. Intersections are a collection of
every type the variable represents. `john` can be represented as this
intersection:

```lua
local john_type = Intersection({
  Operation("index", Literal("name", _string), _string),
  Operation("index", Literal("age", _string), number),
})

-- shorthand
local john_type =
  Operation("index", Literal("name", _string), _string)
  * Operation("index", Literal("age", _string), number)
```

`john_type` supports indexing it for a `name` or `age`, yielding the appropriate
value.

```lua
local name_type = john_type:eval("index", Literal("name", _string))
assert(is_subset(name_type, _string))
assert(is_subset(_string, name_type))

local age_type = john_type:eval("index", Literal("age", _string))
assert(is_subset(age_type, number))
assert(is_subset(number, age_type))
```

## Overview

This system implements duck-typing using a set of meta-types:

- `Operation(op, domain, range)` - a mapping from one type to another over an
  operator
- `Tuple({ ...types })` - an ordered list of types, including variadic types
- `Union({ ...types})` - a set of accepted types
- `Intersection({ ...types })` - a set of required types
- `Never` - the bottom type
- `Unknown` - the top type
- `Literal(value, of)` - a type representing exactly one value
- `GenericOperation(op, derive_fn, infer_fn)` - an operation implemented using a pair of functions
- `Free` - a placeholder for implementing cyclic types

All of these meta-types support a common set of methods:

- `is_subset(subtype, supertype)` for subtyping
- `Type:normalize()` for simplifying types
- `Type:eval(op, domain)` for performing operations on a type
- `Type:get_domain(op)` for getting the top domain type that `type:eval`
  accepts.
- `Type:at(i)` for indexing multi-valued types like tuples
- `Type + Type` for creating unions, derived from LPeg
- `Type * Type` for creating intersections, derived from LPeg
- `tostring(type)` for getting a debug representation of the type

Please note that `Free` doesn't support any of these methods; it represents a
variable that gets reified later.

For example, a generic dictionary `{ [K]: V }` can be implemented as a function
`(K: Type, V: Type) -> Type` like so:

```lua
local meta = require("type-track.meta") -- meta-types
local lua51 = require("type-track.lua51") -- Lua 5.1 types

local Tuple = meta.Tuple
local Never = meta.Never
local number = lua51.number
local _string = lua51.string

-- note that this function creates a mutable map
local map_of = lua51.define.map_of -- <K, V> = { [K]: V }
local string_of = lua51.define.string_of -- (actual string) -> Literal(string)

local function map_of(K, V)
  return Operation('type', Never, string_of("table"))
    * Operation('index', K, V)
    * Operation('newindex', Tuple({ K, V }), Never)
end

-- types can be compared using `is_subset`:

-- This example oversimplifies what it means for something to be a function or a
-- string.

-- () -> "foo"
local Foo = Operation('call', Tuple.Unit, string_of("foo"))

-- () -> "foo" & { read [number]: string }
local FooSub = Foo * Operation('index', number, _string)

assert(is_subset(FooSub, Foo))

-- { [string]: () -> "foo" }
local map_foo = map_of(_string, Foo)

-- { [string]: (() -> "foo") & { read [number]: string } }
local map_foo_sub = map_of(_string, FooSub)

-- mutable maps are invariant
assert(not is_subset(map_foo_sub, map_foo))
assert(not is_subset(map_foo, map_foo_sub))
```

There are a few more methods for select types:

- `Union:refine(constraint)` generates a new union that is a subtype of
  `constraint`
  - this will likely be removed if `is_overlapping`/`is_disjoint` and
    Complement types are implemented.
- `Free:reify(replacement)` dereferences `Free` types

This system is designed for static analysis; runtime type-checking would require
constructing the type to conform to, converting a value to its type
representation, and comparing the types using `is_subset`; all of these
operations are likely very slow, especially for large types.

This system is not a linter; it can tell you if a call is not supported -- and
why that is so in the future -- but it will generally be more permissive about
situations that technically don't cause type mismatches due to things like Lua's
calling and variable binding semantics.

## Gotchas

- `any` is not a type in this system because `any` *ignores* types. Use a
  sentinel value in place of a type object when you need `any`. You can wrap `is_subset` so that it returns early when finding `any`.

- A function that never returns (i.e. infinite loops and guaranteed errors) has
  a return type of `Never`. A function that returns zero values (e.g. `print`)
  has a return type of `Tuple.Unit` a.k.a. `Tuple({})`.

- Operations allow a greater number of arguments than the number of defined
  parameters. It's up to implementors to write a lint that warns against
  this. e.g.

```lua
local A = Literal("A")
local B = Literal("B")
local op = Operation("call", A, B) -- ("A") -> "B"

local args = Tuple({ A, Unknown })

-- op:eval "call" accepts ("A", unknown)
print(op:eval("call", args)) --> { _type = "Literal", value = "B", of = "unknown" }

---@param args type-track.Type
---@return boolean passed, string? err_msg
local function possible_lint(args)
  local len = #args.types

  local domain = op:get_domain("call")
  local last_elem = domain and domain:at(len)
  if not last_elem then
    return false, "some of these arguments are unused"
  end

  return true
end
```
