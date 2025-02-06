# lua-type-track

> [!WARNING]
>
> _This project is de-funct!_ Don't expect it to work. This is just
> documentation of work I've tried and failed to contribute with.

I stopped working on this project because operations aren't fundamentally
decomposable into smaller parts without breaking soundness.

```lua
--- this type is not decomposable:
--- (index(number) -> string) & (newindex(number, string) -> ())
```

This is bad because every type in Lua is fundamentally cyclic, via the 
`select` operation. Every type is able to compute `select(1, ... Type)`, which 
means the `is_subset` predicate would have to be able to handle cyclic cases in 
a sound and computable way, which is a lot of work the way this codebase is 
written (at least if I were to add a `visited` argument to every function), and 
may not even be possible.

This is a Lua library for implementing static analysis of a Lua-like duck type
system.

The `negation-types` branch is my attempt to write the `is_disjoint` predicate,
which would be the basis for complement types, used for type refinement.
Because of the fundamental undecomposable operation problem, I couldn't get
very far.

## Installing

These files aren't published anywhere, so just clone the repo and use LuaRocks
to install dependencies:

```sh
$ git clone https://github.com/goldenstein64/lua-type-track
$ luarocks init
$ luarocks install --deps-only ./type-track-dev-1.rockspec

# for testing,
$ luarocks test
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
- `GenericOperation(op, derive_fn, infer_fn)` - an operation implemented using
  a pair of functions
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

- `any` is not a type in this system because `any` _ignores_ types. Use a
  sentinel value in place of a type object when you need `any`. You can wrap
  `is_subset` so that it returns early when finding `any`.

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
print(op:eval("call", args))
--> { _type = "Literal", value = "B", of = "unknown" }

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
