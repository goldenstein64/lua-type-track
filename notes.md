# type-track

- Types are represented as a set of possible concrete values
- A variable contains two types. Both types are either directly inherited from
  its type annotation or inferred from the AST.
   - A reference type: the most specific type a variable can be assigned to.
   - A definition type: the most general type that can be assigned to that
     variable.
- Whenever a variable A gets assigned to a variable B, we check to see if A's
  reference type is a subset of B's definition type. We are asserting that
  reference A is assignable to definition B.

```lua
-- when given the following code,
local file = {
  name = "tabloid",
  can_execute = false,
  bytes = 11,
}

-- it gives this reference type.
-- note how it matches the value definition almost identically
file: {
  name: "tabloid",
  can_execute: false,
  bytes: 11
}

-- and this definition type.
-- note how the types are more general
file: {
  name: string,
  can_execute: boolean,
  bytes: number,
}
```

This could also be more generally called a read type and write type for
reference and definition types respectively.

Essentially, types can be constructed in two primitive ways:

- A callable type, which at its most basic usage is a function that can be
called with certain arguments, each of which get a variable number of parameter
types and a variable number of return types.

- An object type, which defines what operations are valid on a type as callable
  interfaces.
  - Please note that boolean operations `and`, `or`, and `not` are not
    considered traditional operations, as they only operate on the truthiness of
    a value, which is only controlled by whether their value is equal to `false`
    or `nil`.

Each of the primitive types can be represented as just objects. The `function`
type *could* be represented as a callable of type `(...any) -> (...any)`

How would we check whether a callable `A` is a subset of a callable `B`? Well,
let's imagine them as variables.

```lua
a: (number, number) -> (number)
local a = function(x, y) return x + y end

b: (string, string) -> (string)
local b = function(x, y) return x .. y end

c: (number | string, number | string) -> (number | string)
local c = function(x, y)
  if math.random() < 0.5 then
    return (tonumber(x) or 0) + (tonumber(y) or 0)
  else
    return tostring(x) .. tostring(y)
  end
end

-- is_subset(a, c) => is_subset(a.params, c.params) and is_subset(c.returns, a.returns)
c = a -- warning: a's parameters are too specific

-- is_subset(c, a) => is_subset(a.returns, c.returns)
a = c -- warning: c's returns are too general

a_or_b: ((number, number) -> number) | ((string, string) -> string)
local a_or_b = function(x, y) return x / y end

a_or_b = a -- valid
a_or_b = b -- valid
a = a_or_b -- warning: all of a_or_b's definitions don't satisfy a's type

-- you can't actually use it though, because strings and numbers have an empty
-- subset. (Unless the string can be coerced into a number, then it's a different
-- story.)
a_or_b(3, 4) -- warning: parameters don't satisfy all possible definitions

a_and_b: ((number, number) -> number) & ((string, string) -> string)
local a_and_b = function(x, y)
  if type(x) == "number" and type(y) == "number" then
    return x + y
  elseif type(x) == "string" and type(y) == "string" then
    return x .. y
  end
end

a_and_b = a -- warning: a's type doesn't satisfy all of a_and_b's definitions
```

There are three things this type system has to support:
- Set relationships, `:is_subset`
- Object operator semantics, `:evaluate(op, params)`
- Callable semantics, `:call(params)`

Call it what it is: type validation

So use a type validation library (tableshape / t)

How can we use type validation to validate assignment?

```lua
local file = {
  name = "tabloid",
  can_execute = false,
  bytes = 11,
}

-- reference type
-- note how it's equal to the literal value
local file = shape {
  name = "tabloid",
  can_execute = false,
  bytes = 11,
}

-- definition type
local file = shape {
  name = string,
  can_execute = boolean
  bytes = number
}
```

The difficulty here is comparing types to types...
1. Create a way to generate a dummy type from a type; it should always validate
   against itself.
  * This seems to be error-prone, and might not be feasible for some types (e.g.
    `types.pattern`)
2. Create a way to generate a type-type from a type, i.e. a type that validates
   other types
  * This could be inferred from the type's class, most likely

#1 seems like the simplest and most straight-forward.

There's no way to validate a function directly, but we can still represent them using a shape.

```lua
---@param path string
---@param args string[]
---@return integer code
local function run(path, args)
  return os.execute(path, args)
end

-- reference and definition type
local run = shape {
  params = shape { string:describe("path"), array_of(string):describe("args") },
  returns = shape { integer:describe("code") },
}
```

- Tuple
- Callable
- Object
- Literal
- Intersection
- Union

## UPDATE

I've realized that `Objects`, `Callables` and `Intersections` share a similar
design space, e.g.

```lua
type A = {
  index: ("x") -> (number) & ("y") -> (number)
}

type B = { index: ("x") -> (number) } & { index: ("y") -> (number) }
```

Both are valid ways to represent the same type, but apparently it can't be
proved that `A <: B` because `is_subset(A, B[i])` is not true for any *one*
element `B[i]`, even though this is the case when all of `B` is considered.

The first approach to fixing this was creating an algorithm that converted an
intersection into an object, but this can become very expensive when unions are
involved.

This has been fixed by removing the `Object` meta-type and instead incorporating
operation semantics into the `Callable`. As a result, `Type:call(params)` has
been replaced with `Type:eval(op, params)`, `Callable` renamed to `Operator`,
and its constructor takes an `op` string as its first argument.

Now, `B` is the only valid way to represent the type, which means no more issues
with `is_subset` being faulty.

The biggest caveat to this method is that evaluating intersections is slower and
often involves unnecessary checks. For example, if I wanted to evaluate `B.x`, I
would typically have to iterate through all operators in the intersection, even
the ones after `{ index: x... }`, to get an intersection of all possible
returns. It would likely be more performant to use e.g. a tree of hash-maps,
with the root being a map of operations to operator intersections, and further
trees mapping the nth argument to a result type.

Operators represent mappings from the `params` type to the `returns` type. What
would "disjoint" mean for a mapping? Is there a Lua construct that describes a
disjoint test? For `is_subset`, it's an assignment. For `is_disjoint`, it's
casting.

```lua
a: A
b: B
local a = b -- `is_subset(B, A)` must be true, i.e. B is always A
local a = b :: C -- `is_disjoint(C, A)` must be false, i.e. C is sometimes A
```

`is_disjoint(U, V)` implies U is never V and vice-versa. My biggest gripe with
this concept is I'm always stuck in the mindset that "disjoint" means there is
never a value that embodies both types, but intersections (that would ideally
normalize to Never) between the types can always be constructed, trivially
disproving the idea.

`"A" <:> "B"`
`"A" | "B" <:> "C" | "D"`
`"A" & "B" <:> "C" & "D"`

It doesn't make sense to have a disjoint/overlapping test for a duck-typed
system... a disjoint between `A` and `B` is defined as `A & B == never`, but
there is no way to evaluate an intersection to `never` this way. That would
require checking what values inhabit the set, which is quite difficult.

And yet it makes sense to describe a negation of behaviors. You just negate the
subset relationship. Except that's not really true, a negation of a subset
relationship still allows for overlap. So in reality, a subset relationship to a
negated behavior is the same as a disjoint relationship to said behavior, not
negated. And a disjoint/overlapping relationship is very difficult to describe
in a duck-typed system.

What if types were constructed from its `type` operator? And that governed
whether a value for the type can be generated. That would mean `string & number`
could be checked for inhabited values by gathering all constructors for the
intersection and matching each, i.e. nominal subtyping

```lua
local _string = Free()

local string_type = Operator("type", Never, string_of("string"))
local concat_call = Operator("concat", number + _string, _string)
local string_lib = lib({ --[[...]] })

_string.value = string_type * concat_call * string_lib
_string.normalized = _string.value
_string.value.normalized = _string.value

local function is_string(set)
  if not set:is_subset(_string) then
  return false
  end
end
```

`is_string` only checks for inclusion of these properties, but not for exclusion
of other properties. How would I check if it has an exact list of properties?

`_string:is_subset(set)` might be the answer? Doing both `:is_subset` checks
would imply equality. Do I want that?

Using equality wouldn't work for literals, tables, functions, many many
things...

---

As an example, intersections of unions will be distributed such that the top
type is a union, resulting in an explosion of union elements.

```lua
type Some = (A | B | C) & (D | E | F) & (G | H | I)

-- this gets expanded to:

type Some =
  | (A & D & G) | (A & D & H) | (A & D & I)
  | (A & E & G) | (A & E & H) | (A & E & I)
  | (A & F & G) | (A & F & H) | (A & F & I)

  | (B & D & G) | (B & D & H) | (B & D & I)
  | (B & E & G) | (B & E & H) | (B & E & I)
  | (B & F & G) | (B & F & H) | (B & F & I)

  | (C & D & G) | (C & D & H) | (C & D & I)
  | (C & E & G) | (C & E & H) | (C & E & I)
  | (C & F & G) | (C & F & H) | (C & F & I)
```

The number of times this would happen in production is unclear. The most common
usage of an intersection is to form objects/structures:

```lua
type Some = {
  foo: string,
  bar: string
}

-- represented as:

type Some =
  Operation("type", Never, Literal("table", _string))
  * Operation("index", Literal("foo", _string), _string)
  * Operation("newindex", Tuple({ Literal("foo", _string), _string }), Never)
  * Operation("index", Literal("bar", _string), _string)
  * Operation("newindex", Tuple({ Literal("bar", _string), _string }), Never)
```

If unions were introduced among operations, this would cause an explosion, but
operation domains and ranges are not reducible and unions of operations across
the same operator are locally reducible (although not implemented).

---

We could take a different perspective to `is_disjoint`, defining it as: "two
types are disjoint if they can't be substituted for each other."

- That would actually be equivalent to `t1 </: t2 and t2 </: t1`
- This would be different from the definition of `t1 & t2 == Never`
- It would also be wrong :|

A third perspective for `is_disjoint` is to describe whether two types will
never be equal to each other.

- A literal `"foo"` will never be equal to a literal `"bar"`, so `"foo"` and
  `"bar"` are disjoint.
- An intersection containing `"foo"` will never equal an intersection containing
  `"bar"`, so those two intersections are disjoint.
- A union containing `"foo"` will never be equal to a union that does *not*
  contain `"foo"`.

Operations are difficult to reason about in terms of disjointness because
they don't describe a value, but rather a value's capability.

Use cases for disjointness in an operation:

1. Types that support the `type` operation can compare their ranges to see if
   they are disjoint.

```lua
is_overlapping(set1, set2) == is_overlapping(set1:eval("type", unit), set2:eval("type", unit))

-- and thus,
assert(not is_overlapping(_string, number))
```

   For types with identical domains, this is great! However, I am not
   sure what the logic should be for different domains. My past experience with
   unions and intersections tells me that I should use the counterpart on the
   domain, i.e. `is_subset(set1, set2) and is_subset(set2, set1)` or its
   converse. What other set relations can there be? If you negate one side of an
   `is_subset` relation, you're then comparing the two using `is_overlapping`.
   I guess checking if two sets are equal (which is double `is_subset`) would be
   as close to an inverse relation as I can get.

   There must be some greater truth I could be using to arrive at this
   conclusion though... otherwise, I might be wrong and something else will be
   unsound along the way because of that, like what happened with tuples.

| overlapping?   | domain equal | domain relative | domain overlaps | domain disjoint |
|----------------|--------------|-----------------|-----------------|-----------------|
| range equal    | yes          | yes             | yes             | no              |
| range relative | yes          | yes             | yes             | no              |
| range overlaps | yes          | yes             | yes             | no              |
| range disjoint | no           | no              | no              | no              |

conclusion: `is_overlapping(set1.domain, set2.domain) and is_overlapping(set1.range, set2.range)`

Case study 1: same interface, different domain

```lua
type Message = "app" | "human"

type Connection = {
  print: (Connection, Message, ...any) -> (),
  prompt: (Connection, Message, ...any) -> string,
}

type ConsoleConnection = Connection & {
  map: { [Message]: (...any) -> string },
  format: (ConsoleConnection, Message, ...any) -> string,
}

type MockConnection = Connection & {
  inputs: { string },
  outputs: { Message },
}

local function newConsole(map: { [Message]: (...any) -> string }): ConsoleConnection
  -- ...
  return aConsoleConnection
end

local function newMock(input: string): MockConnection
  -- ...
  return aMockConnection
end

local arg: { [Message]: (...any) -> string } & string = {}
-- arg would have to simplify to never here because of `type` mismatch

local conn: Connection
local arg: never
local newConn: typeof(newConsole) | typeof(newMock)

conn = newConn(arg)

-- both their domain and range overlap and they have the same operator, so there
-- is no definition of "disjoint" that these two functions can cover.
assert(is_overlapping(newConsole, newMock))

-- in which case, tables that *must* be empty have a type compatible to any
-- other table. In a way, an empty table doesn't support any indexing
-- operations, just `type` and `len`

local emptyTable = Intersection({
  Operation("type", Tuple.Unit, string_of("table")),
  Operation("len", Tuple.Unit, number),
})
```

For intersections, everything in the former must overlap with everything in the
latter. If one pair is found that does not follow this, it can be proven that
the intersections do not overlap.
- Although this makes sense, it doesn't make sense for e.g. intersections that
  compose tables. If they have even two operations, the entire thing falls
  apart! Even comparing two things of the same type to each other would be
  disasterous.

For unions, anything in the former must overlap with anything in the latter.
If at least one pair is found that allows this, it can be proven that the unions
overlap.

---

It's possible to assign more variables than an expression yields, and the rest
of the variables are assigned to `nil`. This is more of an implementation
detail, users would have to catch this case and assign `nil` themselves.

---

```lua
-- given two function types:
local f: (A | B) -> (X | Y)
local g: (B | C) -> (Y | Z)

-- there exists an overlap
local h: (A | B | C) -> (Y)

-- such that the former 2 can be assigned and equal
f = h
g = h
assert(f == g)
```

Therefore, operations over the same operator can overlap if just their ranges
overlap, because unions covering both domains will always exist, which isn't
always true for intersections over ranges. This means `type` operations can
continue using `Never` as its domain.

Edit: Even if unions covering both domains always exist, the fact that there is
an intersection between the domains implies there must be an intersection
between the ranges.

```lua
local f: (A | B) -> (U)
local g: (B | C) -> (V)

local h:
  & (A) -> (U)
  & (B) -> (U & V) -- implies U & V must exist
  & (C) -> (V)

-- if all that holds true, then this is allowed
f = h
g = h

assert(f == g)
```

Therefore, one of the following conditions must be met for two operations to
overlap:

1. The operators are different, so the intersection can support both operations
2. The range is overlapping, so the intersection of domains evaluates to the
   intersection of ranges
3. Both the domain and range are disjoint, so the intersection can support both
   operations

More generally, two types have a provable disjoint if and only if the only type
assignable to both of the former is `Never`.

For tuples, it seems logical to try treating it as element-wise equality, i.e.

```lua
local t1: (A, B)
local t2: (C, D)

assert(t1 == t2)

-- same as
assert(A == C and B == D)
```

In that case, to prove that there is an overlap, there must be a type `t3` such
that `t3 <: t1 and t3 <: t2`, which breaks down into element-wise overlapping.

What about different length tuples? Does there exist a tuple assignable to
different-length types?

```lua
local t1: (A | B, D | E, G)
local t2: (B | C, E | F)

local t3 = (B, E, G)

t1 = t3
t2 = t3

assert(t1 == t2)
```

There is in this case. So each element in the smaller tuple must overlap the
elements in the bigger tuple. If the smaller tuple has a `var`, make sure it
overlaps with each of the rest of the enumerable types in the larger tuple.

How about generic operations? We could supply both sides with
`Tuple({}, Unknown)` I suppose, which has the same semantics as `Unknown`
really.

---

Would it make sense to model tuples as operations? i.e.

```lua
-- the traditional representation
local tup = Tuple({ _string, _nil, boolean }, number)

-- and the same thing as a set of operations:
local tup = Intersection({
  Operation("at", Literal(1), _string),
  Operation("at", Literal(2), _nil),
  Operation("at", Literal(3), boolean),
  Operation("at", Literal("var"), number),
})

-- Type:at() refactored
function Type:at(i)
  local i_lit = Literal(i) -- memoized I suppose
  return self:eval("at", i_lit)
end
```

The `at` operator has different semantics from `Type:at` though...

```lua
-- this works with the operation implementation
assert(is_subset(Tuple({ A, B }), Tuple({ A })))

-- but `var` domain does not work...
assert(is_subset(Tuple({ A, A, A }), Tuple({}, A)))
```

`at var` domains don't cross into `at i` domains at all. Giving `at` op special
behavior seems like the wrong move.

If possible, it would be better to have a special `NumberRange` type.

```lua
-- (...A) would be represented as this
Operation("at", NumberRange(1, math.huge), A)
```

- A `NumberRange` is a subtype of number (except that's a runtime-specific
  type!)
- and Literals with number values can be subtypes of NumberRange and are
  subtypes of number
- We can piggyback off the semantics of operations for most of these things.

I suppose a hacky way to get the `var` domain is using
`:eval("at", Literal(math.huge))`.

If `NumberRanges` become a runtime-specific type, what would its underlying type
be? Could it be implemented as a `GenericOperation`? Or maybe it's an
intersection of operations over `min_value` and `max_value` or something? But if
those operations return literals, they would be tested with simple equality...

- another possible avenue could be using a literal to represent the number range
  and overriding `__eq`? But that is the hackiest sentence I've ever written,
  and it doesn't work because number ranges typically wouldn't share metatables
  with numbers. Metatables have varied semantics across Lua versions too :|

- I feel it doesn't really make sense to turn this into its own meta-type
  because it's a subtype of `number`, and it doesn't really make sense to make
  this a runtime-specific type because there isn't a meta-type that supports
  these semantics, not even `GenericOperation`.

- Another possible way to do this is with a "virtual" set (?) -- a meta-type
  that implements its own `is_subset`/`is_overlapping` semantics. It would just
  be a function, one for each relation, that defines how the meta-type works.
  This seems really hacky too.

- And yet another possible way is making primitives first-class types. Which
  means there is a class for `String`, `Number`, `Boolean`, `True`, `False`, and
  `Nil`. Literals are special cases for any of these primitives and possibly
  other types too.
  - This would break the vision of making `type-track` language-agnostic, bad.

- Possibly the easiest way to do this is to make number ranges a meta-type but
  still specify a parent type. In which case a generic `Range` type could be
  written instead.

What about coercing the type to something 1-valued? Obviously you could write
`Type:eval("at", Literal(1))`, but I'm more concerned about how the internal
methods are supposed to use it. How would `is_subset` know when to use `at` and
when to just evaluate normally?

How would unit tuples be encoded if there is always a default `:at(1)`
operation?

Using operations here also allows for comparing unions of tuples to tuples of
unions, because mixed intersections and unions already get standardized (which is
expensive btw)

So the third way to solve the last paragraph is to standardize tuples, probably
tighter than both unions and intersections. This can lead to complicated logic!

Is there a good way to go about standardizing these collections than simply
distributing all of the unions/intersections/tuples?? I think not, or at least
there is no simple way to do so.

```lua
(A, B) | (C, D) <: (A | C, B | D)
(A | C, B | D) == (A, C) | (A, D) | (B, C) | (B, D)

(A & B) | (C & D) == (A | C) & (A | D) & (B | C) & (B | D)
(A | B) & (C | D) == (A & C) | (A & D) | (B & C) | (B & D)
```

How about standardizing the collections by factoring them as much as possible?
I'm pretty sure that would require an e-graph implementation because there will
almost certainly be local maximums.

If we go down the route of forgoing `Tuples` for `Ranges` on operations,
coercing to one-valued types could be done by adding an extra step to
`Type:eval`:

```lua
-- this gets overridden and called by sub-meta-types
function Type:_eval(op, domain)
  if op == "at" and domain == ONE then
    return self
  end

  return nil
end

-- interface
function Type:eval(op, domain)
  local first = self:_eval("at", ONE)
  if not first then
    return nil
  end

  return first:_eval(op, domain)
end
```

> Note: this would mean that `Type:eval("at", ONE)` always returns itself. How is
> this behavior supposed to be overridden?

I suppose the Unit type would also have to be its own meta-type?


```lua
local UnitClass = muun("Unit", Type)

local UnitInst = UnitClass

UnitInst.debug_name = "()"

function UnitInst:eval(op, domain)
  return nil
end

function UnitInst:get_domain(op)
  return nil
end

local Unit = UnitClass()
```

This would have an identical implementation as Unknown, except that
`Unknown:at()` always returns Unknown...

It may be required to make everything that isn't a meta-type an intersection
with a "one-valued" operation. i.e.

```lua
-- all types have to go through the one-valued "decorator"
local function one_valued(t)
  local result_ref = Free()
  local result = t * Operation("at", ONE, t)
  result_ref:reify:(result)
  return result
end
```

And at that point, it may be better to wrap all the meta-type constructors with
this transformation.

One huge concern I would have with doing this is that `is_subset` (or
`is_overlapping`) doesn't handle cyclic relationships, which likely needs to be
implemented anyway. This is a concern because:

```lua
is_subset(t1, t2)

-- eventually becomes
is_subset(
  Operation("at", ONE, t1),
  Operation("at", ONE, t2)
)

-- which derives to
is_subset(ONE, ONE) and is_subset(t1, t2)

-- which comes back around to
is_subset(t1, t2)
-- starting the loop all over again
```

My first thought at solving this is a cache (like everything else that walks a
type), except for two things:

- A cache of `m * n` pairs involves `m` tables as opposed to just one table for
  everything, so it's not very memory efficient. This doesn't matter at the
  moment?

- Even if a cache was implemented, it wouldn't directly solve cyclic types
  because the cache doesn't save anything until an evaluation is completed. If
  there was a way to evaluate pending cache values, then this would be doable.

The second point comes down to the baseline issue of comparing two sets that
under some circumstances evaluate to themselves.

This is the code that evaluates whether an intersection is a subset of another
intersection:

```lua
---accepts a type `subset` if it is a subset of any type in `superset_list`
---@param subset type-track.Type
---@param superset_list type-track.Type[]
---@param i? integer
---@param j? integer
---@return boolean
function is_subset_of_any(subset, superset_list, i, j)
	for k = i or 1, j or #superset_list do
		local superset = superset_list[k]
		if is_subset(subset, superset) then
			return true
		end
	end

	return false
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

-- if we moved `is_subset_of_any` into the latter function,
---@param subset type-track.Intersection
---@param superset type-track.Intersection
---@return boolean
function Intersection.is_subset(subset, superset)
  for _, supertype in ipairs(superset.types) do
    local found = false
    for _, subtype in ipairs(subset.types) do
      if is_subset(supertype, subtype) then
        found = true
        break
      end
    end

    if not found then
      return false
    end
  end

  return true
end
```

Let's say we implemented a cache, and there was a `Pending` value set aside for
when the evaluation of that subset is not complete yet.

```lua
---@param subset type-track.Type
---@param superset type-track.Type
---@param cache type-track.PairCache
---@return boolean
function _is_subset(subset, superset, cache)
  local value = cache:get(subset, superset)
  if value ~= nil then
    return value
  end
  cache:set(subset, superset, Pending)

  local result = -- ...

  cache:set(subset, superset, result)
  return result
end

---@param subset type-track.Type
---@param superset type-track.Type
---@return boolean
function is_subset(subset, superset)
  local result = _is_subset(subset, superset, PairCache())
  assert(result ~= Pending, "is_subset returned Pending as final result")

  return result
end

---@param subset type-track.Intersection
---@param superset type-track.Intersection
---@return boolean
function Intersection.is_subset(subset, superset, cache)
  for _, supertype in ipairs(superset.types) do
    local found = false
    for _, subtype in ipairs(subset.types) do
      local maybe_subset = _is_subset(subtype, supertype, cache)
      if maybe_subset == true then
        found = true
        break
      elseif maybe_subset == Pending then
        -- idk lol
      else
        -- do nothing
      end
    end

    if not found then
      return false
    end
  end

  return true
end
```

What is supposed to happen when we get a `Pending` value? We know how we got
there at least:

- The pair has been evaluated at least once in the past.

Simplest case:

```lua
local A = Literal("A")
local B = Literal("B")
local C = Literal("C")

local ABC
do
  local ABC_ref = Free()
  ABC = Intersection({
    Operation("at", ONE, ABC_ref),
    A,
    B,
    C
  })
  ABC_ref:reify(ABC)
end

local AB
do
  local AB_ref = Free()
  AB = Intersection({
    Operation("at", ONE, AB_ref),
    A,
    B
  })
  AB_ref:reify(AB)
end
```

We know that `is_subset(ABC, AB)` must hold true, but how do we prove that?

The `Operation` comparison uses
`is_subset(ONE, ONE) and is_subset(ABC, AB)`. The former is true, and the latter
is the original comparison we're trying to make, so it ultimately becomes
`Pending`. The rest of the elements in `AB` have a subset in `ABC`, so
everything would be true except for the pending value...

Since Zermelo-Fraenkel set theory wouldn't even attempt to evaluate this due to
the Axiom of Regularity, it may be better to look to other set theories that
allow cycles.

That may not be true tho... Operations can be modeled as a set of triplets
`(op, d, r)`, where `op` is the operator, `d` is the domain, and `r` is the
range.

For example, the set of natural numbers to natural numbers over addition would
look like this:

```lua
{
  ('add', 1, 1),
  ('add', 1, 2),
  ('add', 1, 3),
  ...,
  ('add', 1, math.huge),
  ('add', 2, 1),
  ('add', 2, 2),
  ('add', 2, 3),
  ...,
  ('add', 2, math.huge),
}
```

The axiom of regularity states this: For all non-empty sets `x`, there exists an
element `y` of `x` such that the intersection of `y` and `x` are the empty set.

Does that mean that this set exists?

```lua
T = { ("at", 1, t) if T:contains(t) }
```

It depends on what the intersection of a triplet and T becomes.

From ChatGPT, if we were to use the Kuratowski definition, a pair `(a, b)` would
be represented as `{ {a}, {a, b} }`. To represent a triplet `(a, b, c)`, two
ordered pairs could be used to represent `(a, (b, c))` as
`{ {a}, { a, {{b}, {b, c}} } }`.

Let's try writing `ABC` and `AB` using the above definition.

```lua
ABC =
  { {"at"}, { "at", {{1}, {1, ABC}} } }
  * A
  * B
  * C

AB =
  { {"at"}, { "at", {{1}, {1, AB}} } }
  * A
  * B

-- where '*' is the intersection operator
```

Given that `A`, `B`, and `C` are entirely formalized, we can't really determine
its elements using those. We could replace them with what we really mean:
literals. However, this would lead to an empty intersection because literals
with different values are trivially disjoint. It may be better to use unions,
which would make `ABC` and `AB` become rosterized sets.

```lua
ABC = {
  { {"at"}, { "at", {{1}, {1, ABC}} } },
  "A", "B", "C"
}

AB = {
  { {"at"}, { "at", {{1}, {1, AB}} } },
  "A", "B"
}
```

These would be the original definitions:

```lua
ABC = { "A", "B", "C" }
AB = { "A", "B" }
```

From the original definitions, it's clear that `AB` is a subset of `ABC`. It
would be ideal if the self-referential definitions followed the same semantics.

We now have our axiomatic sets `"at"`, `1`, `"A"`, `"B"`, and `"C"`. It's clear
that `ABC` and `AB` are indirectly cyclic, but it's difficult to recognize if
they contain any elements that fulfill the law of regularity. Our axiomatic sets
make this unclear, e.g. what would the intersection of `ABC` and `"A"` be? My
guess is it's disjoint. If that's the case, then these sets trivially fulfill
the axiom of regularity.

Solution: By the axiom of pairing and union, we can generate this set:
https://en.wikipedia.org/wiki/Axiom_of_pairing#Stronger

```lua
S = {
  ("at", (1, ABC)),
  {"at", (1, ABC)},
  (1, ABC),
  {1, ABC},
  ABC
}
```

where triples `(a, b, c)` are represented as pairs `(a, (b, c))`, and a pair
`(a, b)` is represented as `{{a}, {a, b}}`.

By the axiom of regularity, there must be an element of `S` that is disjoint
from `S`.

- `("at", (1, ABC))` isn't disjoint because it contains `{"at", (1, ABC)}`
- `{"at", (1, ABC)}` isn't disjoint because it contains `(1, ABC)`
- `(1, ABC)` isn't disjoint because it contains `{1, ABC}`
- `{1, ABC}` isn't disjoint because it contains `ABC`
- `ABC` isn't disjoint because it contains `("at", (1, ABC))`

This forms a contradiction, implying that our original assumption that ABC
exists is wrong.

So generally, any sort of loop in a set can't be created because the set of all
elements in the cycle of our type could be formed and the axiom of regularity
would fail against it. That means the set `T = { ("at", 1, t) | t in T }` can't
exist.

Perhaps one way to get around cyclic types is to pair types that would be
subsets given everything else was true.

For example, if we wanted to compare these two types,

```lua
A = {A}
B = {B}
print(is_subset(A, B))
```

we could assume that A is a subset of B for all child comparisons given all
other relations hold. i.e. if the relation is pending, we assume it's truthy.

Where would this be a bad assumption? This is all unknown territory for me...

For unions, I think this makes sense.

```lua
AB = AB + "A" + "B"
ABC = ABC + "A" + "B" + "C"
```

For 1-cycle unions like this, they just get normalized out. Unioning any set
with itself gives the same set.

For 2-cycle unions, it seems about the same and should be normalized. Consider
the following:

```lua
AB = CD + "A" + "B"
CD = AB + "C" + "D"
```

We could substitute each set once:

```lua
AB = (AB + "C" + "D") + "A" + "B"
CD = (CD + "A" + "B") + "C" + "D"
```

This gives us a one-cycle union once again, and we can normalize it out, and
they would be the same set.

What if `CD` was an intersection?

```lua
AB = CD + "A" + "B"
CD = AB * "C" * "D"

-- substituting once
AB = (AB * "C" * "D") + "A" + "B"
CD = (CD + "A" + "B") * "C" * "D"

-- distributing "C" * "D"
AB = (AB * "C" * "D") + "A" + "B"
CD =
  CD * "C" * "D"
  + "A" * "C" * "D"
  + "B" * "C" * "D"

-- substituting another time
AB =
  (
    ((AB * "C" * "D") + "A" + "B")
    * "C" * "D"
  ) + "A" + "B"

CD =
  (
    CD * "C" * "D"
    + "A" * "C" * "D"
    + "B" * "C" * "D"
  ) * "C" * "D"
  + "A" * "C" * "D"
  + "B" * "C" * "D"

-- normalizing
AB =
  (AB * "C" * "D")
  + ("A" * "C" * "D")
  + ("B" * "C" * "D")
  + "A"
  + "B"
CD = -- this didn't get simplified any further
  (CD * "C" * "D")
  + ("A" * "C" * "D")
  + ("B" * "C" * "D")

-- substituting in AB
AB =
  (
    (AB * "C" * "D")
    + ("A" * "C" * "D")
    + ("B" * "C" * "D")
    + "A"
    + "B"
  ) * "C" * "D"
  + ("A" * "C" * "D")
  + ("B" * "C" * "D")
  + "A"
  + "B"

-- normalizing
AB = -- this didn't get simplified any further
  (AB * "C" * "D")
  + ("A" * "C" * "D")
  + ("B" * "C" * "D")
  + "A"
  + "B"

-- final result
AB =
  (AB * "C" * "D")
  + ("A" * "C" * "D")
  + ("B" * "C" * "D")
  + "A"
  + "B"
CD =
  (CD * "C" * "D")
  + ("A" * "C" * "D")
  + ("B" * "C" * "D")
```

Ignoring cyclic relations, it seems that `AB` and `CD` should be different sets.
`AB` includes `"A"` and `"B"` as possible elements, whereas `CD` does not. We can
normalize `AB` further actually, since unions remove elements that are
subsets of other types, and factor `CD`.

```lua
AB = (AB * "C" * "D") + "A" + "B"
CD = (CD + "A" + "B") * "C" * "D"
```

I don't think this gives any extra information though (lol), it's just shorter.

Intuition says that `CD` is a subset of `AB`, but it gives me more questions
about which values are members of each set. For example, is `"C" * "D"` a member
of `CD`? Is it a member of `AB`?

If we wanted to prove that `CD` is a subset of `AB`, maybe some standardization
would help.

It seems that for an n-cycle type, it can be turned into a 1-cycle type with
enough substitutions. And after doing this, there should be a way to remove the
cycle and turn it into an acyclic type.

For types with just one operation, the cycle can be removed quite trivially, but
including more than one operation seems to complicate the process. Let's
consider the smallest possible 1-cycle with multiple ops.

```lua
T = (T * "A") + "B"

-- first substitution
T = (((T * "A") + "B") * "A") + "B"
T = ((T * "A") * "A") + ("B" * "A") + "B" -- distributed * "A"
T = (T * "A") + ("B" * "A") + "B" -- removed duplicate * "A"
T = (T * "A") + "B" -- removed subsets, can't simplify any further
```

So the big question is what happens to `T`? The way this was solved in a 1-op
1-cycle type is by asking if the the cycle introduced anything new to the type.
In this case, it's clear that nothing new was introduced, but that still poses
the question of what to do with `"A"`. Does it stay or does it go?

```lua
-- in the case that it stays,
T = "A" + "B"

-- in the case that it goes,
T = "B"
```

Maybe the better answer is to make a substitution and then remove the cycle.

```lua
T = (((T * "A") + "B") * "A") + "B"

-- case 1
T = (("A" + "B") * "A") + "B"
T = "A" + ("A" * "B") + "B"

-- case 2
T = ("B" * "A") + "B"
T = ("A" * "B") + "B"
```

This gave two *brand new* types. What happens if a second substitution was
performed?

```lua
T = (((((T * "A") + "B") * "A") + "B") * "A") + "B"

-- case 1
T = (((("A" + "B") * "A") + "B") * "A") + "B"
T = (("A" + "A" * "B" + "B") * "A") + "B"
T = "A" + "A" * "B" + "B"

-- case 2
T = ((("B" * "A") + "B") * "A") + "B"
T = ("A" * "B") + "B"
```

This didn't give any new types. What substitution did was introduce the term
`"A" * "B"`.

Let's consider this in the bigger context of a comparison.

```lua
T = (T * "A") + "B"
U = (U * "A") + "B" + "C"

is_subset(T, U)
```

If we try to substitute and then remove the cycle, I believe we should get types
like these:

```lua
-- case 1
T = "A" + ("A" * "B") + "B"
U = "A" + ("A" * "B") + ("A" * "C") + "B" + "C"

-- case 2
T = ("A" * "B") + "B"
U = ("A" * "B") + ("A" * "C") + "B" + "C"
```

In *both* cases, `T` is in fact a subset of `U`. And even if the substitution
wasn't performed, that statement would still be true in both cases. I suppose
this makes comparisons across cycles simple, sort of.

But comparing a cyclic type to an acyclic type may be difficult. Let's consider
the case where `T` is acyclic.

```lua
T = "A" + ("A" * "B") + "B"
U = (U * "A") + "B" + "C"
```

If `T` was a subset of `U`, that would mean substituting `T` in place of `U`
would be possible; that is kind of what assignment implies. If we do that, the
cycle ends.

```lua
U = (U * "A") + "B" + "C"

-- substitute U with T
U = (("A" + ("A" * "B") + "B") * "A") + "B" + "C"
U = "A" + ("A" * "B") + "B" + "C"
```

Under this rule, `U` does become a superset of `T`. If `T` didn't have `"A"` as
an operand in the union, the relation would still hold. And since `T` with `"A"`
is a superset of `T` without `"A"`, it may be more helpful to consider cyclic
supertypes with case 1 in mind.

I would assume you can't perform this kind of substitution when the subset is a
cyclic type though. Let's see...

```lua
T = (T * "A") + "B"
U = "A" + ("A" * "B") + ("A" * "C") + "B" + "C"
```

Because `U` is supposed to be a superset, it's likely that `T` will become a
different type when substituted with `U`. If we use both cases of reduction (?)
rules, `T` would be a subset of `U`. There is a type that only one case covers
though: `U` without `"A"`.

```lua
T = (T * "A") + "B"
U = ("A" * "B") + ("A" * "C") + "B" + "C"

-- case 1: T is not a subset of U
T = "A" + ("A" * "B") + "B"
U = ("A" * "B") + ("A" * "C") + "B" + "C"

-- case 2: T is a subset of U
T = ("A" * "B") + "B"
U = ("A" * "B") + ("A" * "C") + "B" + "C"
```

Quite troubling... let's say we perform the substitution anyway.

If `T` is one of the two cases, `case1 + case2`, then `T` is not a subset of
`U`. If `T` is both cases at the same time, `case1 * case2`, then `T` is a
subset of `U`.

We could say as a general rule that cyclic types are never subsets of acyclic
types... I'm not sure if that would be logical though.

We could take an approach where both cases are combined into one type under
union or intersection. If we take this approach, I would interpret this issue as
"it could be either case 1 or case 2," implying that a union would be the most
appropriate representation of the type. If this is the case, it would be the
same as saying `T` takes the type of case 2.

If we do that, we could normalize cyclic types into acyclic types via
substitution followed by removal of the self-reference.

So far, we've been dealing with cyclic types with one self-reference. What if
there is more than one self-reference?

```lua
T = (T * "A") + (T * "B") + "C"

-- substitution 1
T =
  (((T * "A") + (T * "B") + "C") * "A")
  + (((T * "A") + (T * "B") + "C") * "B")
  + "C"

-- normalization
T =
  (T * "A") + (T * "A" * "B") + ("A" * "C")
  + (T * "A" * "B") + (T * "B") + ("B" * "C")
  + "C"
T =
  (T * "A")
  + (T * "A" * "B")
  + (T * "B")
  + ("A" * "C")
  + ("B" * "C")
  + "C"

-- substitution 2
T =
  (
    (
      (T * "A")
      + (T * "A" * "B")
      + (T * "B")
      + ("A" * "C")
      + ("B" * "C")
      + "C"
    ) * "A"
  ) + (
    (
      (T * "A")
      + (T * "A" * "B")
      + (T * "B")
      + ("A" * "C")
      + ("B" * "C")
      + "C"
    ) * "A" * "B"
  ) + (
    (
      (T * "A")
      + (T * "A" * "B")
      + (T * "B")
      + ("A" * "C")
      + ("B" * "C")
      + "C"
    ) * "B"
  )
  + ("A" * "C")
  + ("B" * "C")
  + "C"

-- normalization
T =
  (
    (T * "A")
    + (T * "A" * "B")
    + ("A" * "C")
    + ("A" * "B" * "C")
  ) + (
    (T * "A" * "B")
    + (T * "A" * "B")
    + (T * "A" * "B")
    + ("A" * "B" * "C")
  ) + (
    (T * "A" * "B")
    + (T * "B")
    + ("A" * "B" * "C")
    + ("B" * "C")
  )
  + ("A" * "C")
  + ("B" * "C")
  + "C"
T =
  (T * "A")
  + (T * "A" * "B")
  + (T * "B")
  + ("A" * "C")
  + ("A" * "B" * "C")
  + ("B" * "C")
  + "C"
```

The type exploded into a ton of permutations! Confusing and mentally taxing...

---

I realized that thinking about this does not solve the problem of
`("at", 1, self)`, because operations are not reducible in the way unions and
intersections are.

It may be better not to manipulate a cyclic type into an acyclic type.

```lua
local function one_valued(t)
  local result_ref = Free()
  local result = Intersection({
    Operation("at", ONE, result_ref),
    t
  })
  result_ref:reify(result)
  return result
end
one_valued = memoize(one_valued)
```

Perhaps pending values could be ignored, e.g. it's `true` for `all` relations
and `false` for `any` relations.

How would operations in unions interact with `Pending` then? Operations would
likely use a `true` value because operations require *all* of three conditions
to be true for the subset relation to exist.

```lua
function Operation.is_subset(subset, superset)
  return subset.op == superset.op
    and is_subset(superset.domain, subset.domain)
    and is_subset(subset.range, superset.range)
end
```

---

One interesting property of 1-cycle types is that they can be un-normalized into
a type with any number of cycles.

For example, these three have the same type:

```lua
union1 = union1 + "A" + "B"

union2A = union2B + "A"
union2B = union2A + "B"
```

It wouldn't be hard to see that one level of substitution would result in the
same type after removing duplicates, thus proving that the cycle doesn't change
the type and can be safely removed. Is there a two-cycle single-operation
aggregation of types that result in two distinctly different types?

If I wanted to make that, I would probably need to write a type that flip-flops
between one type and the other every substitution. Nothing obvious comes up off
the top of my head... but if one were to exist, that would form a contradiction!

```lua
unionA = unionB + "A"

-- after one step of substitution and normalization
unionA = unionB + "B"
```

There shouldn't be a way to "cancel out" a type during substitution/
normalization unless it's a subset of another type, but even if that were to
happen, another substitution would leave the superset there and end the
flip-flop.

---

So my current plan for making `is_subset` handle cycles is letting it return one
of three values:
- `true`
- `false`
- an array of cycles, each implemented as a list of instructions showing how to
  navigate the type graph.

If a boolean is returned, we're done.

If an array of cycles is returned, we prove that the subset contains all of
them. This is done by navigating each path in the cycle and seeing if it's
either equal to the subset or a subset of the superset. The latter implies an
infinite loop, so either it should be implemented carefully or not at all.

The way we navigate the type graph is important! if the superset uses an
`Operation`, perhaps the subset uses an `Intersection` with it included. In
effect, we aren't navigating the data structure directly. Instead, we navigate
through the domains and ranges of the type.

I should note that normalization doesn't cover all cases of unions and
intersections as mentioned above. Cases of one mentioned operation is trivial,
but every additional operation adds a dimension to the issue.

---

Referring to this type,

```lua
T = (T * "A") + "B"

-- case 1
T = "A" + ("A" * "B") + "B"

-- case 2
T = ("A" * "B") + "B"
```

It would be great if there was a way to generate an infinite union/intersection
of `T` and prove that it's equal to another type through induction. Let's give
it a try.

Every time we perform a substitution, we are given one requirement:
- it must have `"A"`

And we are given two options as to what `"A"` intersects with:
- left: `T * "A"`, which simplifies to `T`
- right: `"B"`

Let's say we keep going left forever. We would just have an infinite
intersection of `"A"`s which simplify to just `"A"`. If we take a right even
once, the chain ends, and we end up with a finite number of `"A"` intersections
followed by an intersection with `"B"`, which simplifies to just `"A" * "B"`.

So a subset of `T` must be either `"B"`, `"A"` intersected with an
infinite number of `"A"`s, or `"A"` intersected with a finite number of `"A"`s
and `"B"`. This gives us three options for subsets of `T`:
- `"A"`
- `"A" * "B"`
- `"B"`

Given all other subsets of `T` simplify to one of these three types, we can
safely conclude that `T` is simply a union of these three resulting types.

```lua
T_acyclic = "A" + ("A" * "B") + "B"
```

So `T` is equivalent to case 1.

We can compare this to a finite state machine involving the three options.

```lua
(T) -> (T * "A")
(T) -> ("B")

(T * "A") -> (T * "A")
(T * "A") -> ("A" * "B")
```

All subsets of `T` would be required to follow this finite state machine or
break its own definition.

What if `T` were a little more complex?

```lua
T = (T * "A") + (T * "B") + "C"
```

I think the finite state machine would look similar here too.

```lua
(T) -> (T * "A")
(T) -> (T * "B")
(T) -> ("C")

(T * "A") -> (T * "A")
(T * "A") -> (T * "A" * "B")
(T * "A") -> ("A" * "C")

(T * "B") -> (T * "B")
(T * "B") -> (T * "A" * "B")
(T * "B") -> ("B" * "C")

(T * "A" * "B") -> (T * "A" * "B")
(T * "A" * "B") -> ("A" * "B" * "C")
```

What about the case where the union is the inner operation in `T`?

```lua
T = (T + "A") * "B"

-- normalization
T = (T * "B") + ("A" * "B")
```

Normalization gives it the same shape as intersection being first.

So, cyclic types can be turned into acyclic types by turning it into an FSM and
considering the case of traveling down one path of the FSM forever as an option.
And >1-cycle types can be turned into 1-cycle types using substitution. Thus,
the problem of cyclic types has been solved for intersections and unions. At
least conceptually.

This *still* doesn't solve the issue of cyclic types stemming from operations.
Complement meta-types would also suffer from this issue as an extra layer of
abstraction.

Here are some issues with solving cyclic types in operations:
1. for the most part, operations are not distributive across intersections/
   unions. That means they can't be standardized to one model as easily as them.
   This isn't really true for complement types, so maybe there's still hope.

```lua
T = (A >> B) + (C >> D)
U = (A * C) >> (B + D)
assert(T ~= U)
assert(is_subset(T, U))

V = (A >> B) * (C >> D)
W = (A + C) >> (B * D)
assert(V ~= W)
assert(is_subset(W, V))
```

---

Is there a way to make them standardized here? There's likely a way to split the
domain and range to one element each.

```lua
U = (A * C) >> (B + D)
U = ((A * C) >> B) + ((A * C) >> D) -- split ranges
U = ((A >> B) * (C >> B)) + ((A >> D) * (C >> D)) -- split domains
-- the components of `U` can be matched for each element of `T`

W = (A + C) >> (B * D)
W = ((A + C) >> B) * ((A + C) >> D) -- split ranges
W = ((A >> B) + (C >> B)) * ((A >> D) + (C >> D)) -- split domains
W = -- normalizing W...
  ((A >> B) * (A >> D))
  + ((A >> B) * (C >> D))
  + ((C >> B) * (A >> D))
  + ((C >> B) * (C >> D))
-- The union of `W` contains `V` verbatim
```

So it does so happen that unions/intersections in domains/ranges can be
distributed.

---

2. operations are not "infinitely reducible" like unions/intersections are.
   `A * A` becomes just `A`, but `A >> (A >> A)` doesn't become `A >> A` or `A`
   because they are distinctly different types.

```lua
P = (P >> "A") + "B"

-- generates this FSM
(P) -> (P >> "A")
(P) -> ("B")

(P >> "A") -> ((P >> "A") >> "A") -- ...
(P >> "A") -> ("B" >> "A")
```

In this case, `P` truly becomes an infinitely descending type. Is there some
sort of logic I can use that makes this type "pseudo-reducible"?

`P` could be modeled as a recursive function/generic like this:

```lua
P<n> = (P<n - 1> >> "A")
P<0> = "B"

-- P is the union of all P<n>
P = P<0> + P<1> + P<2> + ... + P<math.huge>
```

And the FSM for *that* would be possible to present finitely:

```lua
(P<n>) -> (P<n - 1> >> "A")
(P<0>) -> ("B")
```

Can I present `P` in terms of `P<n>` as an FSM though? Sort of I guess??

```lua
(P) -> (UP<math.huge>)

-- as in UnionP
(UP<n>) -> (UP<n - 1> + P<n>)

(P<n>) -> (P<n - 1> >> "A")
(P<0>) -> ("B")
```

So a subset of `P` would have to be able to travel through this FSM. It would be
very inefficient to travel through this FSM though, with a complexity of
`O(n^2)`... if every union-element in the subset of `P` could first determine
their `P<0>` and build up to `P<n>`, that would be faster.

How would cyclic types travel through this FSM though? Not sure lol. That takes
a lot of brainpower to figure out... for starters, cyclic types wouldn't have a
`P<0>`.

It would probably have to be proven via induction, which means coming up with a
base case and a formula for a recursive case. Of course, since our system
"generates" the proof, the formula would have to be concrete somehow.

Representing `P` as a function here may not be doable for functions that have
multiple parameters.

Would it be better to write every type as an FSM, and checking a subset of that
type would either mean making it travel through the FSM or comparing the
structure of the FSM directly?