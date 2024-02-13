import Type, Callable, Union, Intersection, Tuple from require 'type-track.meta'

describe 'type', ->
	it 'creates Callables with / operator', ->
		type1 = Type!
		type2 = Type!

		callable = type1 / type2
		assert.is_true callable\is_instance Callable

	it 'creates Unions with + operator', ->
		type1 = Type!
		type2 = Type!

		union = type1 + type2
		assert.is_true union\is_instance Union

	it 'creates Intersections with * operator', ->
		type1 = Type!
		type2 = Type!

		intersection = type1 * type2
		assert.is_true intersection\is_instance Intersection
