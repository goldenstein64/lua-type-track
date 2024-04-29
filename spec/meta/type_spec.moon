import Type, Callable, Union, Intersection, Tuple, Literal from require 'type-track.meta'

describe 'type', ->
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

	describe 'default is_subset', ->
		it 'returns false', ->
			type1 = Type!
			type2 = Type!

			assert.is_false Type.is_subset type1, type2

	describe 'default eval', ->
		it 'errors', ->
			type1 = Type!

			assert.error -> type1\eval!

	describe 'default at', ->
		it 'returns itself on 1', ->
			A = Literal 'A'

			assert.equal A, A\at 1

		it 'returns nil otherwise', ->
			A = Literal 'A'

			assert.is_nil A\at 2
			assert.is_nil A\at 3
			assert.is_nil A\at 4
