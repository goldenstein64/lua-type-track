import Type, Union, Intersection from require 'type-track.meta'

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
		it 'exists', ->
			assert.not_nil Type.is_subset

		it 'returns false', ->
			type1 = Type!
			assert.is_false Type.is_subset type1, type1

	describe 'default eval', ->
		it 'exists', ->
			type1 = Type!
			assert.not_nil type1.eval

		it 'errors', ->
			type1 = Type!

			assert.error -> type1\eval!

	describe 'default at', ->
		it 'exists', ->
			type1 = Type!
			assert.not_nil type1.at

		it 'returns itself on 1', ->
			type2 = Type!

			assert.equal type2, type2\at 1

		it 'returns nil otherwise', ->
			type3 = Type!

			assert.is_nil type3\at 2
			assert.is_nil type3\at 3
			assert.is_nil type3\at 4

	describe 'default unify', ->
		it 'returns itself', ->
			type1 = Type!
			assert.equal type1, type1\unify!

		it 'returns nil if already visited', ->
			type2 = Type!
			visited = { [type2]: true }
			assert.is_nil type2\unify visited
