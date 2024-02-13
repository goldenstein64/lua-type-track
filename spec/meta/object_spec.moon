import Literal, Object, Tuple, Callable, Type from require 'type-track.meta'

describe 'object', ->
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'
	AB = Tuple { A, B }
	BA = Tuple { B, A }
	ABC = Tuple { A, B, C }

	it 'is a Type', ->
		obj = Object!

		assert.is_true obj\is_instance Type

	describe 'is_subset', ->
		it 'rejects objects with unequal data types', ->
			obj1 = Object {}, 'string'
			obj2 = Object {}, 'number'

			assert.is_false obj1\is_subset obj2
			assert.is_false obj2\is_subset obj1

		it 'rejects objects without an operation', ->
			obj1 = Object { index: A / BA, newindex: B / A }

			obj2 = Object { index: A / BA }

			assert.is_true obj1\is_subset obj2
			assert.is_false obj2\is_subset obj1

	describe 'call', ->
		it 'returns nil if the call operation is not supported', ->
			obj = Object!

			assert.is_nil obj\call!
