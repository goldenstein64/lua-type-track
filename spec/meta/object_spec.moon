import Literal, Object, Tuple, Type from require 'type-track.meta'

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

			assert.is_false Object.is_subset obj1, obj2
			assert.is_false Object.is_subset obj2, obj1

		it 'rejects objects without an operation', ->
			obj1 = Object { index: A / B, newindex: AB / C }

			obj2 = Object { index: A / B }

			assert.is_true Object.is_subset obj1, obj2
			assert.is_false Object.is_subset obj2, obj1

		it 'rejects objects with an incompatible operation', ->
			obj1 = Object { index: A / C }
			obj2 = Object { index: B / C }

			assert.is_false Object.is_subset obj1, obj2
			assert.is_false Object.is_subset obj2, obj1

	describe 'call', ->
		it 'returns nil if the call operation is not supported', ->
			obj = Object!

			assert.is_nil obj\call!

		it 'returns a type if the call operation is supported', ->
			obj = Object { call: A / B }

			assert.equal B, obj\call A

		it 'supports additional operations with a second argument', ->
			obj = Object { foo: A / B }

			assert.equal B, obj\call A, 'foo'
