import
	is_subset
	Literal, Union, Type, Unknown, Free, Operation
	Intersection
from require 'type-track.meta'

describe 'Union', ->
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'

	it 'is a Type', ->
		union = Union { A, B }

		assert.is_true union\is_instance Type

	describe 'normalize', ->
		it 'works with Unknown', ->
			union = Union { A, Unknown }

			assert.equal Unknown, union\normalize!

		it 'simplifies nested unions', ->
			union = Union { A, Union { B, C } }
			unified = union\normalize!

			for t in *unified.types
				assert.is_false t\is_instance Union

		it 'simplifies nested unions with duplicates', ->
			union = Union { A, Union { A, B } }

			unified = union\normalize!

			assert.equal 2, #unified.types
			{ elem1, elem2 } = unified.types
			assert.is_true elem1.value == "A" or elem1.value == "B"
			if elem1.value == "A"
				assert.equal "B", elem2.value
			elseif elem1.value == "B"
				assert.equal "A", elem2.value

		it 'works with order-1 cyclic unions', ->
			op = Operation 'call', A, B

			union = Free!
			union.value = Union { op, op, union }

			expected = op
			unified = union\normalize!
			assert.not_nil unified
			assert.is_true is_subset expected, unified
			assert.is_true is_subset unified, expected

		it 'returns nil for order-2 cyclic unions', ->
			op = Free!
			union = Free!

			op.value = Operation "some", A, union
			union.value = Union { op, B }

			unified_union = union\normalize!

			assert.is_nil unified_union

	describe 'get_domain', ->
		it 'returns nil for wrong op', ->
			union = Union {
				Operation 'call', A, B
				Operation 'call', B, C
			}

			assert.is_nil union\get_domain 'index'

		it 'returns an intersection of types', ->
			union = Union {
				Operation 'call', A, B
				Operation 'call', B, C
			}

			result = union\get_domain 'call'
			target = Intersection { A, B }

			assert.is_true is_subset result, target
			assert.is_true is_subset target, result

	describe 'eval', ->
		it 'returns nil for wrong op', ->
			union = Union {
				Operation 'call', A, B
				Operation 'call', B, C
			}

			assert.is_nil union\eval 'index', A

		it 'returns nil for wrong param', ->
			union = Union {
				Operation 'call', A, B
				Operation 'call', B, C
			}

			assert.is_nil union\eval 'call', C

		it 'returns a union of types', ->
			union = Union {
				Operation 'call', A, B
				Operation 'call', A, C
			}

			result = union\eval 'call', A
			target = Union { B, C }

			assert.is_true is_subset result, target
			assert.is_true is_subset target, result
