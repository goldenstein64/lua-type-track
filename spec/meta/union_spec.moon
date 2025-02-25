import
	is_subset
	Literal, Union, Type, Unknown, Free, Operation
	Intersection
from require 'type-track.meta'

describe 'Union', ->
	local A, B, C
	lazy_setup ->
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
			normalized = union\normalize!

			for t in *normalized.types
				assert.is_false t\is_instance Union

		it 'simplifies nested unions with duplicates', ->
			union = Union { A, Union { A, B } }

			normalized = union\normalize!

			assert.equal 2, #normalized.types
			{ elem1, elem2 } = normalized.types
			assert.is_true elem1.value == "A" or elem1.value == "B"
			if elem1.value == "A"
				assert.equal "B", elem2.value
			elseif elem1.value == "B"
				assert.equal "A", elem2.value

		it 'works with order-1 cyclic unions', ->
			op = Operation 'call', A, B

			union_ref = Free!
			union = Union { op, op, union_ref }
			union_ref\reify union

			expected = op
			normalized = union\normalize!
			assert.not_nil normalized
			assert.is_true is_subset expected, normalized
			assert.is_true is_subset normalized, expected

		it 'returns nil for order-2 cyclic unions', ->
			op_ref = Free!
			union_ref = Free!

			op = Operation "some", A, union_ref
			union = op_ref + B

			op_ref\reify op
			union_ref\reify union

			normalized_union = union\normalize!

			assert.is_nil normalized_union

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

	describe 'refine', ->
		it 'works', ->
			valueLit = Literal 'value'
			fooLit = Literal 'foo'
			barLit = Literal 'bar'
			typeLit = Literal 'type'

			fooType = Intersection({
				Operation 'index', typeLit, fooLit
				Operation 'index', fooLit, valueLit
			})
			barType = Intersection({
				Operation 'index', typeLit, barLit
				Operation 'index', barLit, valueLit
			})
			union = Union({ fooType, barType })

			constraint = Operation 'index', typeLit, fooLit

			refined = union\refine constraint

			assert.is_true is_subset fooType, refined
			assert.is_true is_subset refined, fooType
