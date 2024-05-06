import
	Type, Tuple, Union, Intersection, Operator, Literal
	is_subset
from require 'type-track.meta'

describe 'is_subset', ->
	setup -> Tuple.default_var_arg = nil

	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'

	it 'accepts A <: A | B', ->
		assert.is_true is_subset A, A + B

	it 'accepts A & B <: A', ->
		assert.is_true is_subset A * B, A

	it 'accepts A | (B & C) == (A | B) & (A | C)', ->
		type1 = (A + (B * C))
		type2 = ((A + B) * (A + C))

		assert.is_true is_subset type1, type2
		assert.is_true is_subset type2, type1

	it 'accepts (A | B) & (A | C) <: A | B', ->
		type1 = ((A + B) * (A + C))
		type2 = A + B

		assert.is_true is_subset type1, type2

	it 'accepts A & B <: (A & B) | (A & C)', ->
		type1 = (A * B)
		type2 = ((A * B) + (A * C))

		assert.is_true is_subset type1, type2

	describe 'when compared up to tuples', ->
		it 'accepts anything compared to zero elements', ->
			assert.is_true is_subset Type!, Tuple {}

		it 'accepts the same type compared to zero elements with a var-arg', ->
			assert.is_true is_subset A, Tuple {}, A
			assert.is_false is_subset B, Tuple {}, A

		it 'accepts the same type compared to one element', ->
			assert.is_true is_subset A, Tuple { A }
			assert.is_false is_subset B, Tuple { A }

		it 'rejects the same type compared to two elements', ->
			assert.is_false is_subset A, Tuple { A, A }
