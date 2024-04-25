import
	Type, Tuple, Union, Intersection, Operator, Literal
	is_subset
from require 'type-track.meta'

describe 'is_subset', ->
	setup -> Tuple.default_var_arg = nil

	A = Literal 'A'
	B = Literal 'B'

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
