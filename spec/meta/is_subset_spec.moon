import
	Type, Tuple, Union, Intersection, Operator, Literal, GenericOperator
	Never, Unknown
	is_subset
from require 'type-track.meta'

empty = Tuple {}
unknown_var = Tuple {}, Unknown

describe 'is_subset', ->
	setup -> Tuple.default_var_arg = nil

	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'

	it 'accepts A <: A | B', ->
		assert.is_true is_subset A, A + B

	it 'accepts A & B <: A', ->
		assert.is_true is_subset A * B, A

	it 'accepts { call: A -> () } <: { call(T): T -> () }', ->
		type1 = Operator 'call', A, empty
		type2 = GenericOperator(
			'call'
			(type_params) ->
				T = type_params\at 1
				return nil if not T

				T, empty

			(domain) -> domain\at 1
		)

		assert.is_true is_subset type1, type2

	it 'accepts { call(T): () -> T } <: { call: () -> A }', ->
		type1 = Operator 'call', empty, A
		type2 = GenericOperator(
			'call'
			(type_params) ->
				T = type_params\at 1
				return nil if not T

				empty, T

			(domain, range) -> range\at 1
		)

		assert.is_true is_subset type2, type1

	it 'accepts { call: A -> A } <: { call(T): T -> T }', ->
		type1 = Operator 'call', A, A
		type2 = GenericOperator(
			'call'
			(type_params) ->
				T = type_params\at 1
				return nil if not T

				T, T

			(domain) -> domain\at 1
		)

		-- _G.DEBUG = true
		assert.is_true is_subset type2, type1

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
		assert.is_true is_subset A * B, (A * B) + (A * C)

	it 'rejects A & B <: B & C', ->
		assert.is_false is_subset A * B, B * C

	it 'rejects B & C <: A & B', ->
		assert.is_false is_subset B * C, A * B

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
