import
	is_subset
	Type, Tuple, Operator, Literal, GenericOperator, Unknown
from require 'type-track.meta'

empty = Tuple {}
unknown_var = Tuple {}, Unknown

describe 'is_subset', ->
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'

	it 'accepts A <: A | B', ->
		assert.is_true is_subset A, A + B

	it 'accepts A & B <: A', ->
		assert.is_true is_subset A * B, A

	it 'accepts { A: A, B: B } <: { A: A }', ->
		index_a = Operator 'index', A, A
		index_b = Operator 'index', B, B

		sub_t = index_a * index_b
		super_t = index_a

		assert.is_true is_subset sub_t, super_t

	it 'accepts ({ A: A, B: B }, { A: A, B: B }) <: { A: A }', ->
		index_a = Operator 'index', A, A
		index_b = Operator 'index', B, B

		-- type SUB = read { A: "A", B: "B" }
		sub_t = index_a * index_b

		-- type SUPER = read { A: "A" }
		super_t = index_a

		-- SUB <: SUPER
		assert.is_true is_subset sub_t, super_t

		-- a, b: (SUB, SUB)
		ab_type = Tuple { sub_t, sub_t }

		-- x: SUPER
		x_type = super_t

		-- x = a, b
		assert.is_true is_subset ab_type, x_type

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

		-- meta.DEBUG = true
		assert.is_true is_subset type2, type1
		-- meta.DEBUG = false

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
		it 'rejects anything compared to zero elements', ->
			assert.is_false is_subset Type!, Tuple {}

		it 'accepts the same type compared to zero elements with a var-arg', ->
			assert.is_true is_subset A, Tuple {}, A
			assert.is_false is_subset B, Tuple {}, A

		it 'accepts the same type compared to one element', ->
			assert.is_true is_subset A, Tuple { A }
			assert.is_false is_subset B, Tuple { A }

		it 'rejects anything compared to two elements', ->
			assert.is_false is_subset Type!, Tuple { Type!, Type! }
