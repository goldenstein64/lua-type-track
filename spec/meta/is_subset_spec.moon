import
	is_subset
	Type, Tuple, Operation, Literal, GenericOperation, Unknown
from require 'type-track.meta'


describe 'is_subset', ->
	unit = Tuple {}
	unknown_var = Tuple {}, Unknown

	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'
	D = Literal 'D'

	it 'accepts A <: A | B', ->
		assert.is_true is_subset A, A + B

	it 'accepts A & B <: A', ->
		assert.is_true is_subset A * B, A

	it 'accepts { A: A, B: B } <: { A: A }', ->
		index_a = Operation 'index', A, A
		index_b = Operation 'index', B, B

		sub_t = index_a * index_b
		super_t = index_a

		assert.is_true is_subset sub_t, super_t

	it 'accepts ({ A: A, B: B }, { A: A, B: B }) <: { A: A }', ->
		index_a = Operation 'index', A, A
		index_b = Operation 'index', B, B

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
		type1 = Operation 'call', A, unit
		type2 = GenericOperation(
			'call'
			(T) ->
				return nil if not T

				T, unit

			(domain) -> domain\at 1
		)

		assert.is_true is_subset type1, type2

	it 'accepts { call(T): () -> T } <: { call: () -> A }', ->
		type1 = Operation 'call', unit, A
		type2 = GenericOperation(
			'call'
			(T) ->
				return nil if not T

				unit, T

			(domain, range) -> range\at 1
		)

		assert.is_true is_subset type2, type1

	it 'accepts { call: A -> A } <: { call(T): T -> T }', ->
		type1 = Operation 'call', A, A
		type2 = GenericOperation(
			'call'
			(T) ->
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

	it 'accepts A & B <: (A & B) | (A & C)', -> assert.is_true is_subset A * B, (A * B) + (A * C)
	it 'accepts B | C <: (A | B) | (C | D)', -> assert.is_true is_subset A + B, (A + B) + (C + D)

	it 'rejects A & B <: B & C', -> assert.is_false is_subset A * B, B * C
	it 'rejects B & C <: A & B', -> assert.is_false is_subset B * C, A * B

	it 'accepts anything <: ()', -> assert.is_true is_subset A, Tuple {}

	it 'accepts A <: (...A)', -> assert.is_true is_subset A, Tuple {}, A
	it 'rejects B <: (...B)', -> assert.is_false is_subset B, Tuple {}, A

	it 'accepts A <: (A)', -> assert.is_true is_subset A, Tuple { A }
	it 'rejects B <: (B)', -> assert.is_false is_subset B, Tuple { A }

	it 'rejects A <: (A, A)', -> assert.is_false is_subset A, Tuple { A, A }

	it 'accepts ({ X: A -> B } | { X: C -> D }) == ({ X: (A & C) -> (B | D) })', ->
		type1 = (Operation 'X', A, B) + (Operation 'X', C, D)
		type2 = Operation 'X', (A * C), (B + D)

		assert.is_true (is_subset type1, type2), 'type1 </: type2'
		assert.is_true (is_subset type2, type1), 'type2 </: type1'
