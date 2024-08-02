import
	is_subset
	Type, Tuple, Operation, Literal, GenericOperation
	Unknown, Never
	Intersection, Union
from require 'type-track.meta'


describe 'is_subset', ->
	local unknown_var, A, B, C, D
	lazy_setup ->
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
		type1 = Operation 'call', A, Tuple.Unit
		type2 = GenericOperation(
			'call'
			(T) ->
				return nil if not T

				T, Tuple.Unit

			(domain) -> domain\at 1
		)

		assert.is_true is_subset type1, type2

	it 'accepts { call(T): () -> T } <: { call: () -> A }', ->
		type1 = Operation 'call', Tuple.Unit, A
		type2 = GenericOperation(
			'call'
			(T) ->
				return nil if not T

				Tuple.Unit, T

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
		type2 = Operation 'X', A * C, B + D

		assert.is_true is_subset type1, type2
		assert.is_true is_subset type2, type1

	it 'rejects map_of(A, C) <: map_of(A, BC) and map_of(A, BC) <: map(A, C) given BC <: C', ->
		map_of = (K, V) ->
			Intersection {
				Operation 'index', K, V
				Operation 'newindex', Tuple({ K, V }), Never
			}

		BC = B * C

		assert.is_true is_subset BC, C
		assert.is_false is_subset C, BC

		mapC = map_of A, C
		mapBC = map_of A, BC

		-- these are invariant
		assert.is_false is_subset mapBC, mapC
		assert.is_false is_subset mapC, mapBC

	it 'accepts read_map_of(A, BC) <: read_map_of(A, C) given BC <: C', ->
		read_map_of = (K, V) -> Operation 'index', K, V

		BC = B * C

		assert.is_true is_subset BC, C
		assert.is_false is_subset C, BC

		mapC = read_map_of A, C
		mapBC = read_map_of A, BC

		assert.is_true is_subset mapBC, mapC
		assert.is_false is_subset mapC, mapBC

	pending 'accepts (A, B) | (C, D) <: (A | C, B | D)', ->
		union = Union {
			Tuple { A, B }
			Tuple { B, C }
		}

		tup = Tuple {
			Union { A, C }
			Union { B, D }
		}

		assert.is_true is_subset union, tup
		assert.is_false is_subset tup, union

	pending 'accepts (A | B, C | D) == (A, C) | (A, D) | (B, C) | (B, D)', ->
		tup = Tuple {
			Union { A, B }
			Union { C, D }
		}

		union = Union {
			Tuple { A, C }
			Tuple { A, D }
			Tuple { B, C }
			Tuple { B, D }
		}

		assert.is_true is_subset tup, union
		assert.is_true is_subset union, tup

	it 'accepts () <: { x: A -> B }', ->
		op = Operation 'x', A, B

		assert.is_false is_subset Tuple.Unit, op
