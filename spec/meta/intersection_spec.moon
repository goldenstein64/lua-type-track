import
	Tuple, Literal, Intersection, Never, Operator
	Type, is_subset
from require 'type-track.meta'

describe 'intersection', ->
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'

	it 'is a type', ->
		inter = Intersection { A, B }

		assert.is_true inter\is_instance Type

	describe 'unify', ->
		it 'works with Never', ->
			inter = Intersection { Never, A }

			assert.equal Never, inter\unify!

		it 'simplifies nested intersections', ->
			inter = Intersection { A, Intersection { B, C } }
			unified = inter\unify!
			for t in *unified.types
				assert.is_false t\is_instance Intersection

		it 'simplifies subset operators #only', ->
			op1 = Operator 'call', (Tuple { A, B }), C
			op2 = Operator 'call', A, C

			-- assuming op1 is a subset of op2,
			assert.is_true is_subset op1, op2

			inter = Intersection { op1, op2 }
			unified = inter\unify!

			-- unifying should return the subset, i.e. op1
			assert.is_true is_subset op1, unified
			assert.is_true is_subset unified, op1
