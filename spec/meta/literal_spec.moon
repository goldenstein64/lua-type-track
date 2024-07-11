import Type, Literal from require 'type-track.meta'

describe 'literals', ->
	it 'is a Type', ->
		A = Literal 'A'

		assert.is_true A\is_instance Type

	describe 'is_subset', ->
		it 'accepts itself', ->
			A = Literal 'A'

			assert.is_true Literal.is_subset A, A

		it 'accepts a copy of itself', ->
			A1 = Literal 'A'
			A2 = Literal 'A'

			assert.is_true Literal.is_subset A1, A2

		it 'rejects other literals', ->
			A = Literal 'A'
			B = Literal 'B'

			assert.is_false Literal.is_subset A, B
		
		-- literals don't use ops in is_subset
		it 'accepts literals with same values and different ops', ->
			T = Literal 'T'
			U = Literal 'U'
			A1 = Literal 'A', T
			A2 = Literal 'A', U

			assert.is_true Literal.is_subset A1, A2

	describe 'is_overlapping', ->
		it 'accepts itself', ->
			A = Literal 'A'

			assert.is_true Literal.is_overlapping A, A

		it 'accepts a copy of itself', ->
			A1 = Literal 'A'
			A2 = Literal 'A'

			assert.is_true Literal.is_overlapping A1, A2

		it 'rejects other literals', ->
			A = Literal 'A'
			B = Literal 'B'

			assert.is_false Literal.is_overlapping A, B
		
		-- literals don't use ops in is_overlapping
		it 'accepts literals with same values and different ops', ->
			T = Literal 'T'
			U = Literal 'U'
			A1 = Literal 'A', T
			A2 = Literal 'A', U

			assert.is_true Literal.is_overlapping A1, A2
