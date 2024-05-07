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

		it 'rejects literals with the same value but non-subset ops', ->
			T = Literal 'T'
			U = Literal 'U'
			A1 = Literal 'A', T
			A2 = Literal 'A', U

			assert.is_false Literal.is_subset A1, A2
