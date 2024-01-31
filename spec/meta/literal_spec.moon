import Type, Literal from require 'type-track.meta'

describe 'literals', ->
	it 'is a Type', ->
		A = Literal 'A'

		assert.is_true A\is_instance Type

	describe 'is_subset', ->
		it 'accepts itself', ->
			A = Literal 'A'

			assert.is_true A\is_subset A

		it 'rejects other literals', ->
			A = Literal 'A'
			B = Literal 'B'

			assert.is_false A\is_subset B

