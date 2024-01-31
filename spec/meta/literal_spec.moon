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

	describe 'at', ->
		it 'returns itself on 1', ->
			A = Literal 'A'

			assert.equal A, A\at 1

		it 'returns nil otherwise', ->
			A = Literal 'A'

			assert.is_nil A\at 2
			assert.is_nil A\at 3
			assert.is_nil A\at 4

