import Type, Tuple, GenericOperation, Literal from require 'type-track.meta'

describe 'GenericOperation', ->
	local A
	lazy_setup ->
		A = Literal 'A'

	it 'is a Type', ->
		gen_op = GenericOperation 'call', (->), (->)

		assert.is_true gen_op\is_instance Type

	describe 'is_subset', ->
		it 'works for simple types', ->
			type1 = GenericOperation(
				'call'
				(type_params) ->
					T = type_params\at 1
					T, T

				(domain) -> domain\at 1
			)

			type2 = GenericOperation(
				'call'
				(type_params) ->
					U = type_params\at 1
					U, U

				(domain) -> domain\at 1
			)

			assert.is_true GenericOperation.is_subset type1, type2
			assert.is_true GenericOperation.is_subset type2, type1

		it 'works for union returns', ->
			type1 = GenericOperation(
				"call"
				(type_params) ->
					T = type_params\at 1
					T, T + A

				(domain) -> domain\at 1
			)

			type2 = GenericOperation(
				"call"
				(type_params) ->
					U = type_params\at 1
					U, U + A

				(domain) -> domain\at 1
			)

			assert.is_true GenericOperation.is_subset type1, type2
			assert.is_true GenericOperation.is_subset type2, type1
