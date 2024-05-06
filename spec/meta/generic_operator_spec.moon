import Type, Tuple, GenericOperator, Literal from require 'type-track.meta'

describe 'GenericOperator', ->
	unit = Tuple {}
	A = Literal 'A'

	it 'is a Type', ->
		gen_op = GenericOperator 'call', (->), (->)

		assert.is_true gen_op\is_instance Type

	describe 'is_subset', ->
		it 'works for simple types', ->
			type1 = GenericOperator(
				'call'
				(type_params) ->
					T = type_params\at 1
					T, T

				(params) -> params\at 1
			)

			type2 = GenericOperator(
				'call'
				(type_params) ->
					U = type_params\at 1
					U, U

				(params) -> params\at 1
			)

			assert.is_true GenericOperator.is_subset type1, type2
			assert.is_true GenericOperator.is_subset type2, type1

		it 'works for union returns', ->
			type1 = GenericOperator(
				"call"
				(type_params) ->
					T = type_params\at 1
					T, T + A

				(params) -> params\at 1
			)

			type2 = GenericOperator(
				"call"
				(type_params) ->
					U = type_params\at 1
					U, U + A

				(params) -> params\at 1
			)

			assert.is_true GenericOperator.is_subset type1, type2
			assert.is_true GenericOperator.is_subset type2, type1

