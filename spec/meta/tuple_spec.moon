import Type, Tuple, Literal from require 'type-track.meta'

describe 'Tuple', ->
	it 'is a Type', ->
		tup = Tuple {}
		assert.is_true tup\is_instance Type

	-- local A, B, C = "A", "B", "C"
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'
	Nil = Literal nil

	describe 'without default_var_arg', ->
		setup -> Tuple.default_var_arg = nil
		teardown -> Tuple.default_var_arg = nil

		describe 'is_subset', ->
			it 'accepts shorter tuples', ->
				short_tup = Tuple { A, B }
				long_tup = Tuple { A, B, C }

				assert.is_true Tuple.is_subset long_tup, short_tup

			it 'rejects longer tuples', ->
				short_tup = Tuple { A, B }
				long_tup = Tuple { A, B, C }

				assert.is_false Tuple.is_subset short_tup, long_tup

			it 'rejects crossed tuples', ->
				tup1 = Tuple { A, B }
				tup2 = Tuple { B, A }

				assert.is_false Tuple.is_subset tup1, tup2
				assert.is_false Tuple.is_subset tup2, tup1

			-- shouldn't this be accepted?
			it 'rejects var-args if it doesn\'t have one', ->
				long_tup = Tuple { A, A, A }
				var_tup = Tuple {}, A

				assert.is_false Tuple.is_subset long_tup, var_tup

			it 'rejects var-args if they are not subsets', ->
				var_tup_A = Tuple {}, A
				var_tup_B = Tuple {}, B

				assert.is_false Tuple.is_subset var_tup_A, var_tup_B

			it 'accepts var-args if they are subsets', ->
				var_tup1 = Tuple { A }, A
				var_tup2 = Tuple {}, A

				assert.is_true Tuple.is_subset var_tup1, var_tup2

			it 'rejects var-args if latter non-var-args are not subsets', ->
				long_tup = Tuple { A, B }, A
				short_tup = Tuple { A }, A

				assert.is_false Tuple.is_subset long_tup, short_tup

		describe 'at', ->
			it 'returns its types by index', ->
				tup = Tuple { A, B, C }

				assert.equal A, tup\at 1
				assert.equal B, tup\at 2
				assert.equal C, tup\at 3

			it 'returns var-args after its length is exceeded', ->
				tup = Tuple { A }, B

				assert.equal A, tup\at 1
				assert.equal B, tup\at 2
				assert.equal B, tup\at 3
				assert.equal B, tup\at 4

			it 'returns nil if var-args is nil', ->
				tup = Tuple { A, B }

				assert.equal A, tup\at 1
				assert.equal B, tup\at 2
				assert.is_nil tup\at 3

	describe 'with default_var_arg', ->
		setup -> Tuple.default_var_arg = Nil
		teardown -> Tuple.default_var_arg = nil
