import
	Type, Tuple, Literal, Operator, Intersection
	is_subset, Never
from require 'type-track.meta'

describe 'Tuple', ->
	it 'is a Type', ->
		tup = Tuple {}
		assert.is_true tup\is_instance Type

	-- local A, B, C = "A", "B", "C"
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'
	Nil = Literal nil

	describe 'is_subset', ->
		it 'accepts shorter tuples but not converse', ->
			-- the latter elements get discarded
			short_tup = Tuple { A, B }
			long_tup = Tuple { A, B, C }

			assert.is_true Tuple.is_subset long_tup, short_tup
			assert.is_false Tuple.is_subset short_tup, long_tup

		it 'rejects crossed tuples', ->
			tup1 = Tuple { A, B }
			tup2 = Tuple { B, A }

			assert.is_false Tuple.is_subset tup1, tup2
			assert.is_false Tuple.is_subset tup2, tup1

		it 'accepts any var-args if both sets have equal element count', ->
			long_tup = Tuple { A, A, A }
			long_var_tup = Tuple { A, A, A }, A
			long_var_unknown_tup = Tuple { A, A, A }, Unknown

			assert.is_true Tuple.is_subset long_tup, long_var_tup
			assert.is_true Tuple.is_subset long_var_tup, long_tup

			-- ...Unknown is discarded here
			assert.is_true Tuple.is_subset long_var_unknown_tup, long_tup

			-- ...Unknown is empty here
			assert.is_true Tuple.is_subset long_tup, long_var_unknown_tup

		it 'rejects subsets with var-args and supersets without var-args', ->
			-- var: (...A)
			-- long: (A, A, A)
			--
			-- long = var -- if this was accepted, then
			-- long = (A) -- this would be acceptable (because var accepts it), but it's not
			-- var = long -- however, the converse holds true
			var_tup = Tuple {}, A
			long_tup = Tuple { A, A, A }

			assert.is_true Tuple.is_subset long_tup, var_tup
			assert.is_false Tuple.is_subset var_tup, long_tup

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

		it 'returns nil outside of range', ->
			tup = Tuple { A, B }

			assert.equal A, tup\at 1
			assert.equal B, tup\at 2
			assert.equal nil, tup\at 3
