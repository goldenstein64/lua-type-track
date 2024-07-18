import
	is_subset
	Type, Tuple, Literal, Unknown, Never
from require 'type-track.meta'

is_equiv = (a, b) -> (is_subset a, b) and (is_subset b, a)

describe 'Tuple', ->
	it 'is a Type', ->
		tup = Tuple {}
		assert.is_true tup\is_instance Type

	local A, B, C, Nil
	local D, E, F
	lazy_setup ->
		-- local A, B, C = "A", "B", "C"
		A = Literal 'A'
		B = Literal 'B'
		C = Literal 'C'
		D = Literal 'D'
		E = Literal 'E'
		F = Literal 'F'
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

	describe 'is_overlapping', ->
		it 'accepts tuples where each element overlaps', ->
			tup1 = Tuple { A + B, D + E }
			tup2 = Tuple { B + C, E + F }

			assert.is_true Tuple.is_overlapping tup1, tup2

		it 'rejects tuples where one element is disjoint', ->
			tup1 = Tuple { A, B }
			tup2 = Tuple { A, C }

			assert.is_false Tuple.is_overlapping tup1, tup2

		it 'accepts different length tuples', ->
			tup1 = Tuple { A, B }
			tup2 = Tuple { A, B, C }

			assert.is_true Tuple.is_overlapping tup1, tup2

		it 'accepts tuples where shorter has overlapping vars', ->
			tup1 = Tuple { A, B }, C
			tup2 = Tuple { A, B, C + E, C + D }

			assert.is_true Tuple.is_overlapping tup1, tup2

		it 'rejects tuples where shorter has disjoint vars', ->
			tup1 = Tuple { A, B }, C
			tup2 = Tuple { A, B, C, D }

			assert.is_false Tuple.is_overlapping tup1, tup2

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

	describe 'normalize', ->
		it 'returns Tuple.Unit when empty', ->
			tup = Tuple {}

			assert.equal Tuple.Unit, tup\normalize!

		it 'returns its single element if no var arg', ->
			tup = Tuple { A }

			normalized = tup\normalize!
			assert.equal "Literal", normalized.__class.__name
			assert.equal "A", normalized.value

		it 'still returns a tuple if var arg', ->
			tup = Tuple { A }, B

			normalized = tup\normalize!
			assert.equal "Tuple", normalized.__class.__name
			assert.equal "A", normalized\at(1).value
			assert.equal "B", normalized.var.value

		it 'truncates middle tuples', ->
			tup = Tuple { A, (Tuple { A, B }), C }

			normalized = tup\normalize!
			assert.equal "A", normalized\at(1).value
			assert.equal "A", normalized\at(2).value
			assert.equal "C", normalized\at(3).value

		it 'flattens the last tuple', ->
			tup = Tuple { A, B, (Tuple { A, B }, C) }

			normalized = tup\normalize!
			assert.equal "A", normalized\at(1).value
			assert.equal "B", normalized\at(2).value
			assert.equal "A", normalized\at(3).value
			assert.equal "B", normalized\at(4).value
			assert.equal "C", normalized.var.value

		it 'rejects conflicting var on last tuple', ->
			tup = Tuple { A, (Tuple {}, B) }, C

			assert.is_nil tup\normalize!

		it 'accepts subset var on last tuple', ->
			tup = Tuple { A, (Tuple {}, B) }, B + C

			normalized = tup\normalize!
			normalized_var = normalized.var
			expected_var = B + C
			assert.equal "A", tup\at(1).value
			assert.is_true is_equiv expected_var, normalized_var
