import Tuple, Literal from require 'type-track.meta_types'

describe 'Tuple', ->
	describe 'is_subset', ->
		-- local A, B, C = "A", "B", "C"
		A = Literal 'A'
		B = Literal 'B'
		C = Literal 'C'

		it 'accepts shorter tuples #only', ->
			short_tup = Tuple { A, B }
			long_tup = Tuple { A, B, C }

			assert.is_true long_tup\is_subset short_tup

		it 'rejects longer tuples', ->
			short_tup = Tuple { A, B }
			long_tup = Tuple { A, B, C }

			assert.is_false short_tup\is_subset long_tup

		it 'rejects crossed tuples', ->
			tup1 = Tuple { A, B }
			tup2 = Tuple { B, A }

			assert.is_false tup1\is_subset tup2
			assert.is_false tup2\is_subset tup1

		it 'compares its first element to a non-tuple', ->
			tupA = Tuple { A }
			tupB = Tuple { B }
			assert.is_true tupA\is_subset A
			assert.is_false tupB\is_subset A
