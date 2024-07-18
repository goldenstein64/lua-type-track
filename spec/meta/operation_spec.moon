import
	is_subset
	Type, Operation, Tuple, Literal, Never, Unknown
from require 'type-track.meta'

describe 'Operation', ->
	local A, B, C, D, AB, ABC
	lazy_setup ->
		A = Literal 'A'
		B = Literal 'B'
		C = Literal 'C'
		D = Literal 'D'

		AB = Tuple { A, B }
		ABC = Tuple { A, B, C }

		assert.is_true is_subset ABC, AB

	it 'is a Type', ->
		func = Operation 'call', Never, Never

		assert.is_true func\is_instance Type

	describe 'is_subset', ->
		it 'accepts (...A) -> () <: (A, A, A) -> () but not converse', ->
			-- long: (A, A, A) -> ()
			-- var: (...A) -> ()
			--
			-- var = long -- if this was accepted, then
			-- var(A) -- this would be acceptable, but it's not
			long_tup = Operation 'call', (Tuple { A, A, A }), Never
			var_tup = Operation 'call', (Tuple {}, A), Never

			assert.is_true Operation.is_subset var_tup, long_tup
			assert.is_false Operation.is_subset long_tup, var_tup

		it 'accepts (A, B) -> () <: (A, B, C) -> () but not converse', ->
			-- funcAB: (A, B) -> ()
			-- funcABC: (A, B, C) -> ()
			--
			-- funcAB = funcABC -- if this was accepted
			-- funcAB(A, B) -- this would be acceptable, but it's not
			funcAB = Operation 'call', AB, Never
			funcABC = Operation 'call', ABC, Never

			assert.is_true is_subset ABC, AB
			assert.is_true Operation.is_subset funcAB, funcABC
			assert.is_false Operation.is_subset funcABC, funcAB

		it 'accepts () -> (A, B, C) <: () -> (A, B) but not converse', ->
			-- funcAB: () -> (A, B)
			-- funcABC: () -> (A, B, C)
			--
			-- funcABC = funcAB -- if this was accepted,
			-- A, B, C = funcABC() -- this would be acceptable, but it's not
			funcAB = Operation 'call', Never, AB
			funcABC = Operation 'call', Never, ABC

			assert.is_true is_subset ABC, AB
			assert.is_true Operation.is_subset funcABC, funcAB
			assert.is_false Operation.is_subset funcAB, funcABC

		it 'accepts (A, B) -> (A, B, C) <: (A, B, C) -> (A, B) but not converse', ->
			func23 = Operation 'call', AB, ABC
			func32 = Operation 'call', ABC, AB

			assert.is_true Operation.is_subset func23, func32
			assert.is_false Operation.is_subset func32, func23

	describe 'is_overlapping', ->
		it 'accepts () -> (B) ~:~ () -> (B)', ->
			funcB1 = Operation 'call', Tuple.Unit, B
			funcB2 = Operation 'call', Tuple.Unit, B

			assert.is_true Operation.is_overlapping funcB1, funcB2

		it 'accepts (A) -> () ~:~ (A) -> ()', ->
			funcA1 = Operation 'call', A, Tuple.Unit
			funcA2 = Operation 'call', A, Tuple.Unit

			assert.is_true Operation.is_overlapping funcA1, funcA2

		it 'accepts (A) -> () ~:~ (B) -> ()', ->
			funcA = Operation 'call', A, Tuple.Unit
			funcB = Operation 'call', B, Tuple.Unit

			assert.is_true Operation.is_overlapping funcA, funcB

		it 'rejects () -> (A) ~:~ () -> (B)', ->
			funcA = Operation 'call', Tuple.Unit, A
			funcB = Operation 'call', Tuple.Unit, B

			assert.is_false Operation.is_overlapping funcA, funcB

		it 'accepts (B) -> (A) ~:~ (A) -> (B)', ->
			funcBA = Operation 'call', B, A
			funcAB = Operation 'call', A, B

			assert.is_true Operation.is_overlapping funcBA, funcAB

		it 'rejects overlapping domain and disjoint range', ->
			funcABA = Operation 'call', A + B, A
			funcBCB = Operation 'call', B + C, B

			assert.is_false Operation.is_overlapping funcABA, funcBCB

		it 'accepts disjoint domain and disjoint range', ->
			funcAB = Operation 'call', A, B
			funcCD = Operation 'call', C, D

			assert.is_true Operation.is_overlapping funcAB, funcCD

		it 'accepts different operators', ->
			funcBA = Operation 'call', B, A
			funcAB = Operation 'index', A, B

			assert.is_true Operation.is_overlapping funcBA, funcAB

	describe 'get_domain', ->
		it 'returns nil for the wrong op', ->
			func = Operation 'call', Never, ABC

			assert.is_nil func\get_domain 'index'

		it 'returns a domain type when given a compatible op', ->
			func = Operation 'call', AB, ABC

			assert.equal AB, func\get_domain 'call'

	describe 'eval', ->

		it 'returns nil for the wrong op', ->
			func = Operation 'call', Never, ABC

			assert.is_nil func\eval 'index'

		it 'returns a type when given a compatible domain type', ->
			func = Operation 'call', AB, ABC

			assert.equal ABC, func\eval 'call', AB
			assert.equal ABC, func\eval 'call', ABC

		it 'returns nil when given an incompatible domain type', ->
			func = Operation 'call', AB, ABC

			assert.is_nil func\eval 'call', Unknown
			assert.is_nil func\eval 'call', A
			assert.is_nil func\eval 'call', Tuple { A }
