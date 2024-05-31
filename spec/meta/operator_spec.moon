import
	is_subset
	Type, Operation, Tuple, Literal, Never, Unknown
from require 'type-track.meta'

describe 'Operation', ->
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'

	AB = Tuple { A, B }
	ABC = Tuple { A, B, C }
	assert.is_true is_subset ABC, AB

	it 'is a Type', ->
		func = Operation Never, Never

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

	describe 'eval', ->
		it 'gives nil when given no arguments', ->
			func = Operation 'call', Never, ABC

			assert._nil func\eval!

		it 'gives a return type when given a compatible param type', ->
			func = Operation 'call', AB, ABC

			assert.equal ABC, func\eval 'call', AB
			assert.equal ABC, func\eval 'call', ABC

		it 'gives nil when given an incompatible param type', ->
			func = Operation 'call', AB, ABC

			assert.is_nil func\eval 'call', Unknown
			assert.is_nil func\eval 'call', A
			assert.is_nil func\eval 'call', Tuple { A }
