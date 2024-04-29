import Type, Operator, Tuple, Literal from require 'type-track.meta'

describe 'callable', ->
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'
	True = Literal true

	never = Tuple {}
	AB = Tuple { A, B }
	ABC = Tuple { A, B, C } -- ABC\is_subset AB

	it 'is a Type', ->
		func = Operator never, never

		assert.is_true func\is_instance Type

	describe 'is_subset', ->
		it 'accepts an operator with a subset of args', ->
			func2 = Operator 'call', AB, never
			func3 = Operator 'call', ABC, never
			-- if func2 is called with args ABC, it would discard the C and unify

			assert.is_true Operator.is_subset func3, func2
			assert.is_false Operator.is_subset func2, func3

		it 'accepts an operator with a superset of returns', ->
			func2 = Operator 'call', never, AB
			func3 = Operator 'call', never, ABC
			-- if the call site only expects AB, then it doesn't matter whether AB or ABC is returned

			assert.is_true Operator.is_subset func2, func3
			assert.is_false Operator.is_subset func3, func2

		it 'accepts an operator with a subset of args and a superset of returns', ->
			func23 = Operator 'call', AB, ABC
			func32 = Operator 'call', ABC, AB

			assert.is_true Operator.is_subset func32, func23
			assert.is_false Operator.is_subset func23, func32

	describe 'eval', ->
		it 'gives nil when given no arguments', ->
			func = Operator 'call', never, ABC

			assert._nil func\eval!

		it 'gives a return type when given a compatible param type', ->
			func = Operator 'call', AB, ABC

			assert.equal ABC, func\eval 'call', AB
			assert.equal ABC, func\eval 'call', ABC

		it 'gives nil when given an incompatible param type', ->
			func = Operator 'call', AB, ABC

			assert.is_nil func\eval 'call', never
			assert.is_nil func\eval 'call', A
			assert.is_nil func\eval 'call', Tuple { A }
