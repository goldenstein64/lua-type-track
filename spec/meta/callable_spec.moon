import Type, Callable, Tuple, Literal from require 'type-track.meta'

describe 'callable', ->
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'
	True = Literal true

	never = Tuple {}
	AB = Tuple { A, B }
	ABC = Tuple { A, B, C } -- ABC\is_subset AB

	it 'is a Type', ->
		func = Callable never, never

		assert.is_true func\is_instance Type

	describe 'is_subset', ->
		it 'can accept tuples by comparing its first element', ->
			func = Callable never, never
			assert.is_true func\is_subset Tuple { func }

		it 'accepts a callable with a subset of args', ->
			func2 = Callable AB, never
			func3 = Callable ABC, never
			-- if func2 is called with args ABC, it would discard the C and unify

			assert.is_true func3\is_subset func2
			assert.is_false func2\is_subset func3

		it 'accepts a callable with a superset of returns', ->
			func2 = Callable never, AB
			func3 = Callable never, ABC
			-- if the call site only expects AB, then it doesn't matter whether AB or ABC is returned

			assert.is_true func2\is_subset func3
			assert.is_false func3\is_subset func2

		it 'accepts a callable with a subset of args and a superset of returns', ->
			func23 = Callable AB, ABC
			func32 = Callable ABC, AB

			assert.is_true func32\is_subset func23
			assert.is_false func23\is_subset func32

	describe 'call', ->
		it 'gives a return type when given no arguments', ->
			func = Callable never, ABC

			assert.equal ABC, func\call!

		it 'gives nil when given an incompatible param type', ->
			func = Callable AB, ABC

			assert.is_nil func\call never
			assert.is_nil func\call A
			assert.is_nil func\call Tuple { A }
