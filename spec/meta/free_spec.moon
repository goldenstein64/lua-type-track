import
	Free, Type, Operation, Literal
	Union, Intersection, Never, Unknown
	Tuple
from require 'type-track.meta'

describe 'Free', ->
	it 'is a Type', ->
		free = Free!

		assert.is_true free\is_instance Type

	describe 'unwrap', ->
		it "errors if it's empty", ->
			free = Free!
			free.value = nil

			assert.error -> free\unwrap!

		it "errors if it's cyclic", ->
			free = Free!
			free.value = free

			assert.error -> free\unwrap!

	describe 'reify', ->
		local A, B, C
		lazy_setup ->
			A = Literal 'A'
			B = Literal 'B'
			C = Literal 'C'

		it 'works for operations', ->
			var = Free!
			op = Operation 'call', var, B

			var\reify op
			assert.equal op, op.domain

		it 'works for tuples without var arg', ->
			var = Free!
			tup = Tuple { var, B }

			var\reify tup
			assert.equal tup, tup.elements[1]

		it 'works for tuples with var arg', ->
			var = Free!
			tup = Tuple { A, B }, var

			var\reify tup
			assert.equal tup, tup.var

		it 'works for unions', ->
			var = Free!
			union = Union { var, B }

			var\reify union
			assert.equal union, union.types[1]

		it 'works for 2-cycle unions', ->
			var1 = Free!
			var2 = Free!

			union1 = Union { var2, A }
			union2 = Union { var1, B }

			var1\reify union1
			var2\reify union2

			assert.equal union2, union1.types[1]
			assert.equal union1, union2.types[1]

		it 'works for intersections', ->
			var = Free!
			inter = Intersection { var, A }

			var\reify inter, inter
			assert.equal inter, inter.types[1]

		it 'works for 2-cycle intersections', ->
			var1 = Free!
			var2 = Free!
			inter1 = Intersection { var2, B }
			inter2 = Intersection { var1, B }

			var1\reify inter1
			var2\reify inter2

			assert.equal inter1, inter2.types[1]
			assert.equal inter2, inter1.types[1]

		it 'works for literals', ->
			var = Free!
			lit = Literal 'value', var

			var\reify lit
			assert.equal lit, lit.of

		it 'works for unknown', ->
			var = Free!

			assert.no_error -> var\reify Unknown, Unknown

		it 'works for never', ->
			var = Free!

			assert.no_error -> var\reify Never, Never
