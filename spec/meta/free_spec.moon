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
		A = Literal 'A'
		B = Literal 'B'
		C = Literal 'C'

		it 'works for operations', ->
			var = Free!
			op = Operation 'call', var, B

			var\reify op, op
			assert.equal op, op.domain

		it 'works for tuples without var arg', ->
			var = Free!
			tup = Tuple { var, B }

			var\reify tup, tup
			assert.equal tup, tup.types[1]

		it 'works for tuples with var arg', ->
			var = Free!
			tup = Tuple { A, B }, var

			var\reify tup, tup
			assert.equal tup, tup.var_arg

		it 'works for unions', ->
			var = Free!
			union = Union { var, B }

			var\reify union, union
			assert.equal union, union.types[1]

		it 'works for 2-cycle unions', ->
			var1 = Free!
			var2 = Free!

			union1 = Union { var2, A }
			union2 = Union { var1, B }

			var2\reify union1, union2
			var1\reify union2, union1

			assert.equal union2, union1.types[1]
			assert.equal union1, union2.types[1]

		it 'works for intersections', ->
			var = Free!
			inter = Intersection { var, A }

			var\reify inter, inter
			assert.equal inter, inter.types[1]

		it 'works for literals', ->
			var = Free!
			lit = Literal 'value', var

			var\reify lit, lit
			assert.equal lit, lit.ops
			
		it 'works for unknown', ->
			var = Free!
			
			assert.no_error -> var\reify Unknown, Unknown

		it 'works for never', ->
			var = Free!

			assert.no_error -> var\reify Never, Never

		it 'works for free', ->
			var = Free!
			other_var = Free!
			other_var.value = var

			var\reify other_var, other_var
			assert.equal other_var, other_var.value

		it 'defaults replacement arg to self.value if nil', ->
			var = Free!
			op = Operation 'call', var, B

			assert.error -> var\reify op

			var.value = op
			var\reify op

			assert.equal op, op.domain
