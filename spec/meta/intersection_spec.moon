import
	is_subset
	Type, Tuple, Literal, Intersection, Never, Operation, Free
	Union
from require 'type-track.meta'

memoize = (f) ->
	cache = {}
	(a, ...) ->
		r = cache[a]
		if r == nil
			r = f a, ...
			cache[a] = r

		r

describe 'Intersection', ->
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'

	it 'is a type', ->
		inter = Intersection { A, B }

		assert.is_true inter\is_instance Type

	describe 'normalize', ->
		it 'works with Never', ->
			inter = Intersection { Never, A }

			assert.equal Never, inter\normalize!

		it 'simplifies nested intersections', ->
			inter = Intersection { A, Intersection { B, C } }
			unified = inter\normalize!
			for t in *unified.types
				assert.is_false t\is_instance Intersection

		it 'simplifies nested intersections with duplicates', ->
			inter = Intersection { A, Intersection { A, B } }

			unified = inter\normalize!

			assert.equal 2, #unified.types
			{ elem1, elem2 } = unified.types
			assert.is_true elem1.value == "A" or elem1.value == "B"
			if elem1.value == "A"
				assert.equal "B", elem2.value
			elseif elem1.value == "B"
				assert.equal "A", elem2.value

		it 'simplifies subset operators', ->
			op1 = Operation 'call', A, C
			op2 = Operation 'call', (Tuple { A, B }), C

			-- assuming op1 is a subset of op2,
			assert.is_true is_subset op1, op2

			inter = Intersection { op1, op2 }
			unified = inter\normalize!

			-- normalizeing should return the subset, i.e. op1
			assert.is_true is_subset op1, unified
			assert.is_true is_subset unified, op1

		it 'works with order-1 cyclic intersections', ->
			op = Operation 'call', A, B

			inter = Free!
			inter.value = Intersection { op, op, inter }

			expected = op

			unified = inter\normalize!
			assert.not_nil unified
			assert.is_true is_subset unified, expected
			assert.is_true is_subset expected, unified

		it 'returns nil for order-2 cyclic intersections', ->
			_string = Free!
			number = Free!

			number.value = Operation "add", number, number
			number.value.unified = number.value -- already unified

			string_of = memoize (value) -> Literal value, _string
			concat_call = Operation "concat", _string + number, _string

			_string.value = Intersection {
				Operation "type", Never, string_of "string"
				concat_call
			}

			assert.is_nil concat_call\normalize!
			assert.is_nil _string\normalize!

	describe 'get_domain', ->
		it 'returns nil for a wrong op', ->
			inter = Intersection {
				Operation 'call', A, B
				Operation 'call', B, C
			}

			assert.is_nil inter\get_domain 'index'

		it 'returns a union of compatible types', ->
			inter = Intersection {
				Operation 'call', A, B
				Operation 'call', B, C
			}

			domain = inter\get_domain 'call'
			target = Union { A, B }

			assert.is_true is_subset domain, target
			assert.is_true is_subset target, domain

	describe 'eval', ->
		it 'returns nil for a wrong op', ->
			inter = Intersection {
				Operation 'call', A, B
				Operation 'call', B, C
			}

			assert.is_nil inter\eval 'index', Never

		it 'returns nil for wrong domain', ->
			inter = Intersection {
				Operation 'call', A, B
				Operation 'call', B, C
			}

			assert.is_nil inter\eval 'call', C
		
		it 'returns an intersection of compatible types', ->
			inter = Intersection {
				Operation 'call', A, B
				Operation 'call', A, C
			}
			range = inter\eval 'call', A
			target = Intersection { B, C }

			assert.is_true is_subset range, target
			assert.is_true is_subset target, range
