import Literal, Intersection, Type from require 'type-track.meta'

describe 'intersection', ->
	A = Literal 'A'
	B = Literal 'B'

	it 'is a type', ->
		intersection = Intersection { A, B }

		assert.is_true intersection\is_instance Type
