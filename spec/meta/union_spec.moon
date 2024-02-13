import Literal, Union, Type from require 'type-track.meta'

describe 'union', ->
	A = Literal 'A'
	B = Literal 'B'

	it 'is a Type', ->
		union = Union { A, B }

		assert.is_true union\is_instance Type
