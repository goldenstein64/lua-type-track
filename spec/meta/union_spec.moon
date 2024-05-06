import Literal, Union, Type, Unknown from require 'type-track.meta'

describe 'Union', ->
	A = Literal 'A'
	B = Literal 'B'
	C = Literal 'C'

	it 'is a Type', ->
		union = Union { A, B }

		assert.is_true union\is_instance Type

	describe 'unify', ->
		it 'works with Unknown', ->
			union = Union { A, Unknown }

			assert.equal Unknown, union\unify!

		it 'simplifies nested unions', ->
			union = Union { A, Union { B, C } }
			unified = union\unify!

			for t in *unified.types
				assert.is_false t\is_instance Union
