import Literal, Object, Type from require 'type-track.meta'

describe 'object', ->
	it 'is a Type', ->
		obj = Object!

		assert.is_true obj\is_instance Type
