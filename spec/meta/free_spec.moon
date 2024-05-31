import Free, Type from require 'type-track.meta'

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
