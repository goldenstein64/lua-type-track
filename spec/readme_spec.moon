import Tuple, Never, Operation, is_subset from require 'type-track.meta'

lua51 = require 'type-track.lua51'
{ :number, string: _string } = require 'type-track.lua51'
import map_of, string_of from lua51.define

describe 'README example', ->
	it 'works', ->
		Foo = Operation 'call', Tuple.Unit, (string_of 'foo')
		Foo.debug_name = "Foo"
		FooSub = Foo * Operation 'index', number, _string
		FooSub.debug_name = "FooSub"

		Foo = assert Foo\normalize!
		FooSub = assert FooSub\normalize!

		assert.is_true is_subset FooSub, Foo
		assert.is_false is_subset Foo, FooSub

		map_foo = map_of _string, Foo
		map_foo_sub = map_of _string, FooSub

		map_foo = assert map_foo\normalize!
		map_foo_sub = assert map_foo_sub\normalize!

		assert.is_false is_subset map_foo_sub, map_foo
		assert.is_false is_subset map_foo, map_foo_sub
