rockspec_format = "3.0"
package = "type-track"
version = "dev-1"
source = {
	url = "https://github.com/goldenstein64/lua-type-track",
}
description = {
	homepage = "https://github.com/goldenstein64/lua-type-track",
	license = "MIT",
}
dependencies = {
	"lua >= 5.1",
}
test_dependencies = {
	"busted ~> 2.2",
	"moonscript ~> 0.5",
}
build = {
	type = "builtin",
	modules = {},
}
