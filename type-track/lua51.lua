local meta_types = require("type-track.meta")

local Tuple, Operator, Union, Intersection, Literal =
	meta_types.Tuple, meta_types.Operator, meta_types.Union, meta_types.Intersection, meta_types.Literal

local LazyRef = meta_types.LazyRef

local T = Tuple
local never = T({})

local unknown = LazyRef()
local number = LazyRef()
local _string = LazyRef()
local _table = LazyRef()
local _function = LazyRef()
local stringlib = LazyRef()

local string_or_num = _string + number
local concat_call = Operator("concat", string_or_num, _string)
local unknown_var = T({}, unknown)

local boolean_str = Literal("boolean", _string)
local function_type = Operator("type", never, Literal("function", _string))

local boolean = Operator("type", never, boolean_str)
local _false = LazyRef()

local _true = Literal(true, boolean)
_false.value = Literal(
	false,
	Intersection({
		boolean,
		Operator("truthy", never, _false),
	})
)

local _nil = Intersection({
	Operator("type", never, Literal("nil", _string)),
	Operator("truthy", never, _false),
})

_string.value = Intersection({
	Operator("type", never, Literal("string", _string)),
	concat_call,
	stringlib,
})

number.value = Intersection({
	Operator("type", never, Literal("number", _string)),
	Operator("add", number, number),
	Operator("sub", number, number),
	Operator("mul", number, number),
	Operator("div", number, number),
	Operator("mod", number, number),
	Operator("pow", number, number),
	concat_call,
})

_table.value = Intersection({
	Operator("type", never, Literal("table", _string)),
	Operator("index", unknown, unknown),
	Operator("newindex", T({ unknown, unknown }), never),
	Operator("len", never, number),
})

_function.value = Intersection({
	function_type,
	Operator("call", unknown_var, unknown_var),
})

local thread = Operator("type", never, Literal("thread", _string))
local userdata = Operator("type", never, Literal("userdata", _string))

---@generic F
---@param f F
---@return F
local function memoize(f)
	local cache = {}
	return function(a, ...)
		if cache[a] ~= nil then
			return cache[a]
		else
			local r = f(a, ...)
			cache[a] = r
			return r
		end
	end
end

---@param value string
---@return type-track.Literal
local function string_of(value)
	return Literal(value, _string)
end
string_of = memoize(string_of)

---@param params type-track.Type
---@param returns type-track.Type
---@return type-track.Type func
local function func(params, returns)
	return Intersection({
		function_type,
		Operator("call", params, returns),
	})
end

local num2_to_num = func(T({ number, number }), number)

local num_or_nil = _nil + number
local num_var = T({}, number)
local string_or_num_var = T({}, string_or_num)

---@param t type-track.Type
local function array_of(t)
	return Intersection({
		Operator("index", number, t),
		Operator("newindex", T({ number, t }), never),
	})
end

---@param tab { [string]: type-track.Type }
---@return type-track.Type
local function lib(tab)
	local entries = { Operator("newindex", T({ unknown, unknown }), never) }
	for k, v in pairs(tab) do
		table.insert(entries, Operator("index", string_of(k), v))
	end

	return Intersection(entries)
end

local string_or_num_or_nil = _string + number + _nil

-- stringlib
stringlib.value = lib({
	byte = func(T({ string_or_num, num_or_nil, num_or_nil }), num_var),
	char = func(string_or_num_var, _string),
	dump = func(_function, _string),
	find = func(
		T({ string_or_num, string_or_num, string_or_num_or_nil, unknown }),
		(_nil + T({ number, number }, string_or_num))
	),
	format = func(T({ string_or_num }, unknown), _string),
	gmatch = func(T({ string_or_num, string_or_num }), func(never, string_or_num_var)),
	gsub = func(
		T({
			string_or_num,
			string_or_num,
			number + _string + _table + _function,
			num_or_nil,
		}),
		T({ _string, number })
	),
	len = func(string_or_num, number),
	lower = func(string_or_num, _string),
	match = func(T({ string_or_num, string_or_num, num_or_nil }), T({}, string_or_num_or_nil)),
	rep = func(T({ string_or_num, number }), _string),
	reverse = func(string_or_num, _string),
	sub = func(T({ string_or_num, number, num_or_nil }), _string),
	upper = func(string_or_num, _string),
})

local tablelib = lib({
	concat = func(T({ array_of(_string), string_or_num_or_nil, num_or_nil, num_or_nil }), _string),
	insert = Intersection({
		func(T({ _table, unknown }), never),
		func(T({ _table, number, unknown }), never),
	}),
	maxn = func(_table, number),
	remove = func(T({ _table, num_or_nil }), unknown),
	sort = func(T({ _table, _function }), never),
})

local num_to_num = func(number, number)
local num_to_num2 = func(number, T({ number, number }))
local num1var = T({ number }, number)
local num1var_to_num = func(num1var, number)

local mathlib = lib({
	abs = num_to_num,
	acos = num_to_num,
	asin = num_to_num,
	atan = num_to_num,
	atan2 = num2_to_num,
	ceil = num_to_num,
	cos = num_to_num,
	cosh = num_to_num,
	deg = num_to_num,
	exp = num_to_num,
	floor = num_to_num,
	fmod = num2_to_num,
	frexp = num_to_num2,
	huge = number,
	ldexp = num2_to_num,
	log = num_to_num,
	log10 = num_to_num,
	max = num1var_to_num,
	min = num1var_to_num,
	modf = num_to_num2,
	pi = number,
	pow = num2_to_num,
	rad = num_to_num,
	random = func(never, number) * num_to_num * num2_to_num,
	randomseed = func(number, never),
	sin = num_to_num,
	sinh = num_to_num,
	sqrt = num_to_num,
	tan = num_to_num,
	tanh = num_to_num,
})

local file_type = string_of("file") + string_of("closed file") + _nil

local file = LazyRef()
local fail = T({ _nil, _string })

local true_or_fail = _true + fail
local file_or_fail = file + fail
local num_or_fail = number + fail

local file_open_mode = string_of("r")
	+ string_of("w")
	+ string_of("a")
	+ string_of("r+")
	+ string_of("w+")
	+ string_of("a+")
	+ string_of("rb")
	+ string_of("wb")
	+ string_of("ab")
	+ string_of("r+b")
	+ string_of("w+b")
	+ string_of("a+b")

local file_read_mode = string_of("*n")
	+ string_of("*a")
	+ string_of("*l")
	+ string_of("*L")
	+ string_of("n")
	+ string_of("a")
	+ string_of("l")
	+ string_of("L")

local file_read_modevar = T({}, file_read_mode)

local file_seek_mode = string_of("set") + string_of("cur") + string_of("end")

local file_seek_mode_or_nil = file_seek_mode + _nil

local vbuf_mode = string_of("no") + string_of("full") + string_of("line")

local implicit_file = func(never, file_or_fail) * func(file, true_or_fail)
local open_file = func(T({ string_or_num, file_open_mode + _nil }), file_or_fail)

local iolib = lib({
	close = func(file, true_or_fail) * func(never, true_or_fail),
	flush = func(never, true_or_fail),
	input = implicit_file,
	lines = func(never, func(never, _string)) * func(string_or_num, func(never, _string)),
	open = open_file,
	output = implicit_file,
	popen = open_file,
	read = func(file_read_modevar, string_or_num_var),
	tmpfile = func(never, file_or_fail),
	type = func(unknown, file_type),
	write = func(string_or_num_var, file_or_fail),
})

file.value = Intersection({
	userdata,
	Operator("iolib.type", never, string_of("file") + string_of("closed file")),
	lib({
		close = func(file, true_or_fail),
		flush = func(file, true_or_fail),
		lines = func(file, func(never, _string)),
		read = func(T({ file }, file_read_mode), string_or_num_var),
		seek = func(T({ file, file_seek_mode_or_nil, num_or_nil }), num_or_fail),
		setvbuf = func(T({ file, vbuf_mode, num_or_nil }), true_or_fail),
		write = func(T({ file }, string_or_num), file_or_fail),
	}),
})

local osdate_table = lib({
	year = number,
	month = number,
	day = number,
	hour = number,
	min = number,
	sec = number,
	wday = number,
	yday = number,
	isdst = boolean,
})

local bool_or_nil = boolean + _nil

local ostime_table = lib({
	year = number,
	month = number,
	day = number,
	hour = num_or_nil,
	min = num_or_nil,
	sec = num_or_nil,
	isdst = bool_or_nil,
})

local true_or_nil = _true + _nil

local os_success = T({ _true, _string, number })
local os_fail = T({ _nil, _string, number })
local os_result = os_success + os_fail

local os_category = string_of("all")
	+ string_of("collate")
	+ string_of("ctype")
	+ string_of("monetary")
	+ string_of("numeric")
	+ string_of("time")

local string_or_nil = _string + _nil

local oslib = lib({
	clock = func(never, number),
	date = func(T({ string_of("*t") + string_of("!*t"), num_or_nil }), osdate_table)
		* func(T({ _string, num_or_nil }), _string),
	difftime = num2_to_num,
	execute = func(_nil, true_or_nil) * func(_string, os_result),
	exit = func(string_or_num_or_nil, never),
	getenv = func(string_or_num, _string),
	remove = func(string_or_num, true_or_fail),
	rename = func(T({ string_or_num, string_or_num }), true_or_fail),
	setlocale = func(T({ string_or_nil, os_category }), string_or_nil),
	time = func(ostime_table, number),
	tmpname = func(never, _string),
})

local coroutine_status = string_of("running") + string_of("suspended") + string_of("normal") + string_of("dead")

-- coroutinelib
local coroutinelib = lib({
	create = func(_function, thread),
	resume = func(T({ thread }, unknown), T({ boolean }, unknown)),
	status = func(thread, coroutine_status),
	wrap = func(_function, _function),
	yield = _function,
})

local hook_event = string_of("call")
	+ string_of("return")
	+ string_of("tail return")
	+ string_of("line")
	+ string_of("count")

local debug_hook = func(T({ hook_event, number }), never)

-- debuglib
local debuglib = lib({
	debug = func(never, never),
	getfenv = func(_function, _table),
	gethook = T({ debug_hook }),
})

-- packagelib
-- module()
-- require()

-- globals:
-- assert()
-- collectgarbage()
-- dofile()
-- error()
-- _G
-- getfenv()
-- getmetatable()
-- ipairs()
-- load()
-- loadfile()
-- loadstring()
-- next()
-- pairs()
-- pcall()
-- print()
-- rawequal()
-- rawget()
-- rawset()
-- select()
-- setfenv()
-- setmetatable()
-- tonumber()
-- tostring()
-- type()
-- unpack()
-- _VERSION
-- xpcall()

return {
	["nil"] = _nil,
	boolean = boolean,
	number = number,
	string = _string,
	table = _table,
	["function"] = _function,
	thread = thread,
	userdata = userdata,

	coroutinelib = coroutinelib,
	stringlib = stringlib,
	tablelib = tablelib,
	mathlib = mathlib,
	iolib = iolib,
	oslib = oslib,
}
