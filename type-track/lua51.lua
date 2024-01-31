local meta_types = require("type-track.meta")

local Tuple, Callable, Object, Union, Intersection, Literal =
	meta_types.Tuple, meta_types.Callable, meta_types.Object,
	meta_types.Union, meta_types.Intersection, meta_types.Literal

local T = Tuple

local _nil = Object(nil, "nil")
local boolean = Object(nil, "boolean")
local number = Object(nil, "number")
local _string = Object(nil, "string")
local _table = Object(nil, "table")
local _function = Object(nil, "function")
local thread = Object(nil, "thread")
local userdata = Object(nil, "userdata")
local never = T{}
local any = Object()

local _true = Literal(true, nil, "boolean")
local _false = Literal(false, nil, "boolean")

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
	return Literal(value, _string.ops, "string")
end
string_of = memoize(string_of)

local any2 = T{any, any}
local any3 = T{any, any, any}
local anyvar = T({}, any)

local any_to_any = any / any
local any2_to_any = any2 / any
local any3_to_never = any3 / never
local any2_to_bool = any2 / boolean
local anyvar_to_anyvar = anyvar / anyvar

any.ops = {
	add = any2_to_any,
	sub = any2_to_any,
	mul = any2_to_any,
	div = any2_to_any,
	mod = any2_to_any,
	pow = any2_to_any,
	unm = any_to_any,
	concat = any2_to_any,
	len = any_to_any,
	eq = any2_to_bool,
	lt = any2_to_bool,
	le = any2_to_bool,
	index = any2_to_any,
	newindex = any3_to_never,
	call = anyvar_to_anyvar,
}

local string_or_num = number + _string
local num2_to_num = T{number, number} / number
local concat_call = T{string_or_num, string_or_num} / _string

number.ops = {
	add = num2_to_num,
	sub = num2_to_num,
	mul = num2_to_num,
	div = num2_to_num,
	mod = num2_to_num,
	pow = num2_to_num,
	concat = concat_call
}

_string.ops = {
	concat = concat_call,
}

_table.ops = {
	index = T{_table, any} / any,
	newindex = T{_table, any, any} / any,
	len = _table / number,
}

_function.ops = {
	call = anyvar_to_anyvar,
}

local num_or_nil = _nil + number
local numvar = T({}, number)
local string_or_numvar = T({}, string_or_num)

---@param t type-track.Type
local function array_of(t)
	return Object({
		index = T{any, number} / t,
		newindex = T{any, number, t} / never,
	}, "table")
end

local function lib(src, tab)
	local entries = {}
	for k, v in pairs(tab) do
		table.insert(entries, T{src, string_of(k)} / v)
	end

	return Intersection(entries)
end

local string_or_num_or_nil = _string + number + _nil

-- stringlib
local stringlib = Object({
	index = lib(any, {
		byte = T{string_or_num, num_or_nil, num_or_nil} / numvar,
		char = string_or_numvar / _string,
		dump = _function / _string,
		find = T{string_or_num, string_or_num, string_or_num_or_nil, any}
			/ (_nil + T({ number, number }, string_or_num)),
		format = T({ string_or_num }, any) / _string,
		gmatch = T{string_or_num, string_or_num}
			/ (never / string_or_numvar),
		gsub = T{string_or_num, string_or_num, number + _string + _table + _function, num_or_nil}
			/ T{_string, number},
		len = string_or_num / number,
		lower = string_or_num / _string,
		match = T{string_or_num, string_or_num, num_or_nil} / T({}, string_or_num_or_nil),
		rep = T{string_or_num, number} / _string,
		reverse = string_or_num / _string,
		sub = T{string_or_num, number, num_or_nil} / _string,
		upper = string_or_num / _string,
	}),
	newindex = any3_to_never,
}, "table")

_string.ops.index = stringlib.ops.index

local tablelib = Object({
	index = lib(any, {
		concat = T{array_of(_string), string_or_num_or_nil, num_or_nil, num_or_nil} / _string,
		insert = (T{_table, any} / never) * (T{_table, number, any} / never),
		maxn = _table / number,
		remove = T{_table, num_or_nil} / never,
		sort = T{_table, _function} / never,
	}),
	newindex = any3_to_never,
}, "table")

local num_to_num = number / number
local num_to_num2 = number / T{number, number}
local num1Var = T({number}, number)
local num1var_to_num = num1Var / number

local mathlib = Object({
	index = lib(any, {
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
		random = (never / number) * num_to_num * num2_to_num,
		randomseed = number / never,
		sin = num_to_num,
		sinh = num_to_num,
		sqrt = num_to_num,
		tan = num_to_num,
		tanh = num_to_num,
	}),
	newindex = any3_to_never,
})

local file = Object(nil, "userdata")
local fail = T{_nil, _string}

local true_or_fail = _true + fail
local file_or_fail = file + fail
local num_or_fail = number + fail

local file_open_mode =
	string_of("r")
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

local file_read_mode =
	string_of("*n")
	+ string_of("*a")
	+ string_of("*l")
	+ string_of("*L")
	+ string_of("n")
	+ string_of("a")
	+ string_of("l")
	+ string_of("L")

local file_read_modevar = T({}, file_read_mode)

local file_seek_mode =
  string_of("set")
	+ string_of("cur")
	+ string_of("end")

local file_seek_mode_or_nil = file_seek_mode + _nil

local vbuf_mode =
	string_of("no")
	+ string_of("full")
	+ string_of("line")

local file_type =
	string_of("file")
	+ string_of("closed file")
	+ _nil

local implicit_file = (never / file_or_fail) * (file / true_or_fail)
local open_file = T{string_or_num, file_open_mode + _nil} / file_or_fail

local iolib = Object({
	index = lib(any, {
		close = (file / true_or_fail)
			* (never / true_or_fail),
		flush = never / true_or_fail,
		input = implicit_file,
		lines = (never / (never / _string)) * (string_or_num / (never / _string)),
		open = open_file,
		output = implicit_file,
		popen = open_file,
		read = file_read_modevar / string_or_numvar,
		tmpfile = never / file_or_fail,
		type = any / file_type,
		write = string_or_numvar / file_or_fail,
	}),
	newindex = any3_to_never,
})

file.ops = {
	index = lib(any, {
		close = file / true_or_fail,
		flush = file / true_or_fail,
		lines = file / (never / _string),
		read = T({file}, file_read_mode) / string_or_numvar,
		seek = T{file, file_seek_mode_or_nil, num_or_nil} / num_or_fail,
		setvbuf = T{file, vbuf_mode, num_or_nil} / true_or_fail,
		write = T({file}, string_or_num) / file_or_fail,
	}),
	newindex = any3_to_never,
}

local osdate_table = Object({
	index = lib(any, {
		year = number,
		month = number,
		day = number,
		hour = number,
		min = number,
		sec = number,
		wday = number,
		yday = number,
		isdst = boolean,
	}),
	newindex = any3_to_never,
}, "table")

local bool_or_nil = boolean + _nil

local ostime_table = Object({
	index = lib(any, {
		year = number,
		month = number,
		day = number,
		hour = num_or_nil,
		min = num_or_nil,
		sec = num_or_nil,
		isdst = bool_or_nil
	}),
	newindex = any3_to_never,
})

local true_or_nil = _true + _nil

local os_success = T{_true, _string, number}
local os_fail = T{_nil, _string, number}
local os_result = os_success + os_fail

local os_category =
	string_of("all")
	+ string_of("collate")
	+ string_of("ctype")
	+ string_of("monetary")
	+ string_of("numeric")
	+ string_of("time")

local string_or_nil = _string + _nil

local oslib = Object({
	index = lib(any, {
		clock = never / number,
		date = (T{string_of("*t") + string_of("!*t"), num_or_nil} / osdate_table)
			* (T{_string, num_or_nil} / _string),
		difftime = num2_to_num,
		execute = (_nil / true_or_nil) * (_string / os_result),
		exit = string_or_num_or_nil / never,
		getenv = string_or_num / _string,
		remove = string_or_num / true_or_fail,
		rename = T{string_or_num, string_or_num} / true_or_fail,
		setlocale = T{string_or_nil, os_category} / string_or_nil,
		time = ostime_table / number,
		tmpname = never / _string,
	}),
	newindex = any3_to_never,
})

local coroutine_status =
	string_of("running")
	+ string_of("suspended")
	+ string_of("normal")
	+ string_of("dead")

-- coroutinelib
local coroutinelib = Object({
	index = lib(any, {
		create = anyvar_to_anyvar / thread,
		resume = T({thread}, any) / (T({_true + _false}, any)),
		status = T{thread} / coroutine_status,
		wrap = anyvar_to_anyvar / anyvar_to_anyvar,
		yield = anyvar_to_anyvar,
	}),
	newindex = any3_to_never,
})

local hook_event =
	string_of("call")
	+ string_of("return")
	+ string_of("tail return")
	+ string_of("line")
	+ string_of("count")

local debug_hook = T{hook_event, number} / never

-- debuglib
local debuglib = Object({
	index = lib(any, {
		debug = never / never,
		getfenv = anyvar_to_anyvar / _table,
		gethook = T{debug_hook},
	}),
	newindex = any3_to_never,
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
