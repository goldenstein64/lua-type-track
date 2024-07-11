local meta_types = require("type-track.meta")

local Type, Tuple, Operation, Literal, Free, Never, Unknown, GenericOperation, is_subset =
	meta_types.Type,
	meta_types.Tuple,
	meta_types.Operation,
	meta_types.Literal,
	meta_types.Free,
	meta_types.Never,
	meta_types.Unknown,
	meta_types.GenericOperation,
	meta_types.is_subset

---@generic F
---@param f F
---@return F, table
local function memoize(f)
	local cache = {}
	return function(a, ...)
		local r = cache[a]
		if cache[a] == nil then
			r = f(a, ...)
			cache[a] = r
		end

		return r
	end,
		cache
end

---modifies the type so it unifies to itself
---@param t type-track.Type
---@return type-track.Type t
local function axiom(t)
	if t.__class == Free then
		---@cast t type-track.Free
		t.normalized = t.value
		t.value.normalized = t.value
	else
		t.normalized = t
	end
	return t
end

local T = Tuple

local unit = Tuple.Unit

local stringlib_ref = Free()

local number, _string, string_or_num, concat_call
---@param value string
---@return type-track.Literal
local function string_of(value)
	return Literal(value, _string)
end
memoize(string_of)
do
	local number_ref = Free()
	local string_ref = Free()
	string_or_num = string_ref + number_ref

	concat_call = Operation("concat", string_or_num, string_ref)

	local number_lit = string_of("number")
	local string_lit = string_of("string")

	number = Operation("type", Never, number_lit)
		* Operation("add", number_ref, number_ref)
		* Operation("sub", number_ref, number_ref)
		* Operation("mul", number_ref, number_ref)
		* Operation("div", number_ref, number_ref)
		* Operation("mod", number_ref, number_ref)
		* Operation("pow", number_ref, number_ref)
		* concat_call

	_string = Operation("type", Never, string_lit) * concat_call * stringlib_ref

	string_ref:reify(_string, _string) --> concat_call -> string_or_num -> number
	number_ref:reify(number, number) --> concat_call -> string_or_num -> _string
	number_lit.ops = _string
	string_lit.ops = _string

	_string.debug_name = "type[string]"
	number.debug_name = "type[number]"
end

local falsy, _nil, boolean, _true, _false
do
	local false_ref = Free()
	falsy = Operation("truthy", Never, false_ref)

	_nil = Literal(nil, Operation("type", Never, string_of("nil")) * falsy)

	boolean = Operation("type", Never, string_of("boolean"))
	_true = Literal(true, boolean)
	_false = Literal(false, boolean * falsy)
	false_ref:reify(_false, _false)
	boolean.debug_name = "type[boolean]"
	_true.debug_name = "true"
	_false.debug_name = "false"
	_nil.debug_name = "nil"
end

local thread = Operation("type", Never, string_of("thread"))
local userdata = Operation("type", Never, string_of("userdata"))
thread.debug_name = "type[thread]"
userdata.debug_name = "type[userdata]"

local unknown_var = T({}, Unknown)

local function_type = Operation("type", Never, string_of("function"))
function_type.debug_name = "type[function]"
local _function = function_type * Operation("call", unknown_var, unknown_var)

local _table = Operation("type", Never, string_of("table"))
	* Operation("index", Unknown, Unknown)
	* Operation("newindex", T({ Unknown, Unknown }), Never)
	* Operation("len", Never, number)
_table.debug_name = "type[table]"

---@param params type-track.Type
---@param returns type-track.Type
---@return type-track.Type func
local function func(params, returns)
	return function_type * Operation("call", params, returns)
end

---@param derive_fn type-track.GenericOperation.derive_fn
---@param infer_fn type-track.GenericOperation.infer_fn
---@return type-track.Type func
local function gen_func(derive_fn, infer_fn)
	return function_type * GenericOperation("call", derive_fn, infer_fn)
end

local num2_to_num = func(T({ number, number }), number)

local num_or_nil = _nil + number
local num_var = T({}, number)
local string_or_num_var = T({}, string_or_num)

---@param k type-track.Type
---@param v type-track.Type
---@return type-track.Type
local function map_of(k, v)
	return Operation("type", Never, string_of("table"))
		* Operation("index", k, v)
		* Operation("newindex", T({ k, v }), Never)
end

---@param t type-track.Type
---@return type-track.Type
local function array_of(t)
	return Operation("type", Never, string_of("table"))
		* Operation("index", number, t)
		* Operation("newindex", T({ number, t }), Never)
end

---@param tab { [string]: type-track.Type }
---@return type-track.Type
local function lib(tab)
	---@type type-track.Type
	local result = Operation("type", Never, string_of("table"))
		* Operation("newindex", T({ Unknown, Unknown }), Never)
	for k, v in pairs(tab) do
		result = result * Operation("index", string_of(k), v)
	end

	return result
end

local string_or_num_or_nil = string_or_num + _nil

-- stringlib
local stringlib = lib({
	byte = func(T({ string_or_num, num_or_nil, num_or_nil }), num_var),
	char = func(string_or_num_var, _string),
	dump = func(_function, _string),
	find = func(
		T({ string_or_num, string_or_num, string_or_num_or_nil, Unknown }),
		(_nil + T({ number, number }, string_or_num))
	),
	format = func(T({ string_or_num }, Unknown), _string),
	gmatch = func(
		T({ string_or_num, string_or_num }),
		func(unit, string_or_num_var)
	),
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
	match = func(
		T({ string_or_num, string_or_num, num_or_nil }),
		T({}, string_or_num_or_nil)
	),
	rep = func(T({ string_or_num, number }), _string),
	reverse = func(string_or_num, _string),
	sub = func(T({ string_or_num, number, num_or_nil }), _string),
	upper = func(string_or_num, _string),
})

local tablelib = lib({
	concat = func(
		T({ array_of(_string), string_or_num_or_nil, num_or_nil, num_or_nil }),
		_string
	),
	insert = func(T({ _table, Unknown }), unit)
		* func(T({ _table, number, Unknown }), unit),

	maxn = func(_table, number),
	remove = func(T({ _table, num_or_nil }), Unknown),
	sort = func(T({ _table, _function }), unit),
})
stringlib_ref:reify(stringlib, stringlib)
stringlib_ref:reify(_string, stringlib)

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
	random = func(unit, number) * num_to_num * num2_to_num,
	randomseed = func(number, unit),
	sin = num_to_num,
	sinh = num_to_num,
	sqrt = num_to_num,
	tan = num_to_num,
	tanh = num_to_num,
})

local file_type = string_of("file") + string_of("closed file") + _nil

local file_ref = Free()
local fail = T({ _nil, _string })

local true_or_fail = _true + fail
local file_or_fail = file_ref + fail
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

local implicit_file = func(unit, file_or_fail) * func(file_ref, true_or_fail)
local open_file =
	func(T({ string_or_num, file_open_mode + _nil }), file_or_fail)

local iolib = lib({
	close = func(file_ref, true_or_fail) * func(unit, true_or_fail),
	flush = func(unit, true_or_fail),
	input = implicit_file,
	lines = func(unit, func(unit, _string))
		* func(string_or_num, func(unit, _string)),
	open = open_file,
	output = implicit_file,
	popen = open_file,
	read = func(file_read_modevar, string_or_num_var),
	tmpfile = func(unit, file_or_fail),
	type = func(Unknown, file_type),
	write = func(string_or_num_var, file_or_fail),
})

local file = userdata
	* Operation("iolib.type", Never, string_of("file") + string_of("closed file"))
	* lib({
		close = func(file_ref, true_or_fail),
		flush = func(file_ref, true_or_fail),
		lines = func(file_ref, func(unit, _string)),
		read = func(T({ file_ref }, file_read_mode), string_or_num_var),
		seek = func(
			T({ file_ref, file_seek_mode_or_nil, num_or_nil }),
			num_or_fail
		),
		setvbuf = func(T({ file_ref, vbuf_mode, num_or_nil }), true_or_fail),
		write = func(T({ file_ref }, string_or_num), file_or_fail),
	})

file_ref:reify(file, file)
file_ref:reify(iolib, file)

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
	clock = func(unit, number),
	date = func(
		T({ string_of("*t") + string_of("!*t"), num_or_nil }),
		osdate_table
	) * func(T({ _string, num_or_nil }), _string),
	difftime = num2_to_num,
	execute = func(_nil, true_or_nil) * func(_string, os_result),
	exit = func(string_or_num_or_nil, unit),
	getenv = func(string_or_num, _string),
	remove = func(string_or_num, true_or_fail),
	rename = func(T({ string_or_num, string_or_num }), true_or_fail),
	setlocale = func(T({ string_or_nil, os_category }), string_or_nil),
	time = func(ostime_table, number),
	tmpname = func(unit, _string),
})

local coroutine_status = string_of("running")
	+ string_of("suspended")
	+ string_of("normal")
	+ string_of("dead")

-- coroutinelib
local coroutinelib = lib({
	create = func(_function, thread),
	resume = func(T({ thread }, Unknown), T({ boolean }, Unknown)),
	status = func(thread, coroutine_status),
	wrap = func(_function, _function),
	yield = _function,
})

local hook_event = string_of("call")
	+ string_of("return")
	+ string_of("tail return")
	+ string_of("line")
	+ string_of("count")

local debug_hook = func(T({ hook_event, number }), unit)

local thread_or_nil = thread + _nil
local func_or_num = _function + number
local table_or_nil = _table + _nil

local debug_info = lib({
	currentline = number,
	func = _function,
	isvararg = boolean,
	lastlinedefined = number,
	linedefined = number,
	namewhat = _string,
	nparams = number,
	nups = number,
	short_src = _string,
	source = _string,
	what = _string,
})

debug_info.debug_name = "debug_info"

-- debuglib
local debuglib = lib({
	debug = func(unit, unit),
	getfenv = func(_function, _table),
	gethook = func(thread_or_nil, _nil + T({ debug_hook, _string, number })),
	getinfo = func(T({ thread, func_or_num, _string }), _table)
		* func(T({ func_or_num, _string }), _table)
		* func(T({ thread, func_or_num }), debug_info)
		* func(func_or_num, debug_info),
	getlocal = func(T({ thread, number, number }), T({
		_string,
		Unknown --[[should be "any"]],
	}) + unit),
	getmetatable = func(Unknown, _table + _nil),
	getregistry = func(unit, _table),
	getupvalue = func(T({ _function, number }), T({ _string, Unknown }) + unit),
	setfenv = gen_func(function(tup) -- derive_fn
		local A = tup:at(1)
		local R = tup:at(2)
		if not A or not R then
			return nil
		end

		local func_arg = func(A, R)
		return T({ func_arg, _table }), func_arg
	end, function(domain) -- infer_fn
		local func_arg = domain:at(1)
		local table_arg = domain:at(2)
		if
			not func_arg
			or not table_arg
			or not is_subset(func_arg, function_type)
		then
			return nil
		end

		local func_params = func_arg:get_domain("call")
		if func_params then
			local func_returns = func_arg:eval("call", func_params)
			if func_returns then
				return T({ func_params, func_returns })
			end
		end

		return nil
	end),
	sethook = func(T({ thread_or_nil, debug_hook, _string, num_or_nil }), unit)
		* func(T({ thread_or_nil, _nil, string_or_nil, num_or_nil }), unit)
		* func(T({ debug_hook, _string, num_or_nil }), unit)
		* func(T({ _nil, string_or_nil, num_or_nil }), unit),
	setlocal = func(T({ thread, number, number, Unknown }), string_or_nil)
		* func(T({ number, number, Unknown }), string_or_nil),
	setmetatable = func(T({ Unknown, table_or_nil }), unit),

	setupvalue = func(T({ _function, number, Unknown }), string_or_nil),

	traceback = func(T({ thread, string_or_nil, num_or_nil }), _string)
		* func(T({ string_or_nil, num_or_nil }), _string),
})

local packagelib = lib({
	cpath = _string,
	path = _string,
	loaded = map_of(_string, Unknown),
	loaders = array_of(func(_string, func(_string, Unknown) + _string + _nil)),
	loadlib = func(T({ _string, _string }), _function),
	preload = map_of(_string, func(_string, Unknown) + _nil),

	seeall = func(_table, unit),
})

local _module = func(T({ _string }, func(_table, unit)), unit)
local _require = func(_string, Unknown)

-- users will have to implement type refinements themselves
-- <As...>(As...) -> (As...)
local _assert = gen_func(function(tup)
	return tup, tup
end, function(domain, range)
	if range then
		return (domain * range):normalize()
	else
		return domain
	end
end)

local _collectgarbage = func(_nil, number)
	* func(unit, number)
	* func(string_of("collect"), number)
	* func(string_of("stop"), number)
	* func(string_of("restart"), number)
	* func(string_of("count"), number)
	* func(string_of("step"), boolean)
	* func(string_of("setpause"), number)
	* func(string_of("setstepmul"), number)
	* func(Tuple({ string_of("setpause"), num_or_nil }), number)
	* func(Tuple({ string_of("setstepmul"), num_or_nil }), number)

local _dofile = func(string_or_nil, unknown_var)

local _error = func(Tuple({ _string, num_or_nil }), Never)
	* func(_string, Never)
	* func(unit, Never)

local __G = _table

local _getfenv
do
	local getfenv_result = map_of(_string, Unknown) + _nil
	_getfenv = func(unit, getfenv_result) * func(_function + _nil, getfenv_result)
end

-- globals:
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

local lua51 = {
	-- base types
	["nil"] = _nil,
	boolean = boolean,
	number = number,
	string = _string,
	table = _table,
	["function"] = _function,
	thread = thread,
	userdata = userdata,

	-- standard libraries
	coroutinelib = coroutinelib,
	stringlib = stringlib,
	tablelib = tablelib,
	mathlib = mathlib,
	iolib = iolib,
	oslib = oslib,
	debuglib = debuglib,
	packagelib = packagelib,

	-- standard globals that aren't libraries
	module = _module,
	require = _require,
	assert = _assert,
	collectgarbage = _collectgarbage,
	dofile = _dofile,
	error = _error,
	_G = __G,
	getfenv = _getfenv,
}

for k, t in pairs(lua51) do
	lua51[k] = axiom(t)
end

return lua51
