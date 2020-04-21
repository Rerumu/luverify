-- Lua bytecode checker
-- performs the task of `symbexec` in Lua 5.1, more or less
-- https://www.lua.org/source/5.1/ldebug.c.html#symbexec
local bit = bit or bit32 or require('bit')
local stm_lua_bytecode
local stm_lua_func

local opcode_m = {
	[0] = {b = 'OpArgR', c = 'OpArgN', t = false, a = true, m = 'ABC', n = 'MOVE'},
	{b = 'OpArgK', c = 'OpArgN', t = false, a = true, m = 'ABx', n = 'LOADK'},
	{b = 'OpArgU', c = 'OpArgU', t = false, a = true, m = 'ABC', n = 'LOADBOOL'},
	{b = 'OpArgR', c = 'OpArgN', t = false, a = true, m = 'ABC', n = 'LOADNIL'},
	{b = 'OpArgU', c = 'OpArgN', t = false, a = true, m = 'ABC', n = 'GETUPVAL'},
	{b = 'OpArgK', c = 'OpArgN', t = false, a = true, m = 'ABx', n = 'GETGLOBAL'},
	{b = 'OpArgR', c = 'OpArgK', t = false, a = true, m = 'ABC', n = 'GETTABLE'},
	{b = 'OpArgK', c = 'OpArgN', t = false, a = false, m = 'ABx', n = 'SETGLOBAL'},
	{b = 'OpArgU', c = 'OpArgN', t = false, a = false, m = 'ABC', n = 'SETUPVAL'},
	{b = 'OpArgK', c = 'OpArgK', t = false, a = false, m = 'ABC', n = 'SETTABLE'},
	{b = 'OpArgU', c = 'OpArgU', t = false, a = true, m = 'ABC', n = 'NEWTABLE'},
	{b = 'OpArgR', c = 'OpArgK', t = false, a = true, m = 'ABC', n = 'SELF'},
	{b = 'OpArgK', c = 'OpArgK', t = false, a = true, m = 'ABC', n = 'ADD'},
	{b = 'OpArgK', c = 'OpArgK', t = false, a = true, m = 'ABC', n = 'SUB'},
	{b = 'OpArgK', c = 'OpArgK', t = false, a = true, m = 'ABC', n = 'MUL'},
	{b = 'OpArgK', c = 'OpArgK', t = false, a = true, m = 'ABC', n = 'DIV'},
	{b = 'OpArgK', c = 'OpArgK', t = false, a = true, m = 'ABC', n = 'MOD'},
	{b = 'OpArgK', c = 'OpArgK', t = false, a = true, m = 'ABC', n = 'POW'},
	{b = 'OpArgR', c = 'OpArgN', t = false, a = true, m = 'ABC', n = 'UNM'},
	{b = 'OpArgR', c = 'OpArgN', t = false, a = true, m = 'ABC', n = 'NOT'},
	{b = 'OpArgR', c = 'OpArgN', t = false, a = true, m = 'ABC', n = 'LEN'},
	{b = 'OpArgR', c = 'OpArgR', t = false, a = true, m = 'ABC', n = 'CONCAT'},
	{b = 'OpArgR', c = 'OpArgN', t = false, a = false, m = 'AsBx', n = 'JMP'},
	{b = 'OpArgK', c = 'OpArgK', t = true, a = false, m = 'ABC', n = 'EQ'},
	{b = 'OpArgK', c = 'OpArgK', t = true, a = false, m = 'ABC', n = 'LT'},
	{b = 'OpArgK', c = 'OpArgK', t = true, a = false, m = 'ABC', n = 'LE'},
	{b = 'OpArgR', c = 'OpArgU', t = true, a = true, m = 'ABC', n = 'TEST'},
	{b = 'OpArgR', c = 'OpArgU', t = true, a = true, m = 'ABC', n = 'TESTSET'},
	{b = 'OpArgU', c = 'OpArgU', t = false, a = true, m = 'ABC', n = 'CALL'},
	{b = 'OpArgU', c = 'OpArgU', t = false, a = true, m = 'ABC', n = 'TAILCALL'},
	{b = 'OpArgU', c = 'OpArgN', t = false, a = false, m = 'ABC', n = 'RETURN'},
	{b = 'OpArgR', c = 'OpArgN', t = false, a = true, m = 'AsBx', n = 'FORLOOP'},
	{b = 'OpArgR', c = 'OpArgN', t = false, a = true, m = 'AsBx', n = 'FORPREP'},
	{b = 'OpArgN', c = 'OpArgU', t = true, a = false, m = 'ABC', n = 'TFORLOOP'},
	{b = 'OpArgU', c = 'OpArgU', t = false, a = false, m = 'ABC', n = 'SETLIST'},
	{b = 'OpArgN', c = 'OpArgN', t = false, a = false, m = 'ABC', n = 'CLOSE'},
	{b = 'OpArgU', c = 'OpArgN', t = false, a = true, m = 'ABx', n = 'CLOSURE'},
	{b = 'OpArgU', c = 'OpArgN', t = false, a = true, m = 'ABC', n = 'VARARG'},
}

-- int rd_int_basic(string src, int s, int e, int d)
-- @src - Source binary string
-- @s - Start index of a little endian integer
-- @e - End index of the integer
-- @d - Direction of the loop
local function rd_int_basic(src, s, e, d)
	local num = 0

	-- if bb[l] > 127 then -- signed negative
	-- 	num = num - 256 ^ l
	-- 	bb[l] = bb[l] - 128
	-- end

	for i = s, e, d do num = num + src:byte(i, i) * 256 ^ (i - s) end

	return num
end

-- float rd_flt_basic(byte f1..8)
-- @f1..4 - The 4 bytes composing a little endian float
local function rd_flt_basic(f1, f2, f3, f4)
	local sign = bit.rshift(f4, 7)
	local exp = bit.rshift(f3, 7) + bit.lshift(bit.band(f4, 0x7F), 1)
	local frac = f1 + bit.lshift(f2, 8) + bit.lshift(bit.band(f3, 0x7F), 16)
	local normal = 1

	if exp == 0 then
		if frac == 0 then
			return sign * 0
		else
			normal = 0
			exp = 1
		end
	elseif exp == 0x7F then
		if frac == 0 then
			return sign * (1 / 0)
		else
			return sign * (0 / 0)
		end
	end

	return (-1) ^ sign * 2 ^ (exp - 127) * (1 + normal / 2 ^ 23)
end

-- double rd_dbl_basic(byte f1..8)
-- @f1..8 - The 8 bytes composing a little endian double
local function rd_dbl_basic(f1, f2, f3, f4, f5, f6, f7, f8)
	local sign = bit.rshift(f8, 7)
	local exp = bit.lshift(bit.band(f8, 0x7F), 4) + bit.rshift(f7, 4)
	local frac = bit.band(f7, 0x0F) * 2 ^ 48
	local normal = 1

	frac = frac + (f6 * 2 ^ 40) + (f5 * 2 ^ 32) + (f4 * 2 ^ 24) + (f3 * 2 ^ 16) + (f2 * 2 ^ 8) + f1 -- help

	if exp == 0 then
		if frac == 0 then
			return sign * 0
		else
			normal = 0
			exp = 1
		end
	elseif exp == 0x7FF then
		if frac == 0 then
			return sign * (1 / 0)
		else
			return sign * (0 / 0)
		end
	end

	return (-1) ^ sign * 2 ^ (exp - 1023) * (normal + frac / 2 ^ 52)
end

-- int rd_int_le(string src, int s, int e)
-- @src - Source binary string
-- @s - Start index of a little endian integer
-- @e - End index of the integer
local function rd_int_le(src, s, e) return rd_int_basic(src, s, e - 1, 1) end

-- int rd_int_be(string src, int s, int e)
-- @src - Source binary string
-- @s - Start index of a big endian integer
-- @e - End index of the integer
local function rd_int_be(src, s, e) return rd_int_basic(src, e - 1, s, -1) end

-- float rd_flt_le(string src, int s)
-- @src - Source binary string
-- @s - Start index of little endian float
local function rd_flt_le(src, s) return rd_flt_basic(src:byte(s, s + 3)) end

-- float rd_flt_be(string src, int s)
-- @src - Source binary string
-- @s - Start index of big endian float
local function rd_flt_be(src, s)
	local f1, f2, f3, f4 = src:byte(s, s + 3)
	return rd_flt_basic(f4, f3, f2, f1)
end

-- double rd_dbl_le(string src, int s)
-- @src - Source binary string
-- @s - Start index of little endian double
local function rd_dbl_le(src, s) return rd_dbl_basic(src:byte(s, s + 7)) end

-- double rd_dbl_be(string src, int s)
-- @src - Source binary string
-- @s - Start index of big endian double
local function rd_dbl_be(src, s)
	local f1, f2, f3, f4, f5, f6, f7, f8 = src:byte(s, s + 7) -- same
	return rd_dbl_basic(f8, f7, f6, f5, f4, f3, f2, f1)
end

-- to avoid nested ifs in deserializing
local float_types = {
	[4] = {little = rd_flt_le, big = rd_flt_be},
	[8] = {little = rd_dbl_le, big = rd_dbl_be},
}

-- byte stm_byte(Stream S)
-- @S - Stream object to read from
local function stm_byte(S)
	local idx = S.index
	local bt = S.source:byte(idx, idx)

	S.index = idx + 1
	return bt
end

-- string stm_string(Stream S, int len)
-- @S - Stream object to read from
-- @len - Length of string being read
local function stm_string(S, len)
	local pos = S.index + len
	local str = S.source:sub(S.index, pos - 1)

	S.index = pos
	return str
end

-- string stm_lstring(Stream S)
-- @S - Stream object to read from
local function stm_lstring(S)
	local len = S:s_szt()
	local str

	if len ~= 0 then str = stm_string(S, len):sub(1, -2) end

	return str
end

-- fn cst_int_rdr(string src, int len, fn func)
-- @len - Length of type for reader
-- @func - Reader callback
local function cst_int_rdr(len, func)
	return function(S)
		local pos = S.index + len
		local int = func(S.source, S.index, pos)
		S.index = pos

		return int
	end
end

-- fn cst_flt_rdr(string src, int len, fn func)
-- @len - Length of type for reader
-- @func - Reader callback
local function cst_flt_rdr(len, func)
	return function(S)
		local flt = func(S.source, S.index)
		S.index = S.index + len

		return flt
	end
end

local function stm_instructions(S)
	local size = S:s_int()
	local code = {}

	for i = 1, size do
		local ins = S:s_ins()
		local op = bit.band(ins, 0x3F)
		local mode = opcode_m[op]
		local data = {value = ins, op = op, A = bit.band(bit.rshift(ins, 6), 0xFF)}

		if mode.m == 'ABC' then
			data.B = bit.band(bit.rshift(ins, 23), 0x1FF)
			data.C = bit.band(bit.rshift(ins, 14), 0x1FF)
			data.is_KB = mode.b == 'OpArgK' and data.B > 0xFF -- post process optimization
			data.is_KC = mode.c == 'OpArgK' and data.C > 0xFF
		elseif mode.m == 'ABx' then
			data.Bx = bit.band(bit.rshift(ins, 14), 0x3FFFF)
			data.is_K = mode.b == 'OpArgK'
		elseif mode.m == 'AsBx' then
			data.sBx = bit.band(bit.rshift(ins, 14), 0x3FFFF) - 131071
		end

		code[i] = data
	end

	return code
end

local function stm_constants(S)
	local size = S:s_int()
	local consts = {}

	for i = 1, size do
		local tt = stm_byte(S)
		local k

		if tt == 1 then
			k = stm_byte(S) ~= 0
		elseif tt == 3 then
			k = S:s_num()
		elseif tt == 4 then
			k = stm_lstring(S)
		end

		consts[i] = k -- offset +1 during instruction decode
	end

	return consts
end

local function stm_subfuncs(S, src)
	local size = S:s_int()
	local sub = {}

	for i = 1, size do
		sub[i] = stm_lua_func(S, src) -- offset +1 in CLOSURE
	end

	return sub
end

local function stm_lineinfo(S)
	local size = S:s_int()
	local lines = {}

	for i = 1, size do lines[i] = S:s_int() end

	return lines
end

local function stm_locvars(S)
	local size = S:s_int()
	local locvars = {}

	for i = 1, size do locvars[i] = {varname = stm_lstring(S), startpc = S:s_int(), endpc = S:s_int()} end

	return locvars
end

local function stm_upvals(S)
	local size = S:s_int()
	local upvals = {}

	for i = 1, size do upvals[i] = stm_lstring(S) end

	return upvals
end

function stm_lua_func(S, psrc)
	local proto = {}
	local src = stm_lstring(S) or psrc -- source is propagated

	proto.source = src -- source name

	S:s_int() -- line defined
	S:s_int() -- last line defined

	proto.numupvals = stm_byte(S) -- num upvalues
	proto.numparams = stm_byte(S) -- num params
	proto.is_vararg = stm_byte(S) -- vararg flag
	proto.max_stack_size = stm_byte(S) -- max stack size
	proto.code = stm_instructions(S)
	proto.const = stm_constants(S)
	proto.subs = stm_subfuncs(S, src)
	proto.lines = stm_lineinfo(S)

	stm_locvars(S)

	proto.upvals = stm_upvals(S)

	-- post process optimization
	for _, v in ipairs(proto.code) do
		if v.is_K then
			v.const = proto.const[v.Bx + 1] -- offset for 1 based index
		else
			if v.is_KB then v.const_B = proto.const[v.B - 0xFF] end

			if v.is_KC then v.const_C = proto.const[v.C - 0xFF] end
		end
	end

	return proto
end

function stm_lua_bytecode(src)
	-- func reader
	local rdr_func

	-- header flags
	local little
	local size_int
	local size_szt
	local size_ins
	local size_num
	local flag_int

	-- stream object
	local stream = {
		-- data
		index = 1,
		source = src,
	}

	assert(stm_string(stream, 4) == '\27Lua', 'invalid Lua signature')
	assert(stm_byte(stream) == 0x51, 'invalid Lua version')
	assert(stm_byte(stream) == 0, 'invalid Lua format')

	little = stm_byte(stream) ~= 0
	size_int = stm_byte(stream)
	size_szt = stm_byte(stream)
	size_ins = stm_byte(stream)
	size_num = stm_byte(stream)
	flag_int = stm_byte(stream) ~= 0

	rdr_func = little and rd_int_le or rd_int_be
	stream.s_int = cst_int_rdr(size_int, rdr_func)
	stream.s_szt = cst_int_rdr(size_szt, rdr_func)
	stream.s_ins = cst_int_rdr(size_ins, rdr_func)

	if flag_int then
		stream.s_num = cst_int_rdr(size_num, rdr_func)
	elseif float_types[size_num] then
		stream.s_num = cst_flt_rdr(size_num, float_types[size_num][little and 'little' or 'big'])
	else
		error('unsupported float size')
	end

	return stm_lua_func(stream, '@virtual')
end

local MAX_REG = 250

local ERR = {
	BAD_CONST_NAME = 'bad const name not of type `string`',
	BAD_JUMP_SETLIST = 'bad jump enters `setlist` argument',
	BAD_VARARG = 'bad `vararg` in non-vararg function',
	NO_CONCAT = 'no concat happens; `b < c`',
	OOB_CODE_MINUS = 'out of bounds of code segment (< 0)',
	OOB_CODE_PLUS = 'out of bounds of code segment (> max)',
	OOB_FAST_K = 'out of bounds fast constant',
	OOB_KST = 'out of bounds constant',
	OOB_OPCODE = 'out of bounds opcode',
	OOB_REG = 'out of bounds register',
	OOB_SUB = 'out of bounds function',
	OOB_UPV = 'out of bounds upvalue',
	TOO_MANY_PARAM = 'too many parameters',
	TOO_MANY_REG = 'too many registers',
	TOO_MANY_UPV_D = 'too many upvalues in debug',
	UNUSED_REG = 'unused register must be zero',
	WRONG_CAPTURE = 'wrong trailing instruction; some capture expected',
	WRONG_END = 'wrong end to function; `return` expected',
	WRONG_JUMP = 'wrong trailing instruction; `jump` expected',
	WRONG_LINE = 'wrong line info for code',
	WRONG_TOP = 'wrong trailing instruction; a top handler was expected',
	WRONG_TOP_NUM = 'wrong trailing instruction; top `b == 0` expected',
	WRONG_VARARG = 'wrong vararg flag',
}

local OP = {}

for i, v in pairs(opcode_m) do OP[v.n] = i end

local function fail_if_not(s, cond, err)
	if not cond then
		local hint = {}
		local saved_pc

		for i = -4, 4 do
			local pc = s.pc + i
			local inst = s.pt.code[pc]

			if inst then
				local A = inst.A
				local B = inst.B or inst.Bx or inst.sBx
				local C = inst.C or '-'
				local name = opcode_m[inst.op].n:lower()
				local str = ('    [%d] %-10s %-3s %-3s %-3s'):format(pc, name, A, B, C)

				table.insert(hint, str)

				if i == 0 then saved_pc = #hint end
			end
		end

		hint[saved_pc] = '->' .. hint[saved_pc]:sub(3)

		local code = table.concat(hint, '\n')
		local final = ('<func=%d>: %s\n%s'):format(s.id, err, code)

		error(final, 0)
	end
end

local function validate_header(s)
	local pt = s.pt
	local last_pc = #pt.code
	local last_line = #pt.lines

	fail_if_not(s, pt.max_stack_size <= MAX_REG, ERR.TOO_MANY_REG)
	fail_if_not(s, pt.is_vararg == 0 or pt.is_vararg == 1, ERR.WRONG_VARARG)
	fail_if_not(s, pt.numparams + (pt.is_vararg ~= 0 and 1 or 0) <= MAX_REG, ERR.TOO_MANY_PARAM)
	fail_if_not(s, pt.numupvals <= #pt.upvals, ERR.TOO_MANY_UPV_D)
	fail_if_not(s, last_line == last_pc and last_line ~= 0, ERR.WRONG_LINE)
	fail_if_not(s, last_pc ~= 0 and pt.code[last_pc].op == OP.RETURN, ERR.WRONG_END)
end

local function v_reg(s, x) fail_if_not(s, x < s.pt.max_stack_size, ERR.OOB_REG) end

local function v_code_pc(s, o) fail_if_not(s, s.pc + o <= #s.pt.code, ERR.OOB_CODE_PLUS) end

local function v_arg_mode(s, x, m)
	if m == 'OpArgN' then
		fail_if_not(s, x == 0, ERR.UNUSED_REG)
	elseif m == 'OpArgR' then
		v_reg(s, x)
	elseif m == 'OpArgK' then
		if x > 0xFF then
			fail_if_not(s, x - 0x100 < #s.pt.const, ERR.OOB_FAST_K)
		else
			v_reg(s, x)
		end
	end
end

local function v_open_op(s)
	local i = s.pt.code[s.pc + 1]
	local ok = i.op == OP.CALL or i.op == OP.TAILCALL or i.op == OP.RETURN or i.op == OP.SETLIST

	fail_if_not(s, ok, ERR.WRONG_TOP)
	fail_if_not(s, i.B == 0, ERR.WRONG_TOP_NUM)
end

local function validate_code(s)
	local pt = s.pt
	local last_pc = #pt.code

	while s.pc <= last_pc do
		local i = pt.code[s.pc]
		local op = i.op
		local a = i.A
		local b, c = 0, 0

		fail_if_not(s, op <= #opcode_m, ERR.OOB_OPCODE)
		v_reg(s, a)

		local mode = opcode_m[op]

		if mode.m == 'ABC' then
			b = i.B
			c = i.C
			v_arg_mode(s, b, mode.b)
			v_arg_mode(s, c, mode.c)
		elseif mode.m == 'ABx' then
			b = i.Bx

			if mode.b == 'OpArgK' then fail_if_not(s, b < #pt.const, ERR.OOB_KST) end
		elseif mode.m == 'AsBx' then
			b = i.sBx
			if mode.b == 'OpArgR' then
				local dest = s.pc + 1 + b

				fail_if_not(s, 1 <= dest, ERR.OOB_CODE_MINUS)
				v_code_pc(s, 1 + b)

				if dest > 1 then
					local j = 1

					while j < dest do
						local d = pt.code[dest - j]

						if d.op ~= OP.SETLIST or d.C ~= 0 then
							break
						else
							j = j + 1
						end
					end

					fail_if_not(s, j % 2 == 1, ERR.BAD_JUMP_SETLIST)
				end
			end
		end

		if mode.t then
			v_code_pc(s, 2)
			fail_if_not(s, pt.code[s.pc + 1].op == OP.JMP, ERR.WRONG_JUMP)
		end

		if op == OP.LOADBOOL then
			if c == 1 then
				v_code_pc(s, 2)

				local val = pt.code[s.pc + 1]

				fail_if_not(s, val.op ~= OP.SETLIST or val.c ~= 0, ERR.BAD_JUMP_SETLIST)
			end
		elseif op == OP.GETUPVAL or op == OP.SETUPVAL then
			fail_if_not(s, b < pt.numupvals, ERR.OOB_UPV)
		elseif op == OP.GETGLOBAL or op == OP.SETGLOBAL then
			fail_if_not(s, type(i.const) == 'string', ERR.BAD_CONST_NAME)
		elseif op == OP.SELF then
			v_reg(s, a + 1)
		elseif op == OP.CONCAT then
			fail_if_not(s, b < c, ERR.NO_CONCAT)
		elseif op == OP.TFORLOOP then
			fail_if_not(s, c >= 1, ERR.NO_CONTROL)
			v_reg(s, a + 2 + c)
		elseif op == OP.FORLOOP or op == OP.FORPREP then
			v_reg(s, a + 3)
		elseif op == OP.CALL or op == OP.TAILCALL then
			if b ~= 0 then v_reg(s, a + b - 1) end

			c = c - 1

			if c == -1 then
				v_open_op(s)
			elseif c ~= 0 then
				v_reg(s, a + c - 1)
			end
		elseif op == OP.RETURN then
			b = b - 1

			if b > 0 then v_reg(s, a + b - 1) end
		elseif op == OP.SETLIST then
			if b > 0 then v_reg(s, a + b) end
			if c == 0 then
				v_code_pc(s, 1)
				s.pc = s.pc + 1
			end
		elseif op == OP.CLOSURE then
			fail_if_not(s, b <= #pt.subs, ERR.OOB_SUB)

			local nup = pt.subs[b + 1].numupvals

			v_code_pc(s, nup)

			for j = 1, nup do
				local ps = pt.code[s.pc + j].op

				fail_if_not(s, ps == OP.MOVE or ps == OP.GETUPVAL, ERR.WRONG_CAPTURE)
			end
		elseif op == OP.VARARG then
			fail_if_not(s, pt.is_vararg == 0 or pt.is_vararg == 1, ERR.BAD_VARARG)
			b = b - 1

			if b == -1 then v_open_op(s) end

			v_reg(s, a + b - 1)
		end

		s.pc = s.pc + 1
	end
end

local function validate_sub(pt, id)
	local state = {pt = pt, id = id, pc = 1}

	validate_header(state)
	validate_code(state)

	for _, func in ipairs(pt.subs) do
		id = id + 1
		validate_sub(func, id)
	end
end

local function validate_lua(pt) return validate_sub(pt, 0) end

return {stm_lua = stm_lua_bytecode, validate_lua = validate_lua}
