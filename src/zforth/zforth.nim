import tables, options

type
  ZfCell* = int
  ZfAddr* = uint

const
  ZF_CELL_FMT* = "%.14g"
  ZF_ADDR_FMT* = "%04x"

##  Memory region sizes: dictionary size is given in bytes, stack sizes are
##  number of elements of type ZfCell

const
  ZF_DICT_SIZE* = 4096
  ZF_DSTACK_SIZE* = 128
  ZF_RSTACK_SIZE* = 128

type
  zf_result* = enum
    ZF_OK = 0x0,
    ZF_ABORT_INTERNAL_ERROR = 0x1,
    ZF_ABORT_OUTSIDE_MEM = 0x2,
    ZF_ABORT_DSTACK_UNDERRUN = 0x3,
    ZF_ABORT_DSTACK_OVERRUN = 0x4,
    ZF_ABORT_RSTACK_UNDERRUN = 0x5,
    ZF_ABORT_RSTACK_OVERRUN = 0x6,
    ZF_ABORT_NOT_A_WORD = 0x7,
    ZF_ABORT_COMPILE_ONLY_WORD = 0x8,
    ZF_ABORT_INVALID_SIZE = 0x9,
    ZF_ABORT_DIVISION_BY_ZERO = 0xA,
    ZF_ABORT_UNKNOWN_SYS = 0xB,
    ZF_ABORT_UNKNOWN = 0xC
  zf_mem_size* = enum
    ZF_MEM_SIZE_VAR,
    ZF_MEM_SIZE_CELL,
    ZF_MEM_SIZE_U8,
    ZF_MEM_SIZE_U16,
    ZF_MEM_SIZE_U32,
    ZF_MEM_SIZE_S8,
    ZF_MEM_SIZE_S16,
    ZF_MEM_SIZE_S32
  zf_input_state* = enum
    ZF_INPUT_INTERPRET,
    ZF_INPUT_PASS_CHAR,
    ZF_INPUT_PASS_WORD
  zf_syscall_id* = enum
    ZF_SYSCALL_EMIT,
    ZF_SYSCALL_PRINT,
    ZF_SYSCALL_TELL,
    ZF_SYSCALL_USER = 128


##  ZForth API functions

proc zf_init*()
proc zf_bootstrap*()
proc zf_dump*(len: ref csize): pointer
proc zf_eval*(buf: cstring): zf_result
proc zf_abort*(reason: zf_result)
proc zf_push*(v: ZfCell)
proc zf_pop*(): ZfCell
proc zf_pick*(n: ZfAddr): ZfCell
proc zf_sc*(): ZfCell
##  Host provides these functions

# proc zf_host_sys*(id: zf_syscall_id; last_word: string): zf_input_state
# proc zf_host_trace*(fmt: string; va: seq[string])
# proc zf_host_parse_num*(buf: string): ZfCell

##  Flags and length encoded in words

const
  ZF_FLAG_IMMEDIATE* = (1 shl 6)
  ZF_FLAG_PRIM* = (1 shl 5)

template ZF_FLAG_LEN*(v: untyped): untyped =
  (v and 0x0000001F)

when defined(zfInline):
  const
    ZF_INLINE* = inline
else:
  const
    ZF_INLINE* = true
##  This macro is used to perform boundary checks. If defined(zfBoundaryChecks)
##  is set to 0, the boundary check code will not be compiled in to reduce size

when defined(zfBoundaryChecks):
  template CHECK*(exp, abort: untyped): void =
    if not (exp):
      zf_abort(abort)

else:
  template CHECK*(exp, abort: untyped): void =
    nil

##  Define all primitives, make sure the two tables below always match.  The
##  names are defined as a \0 separated list, terminated by double \0. This
##  saves space on the pointers compared to an array of strings. Immediates are
##  prefixed by an underscore, which is later stripped of when putting the name
##  in the dictionary.

type
  zf_primitive* = enum
    PRIM_EXIT = "exit",
    PRIM_LIT = "lit",
    PRIM_LTZ = "<0",
    PRIM_COL = ":",
    PRIM_SEMICOL = "_;",
    PRIM_ADD = "+",
    PRIM_SUB = "-",
    PRIM_MUL = "*",
    PRIM_DIV = "/",
    PRIM_MOD = "%",
    PRIM_DROP = "drop",
    PRIM_DUP = "dup",
    PRIM_PICKR = "pickr",
    PRIM_IMMEDIATE = "_immediate",
    PRIM_PEEK = "@@",
    PRIM_POKE = "!!",
    PRIM_SWAP = "swap",
    PRIM_ROT = "rot",
    PRIM_JMP = "jmp",
    PRIM_JMP0 = "jmp0",
    PRIM_TICK = "\'",
    PRIM_COMMENT = "\\",
    PRIM_PUSHR = ">r",
    PRIM_POPR = "r>",
    PRIM_EQUAL = "=",
    PRIM_SYS = "sys",
    PRIM_PICK = "pick",
    PRIM_COMMA = "k,,",
    PRIM_KEY = "key",
    PRIM_LITS = "lits",
    PRIM_LEN = "#",
    PRIM_AND = "&",
    PRIM_COUNT = "cnt"


##  Stacks and dictionary memory
## ##  Stacks and dictionary memory
type 
  ZStack* = ref object 
    maximum: ZfCell
    data: seq[ZfCell]

proc newZStack*(maximum: ZfCell): ZStack =
  result = new(ZStack)
  result.maximum = 0
  result.data = newSeqOfCap[ZfCell](maximum)

proc push*(stack: ZStack, val: ZfCell) = 
  if len(stack.data) < stack.maximum:
    stack.data.add(val)
  else:
    raise newException(CatchableError, "push overflow")

proc pop*(stack: ZStack): ZfCell = 
  return stack.data.pop()

proc pick*(stack: ZStack, idx: ZfAddr): ZfCell = 
  return stack.data[idx]

# Return the element at r, c
proc `[]`*(stack: ZStack, idx: ZfAddr): ZfCell =
  result = stack.data[idx]

# Set the element at r, c
proc `[]=`*(stack: var ZStack, idx: ZfAddr, val: ZfCell) =  
  stack.data[idx] = val

var
  rstack*: ZStack = newZStack(ZF_RSTACK_SIZE)
  dstack*: ZStack = newZStack(ZF_DSTACK_SIZE)
  zcodes*: ZStack = newZStack(ZF_DICT_SIZE)
  dict*: Table[string, ZfAddr]

##  State and stack and interpreter pointers
var
  input_state*: zf_input_state
  dsp*: ZfAddr
  rsp*: ZfAddr
  ip*: ZfAddr

##  setjmp env for handling aborts


##  User variables are variables which are shared between forth and C. From
##  forth these can be accessed with @ and ! at pseudo-indices in low memory, in
##  C they are stored in an array of ZfAddr with friendly reference names
##  through some macros

type
  UserVars* = enum
    HERE = (0, "h"),           ##  compilation pointer in dictionary
    LATEST = "latest",
    TRACE = "trace",           ##  trace enable flag
    COMPILING = "compiling",       ##  compiling flag
    POSTPONE = "_postpone",        ##  flag to indicate next imm word should be compiled
    USERVAR_COUNT = "_count"

##  Prototypes

# proc do_prim*(prim: zf_primitive; input: string)

# proc dict_get_cell*(zaddr: ZfAddr; v: ref ZfCell): ZfAddr
# proc dict_get_bytes*(zaddr: ZfAddr; buf: pointer; len: size)
##  Tracing functions. If disabled, the trace() function is replaced by an empty
##  macro, allowing the compiler to optimize away the function calls to
##  op_name()

proc trace*(fmt: string) {.varargs.} =
  discard

proc op_name*(zaddr: ZfAddr): string =
  return ""

##
##  Handle abort by unwinding the C stack and sending control back into
##  zf_eval()
##

proc zf_abort*(reason: zf_result) =
  raise newException(CatchableError, "error: " & $zf_result)

##
##  Stack operations.
##

proc zf_push*(v: ZfCell) =
  trace("»", ZF_CELL_FMT, " ", v)
  dstack.push(v)

proc zf_pop*(): ZfCell =
  var v: ZfCell
  v = dstack.pop()
  trace("«", ZF_CELL_FMT, " ", v)
  return v

proc zf_pick*(n: ZfAddr): ZfCell =
  return dstack.pick(n)

proc zf_pushr*(v: ZfCell) =
  trace("r»", ZF_CELL_FMT, " ", v)
  rstack.push(v)

proc zf_popr*(): ZfCell =
  result = rstack.pop()

proc zf_pickr*(n: ZfAddr): ZfCell =
  return rstack.pick(rsp - n - 1)

##
##  ZfCells are encoded in the dictionary with a variable length:
##
##  encode:
##
##     integer   0 ..   127  0xxxxxxx
##     integer 128 .. 16383  10xxxxxx xxxxxxxx
##     else                  11111111 <raw copy of ZfCell>
##
##  #if defined(zf_TYPED_MEM_ACCESS)
##  #define GET(s, t) if(size == s) { t v ## t; dict_get_bytes(addr, &v ## t, sizeof(t)); *v = v ## t; return sizeof(t); };
##  #define PUT(s, t, val) if(size == s) { t v ## t = val; return dict_put_bytes(addr, &v ## t, sizeof(t)); }
##  #else

##
##  Shortcut functions for cell access with variable cell size
##

proc dict_put_cell*(zaddr: ZfAddr; v: ZfCell): ZfAddr =
  return dict_put_cell_typed(zaddr, v, ZF_MEM_SIZE_VAR)

proc dict_get_cell*(zaddr: ZfAddr; v: ref ZfCell): ZfAddr =
  return dict_get_cell_typed(zaddr, v, ZF_MEM_SIZE_VAR)

##
##  Generic dictionary adding, these functions all add at the HERE pointer and
##  increase the pointer
##

proc dict_add_cell_typed*(v: ZfCell; size: zf_mem_size) =
  inc(HERE, dict_put_cell_typed(HERE, v, size))
  trace(" ")

proc dict_add_cell*(v: ZfCell) =
  dict_add_cell_typed(v, ZF_MEM_SIZE_VAR)

proc dict_add_op*(op: ZfAddr) =
  dict_add_cell(op)
  trace("+%s ", op_name(op))

proc dict_add_lit*(v: ZfCell) =
  dict_add_op(PRIM_LIT)
  dict_add_cell(v)

proc dict_add_str*(s: cstring) =
  var l: csize
  trace("\n+", ZF_ADDR_FMT, " ", ZF_ADDR_FMT, " s \'%s\'", HERE, 0, s)
  l = strlen(s)
  inc(HERE, dict_put_bytes(HERE, s, l))

##
##  Create new word, adjusting HERE and LATEST accordingly
##

proc create*(name: cstring; flags: cint) =
  var here_prev: ZfAddr
  trace("\n=== create \'%s\'", name)
  here_prev = HERE
  dict_add_cell((strlen(name)) or flags)
  dict_add_cell(LATEST)
  dict_add_str(name)
  LATEST = here_prev
  trace("\n===")

##
##  Find word in dictionary, returning address and execution token
##

proc find_word*(name: cstring; word: ref ZfAddr; code: ref ZfAddr): cint =
  var w: ZfAddr = LATEST
  var namelen: csize = strlen(name)
  while w:
    var
      link: ZfCell
      d: ZfCell
    var p: ZfAddr = w
    var len: int
    inc(p, dict_get_cell(p, addr(d)))
    inc(p, dict_get_cell(p, addr(link)))
    len = ZF_FLAG_LEN(cast[cint](d))
    if len == namelen:
      var name2: cstring = cast[cstring](addr(dict[p]))
      if memcmp(name, name2, len) == 0:
        word[] = w
        code[] = p + len
        return 1
    w = link
  return 0

##
##  Set 'immediate' flag in last compiled word
##

proc make_immediate*() =
  var lenflags: ZfCell
  dict_get_cell(LATEST, addr(lenflags))
  dict_put_cell(LATEST, cast[cint](lenflags) or ZF_FLAG_IMMEDIATE)

##
##  Inner interpreter
##

proc run*(input: cstring) =
  while ip != 0:
    var d: ZfCell
    var
      i: ZfAddr
      ip_org: ZfAddr = ip
    var l: ZfAddr = dict_get_cell(ip, addr(d))
    var code: ZfAddr = d
    trace("\n ", ZF_ADDR_FMT, " ", ZF_ADDR_FMT, " ", ip, code)
    i = 0
    while i < rsp:
      trace("┊  ")
      inc(i)
    inc(ip, l)
    if code <= PRIM_COUNT:
      do_prim(cast[zf_primitive](code), input)
      ##  If the prim requests input, restore IP so that the
      ##  next time around we call the same prim again
      if input_state != ZF_INPUT_INTERPRET:
        ip = ip_org
        break
    else:
      trace("%s/", ZF_ADDR_FMT, " ", op_name(code), code)
      zf_pushr(ip)
      ip = code
    input = nil

##
##  Execute bytecode from given address
##

proc execute*(zaddr: ZfAddr) =
  ip = zaddr
  rsp = 0
  zf_pushr(0)
  trace("\n[%s/", ZF_ADDR_FMT, "] ", op_name(ip), ip)
  run(nil)

proc peek*(zaddr: ZfAddr; val: ref ZfCell; len: cint): ZfAddr =
  if zaddr < USERVAR_COUNT:
    val[] = uservar[zaddr]
    return 1
  else:
    return dict_get_cell_typed(zaddr, val, cast[zf_mem_size](len))

##
##  Run primitive opcode
##

proc do_prim*(op: zf_primitive; input: cstring) =
  var
    d1: ZfCell
    d2: ZfCell
    d3: ZfCell
  var
    zaddr: ZfAddr
    len: ZfAddr
  trace("(%s) ", op_name(op))
  case op
  of PRIM_COL:
    if input == nil:
      input_state = ZF_INPUT_PASS_WORD
    else:
      create(input, 0)
      COMPILING = 1
  of PRIM_LTZ:
    zf_push(zf_pop() < 0)
  of PRIM_SEMICOL:
    dict_add_op(PRIM_EXIT)
    trace("\n===")
    COMPILING = 0
  of PRIM_LIT:
    inc(ip, dict_get_cell(ip, addr(d1)))
    zf_push(d1)
  of PRIM_EXIT:
    ip = zf_popr()
  of PRIM_LEN:
    len = zf_pop()
    zaddr = zf_pop()
    zf_push(peek(zaddr, addr(d1), len))
  of PRIM_PEEK:
    len = zf_pop()
    zaddr = zf_pop()
    peek(zaddr, addr(d1), len)
    zf_push(d1)
  of PRIM_POKE:
    d2 = zf_pop()
    zaddr = zf_pop()
    d1 = zf_pop()
    if zaddr < USERVAR_COUNT:
      uservar[zaddr] = d1
      break
    dict_put_cell_typed(zaddr, d1, cast[zf_mem_size](d2))
  of PRIM_SWAP:
    d1 = zf_pop()
    d2 = zf_pop()
    zf_push(d1)
    zf_push(d2)
  of PRIM_ROT:
    d1 = zf_pop()
    d2 = zf_pop()
    d3 = zf_pop()
    zf_push(d2)
    zf_push(d1)
    zf_push(d3)
  of PRIM_DROP:
    zf_pop()
  of PRIM_DUP:
    d1 = zf_pop()
    zf_push(d1)
    zf_push(d1)
  of PRIM_ADD:
    d1 = zf_pop()
    d2 = zf_pop()
    zf_push(d1 + d2)
  of PRIM_SYS:
    d1 = zf_pop()
    input_state = zf_host_sys(cast[zf_syscall_id](d1), input)
    if input_state != ZF_INPUT_INTERPRET:
      zf_push(d1)
      ##  re-push id to resume
  of PRIM_PICK:
    zaddr = zf_pop()
    zf_push(zf_pick(zaddr))
  of PRIM_PICKR:
    zaddr = zf_pop()
    zf_push(zf_pickr(zaddr))
  of PRIM_SUB:
    d1 = zf_pop()
    d2 = zf_pop()
    zf_push(d2 - d1)
  of PRIM_MUL:
    zf_push(zf_pop() * zf_pop())
  of PRIM_IMMEDIATE:
    make_immediate()
  of PRIM_JMP:
    inc(ip, dict_get_cell(ip, addr(d1)))
    trace("ip ", ZF_ADDR_FMT, "=>", ZF_ADDR_FMT, ip, cast[ZfAddr](d1))
    ip = d1
  of PRIM_JMP0:
    inc(ip, dict_get_cell(ip, addr(d1)))
    if zf_pop() == 0:
      trace("ip ", ZF_ADDR_FMT, "=>", ZF_ADDR_FMT, ip, cast[ZfAddr](d1))
      ip = d1
  of PRIM_TICK:
    inc(ip, dict_get_cell(ip, addr(d1)))
    trace("%s/", op_name(d1))
    zf_push(d1)
  of PRIM_COMMA:
    d2 = zf_pop()
    d1 = zf_pop()
    dict_add_cell_typed(d1, cast[zf_mem_size](d2))
  of PRIM_COMMENT:
    if not input or input[0] != ')':
      input_state = ZF_INPUT_PASS_CHAR
  of PRIM_PUSHR:
    zf_pushr(zf_pop())
  of PRIM_POPR:
    zf_push(zf_popr())
  of PRIM_EQUAL:
    zf_push(zf_pop() == zf_pop())
  of PRIM_KEY:
    if input == nil:
      input_state = ZF_INPUT_PASS_CHAR
    else:
      zf_push(input[0])
  of PRIM_LITS:
    inc(ip, dict_get_cell(ip, addr(d1)))
    zf_push(ip)
    zf_push(d1)
    inc(ip, d1)
  of PRIM_AND:
    zf_push(cast[cint](zf_pop()) and cast[cint](zf_pop()))
  else:
    zf_abort(ZF_ABORT_INTERNAL_ERROR)

##
##  Handle incoming word. Compile or interpreted the word, or pass it to a
##  deferred primitive if it requested a word from the input stream.
##

proc handle_word*(buf: cstring) =
  var
    w: ZfAddr
    c: ZfAddr = 0
  var found: cint
  ##  If a word was requested by an earlier operation, resume with the new
  ##  word
  if input_state == ZF_INPUT_PASS_WORD:
    input_state = ZF_INPUT_INTERPRET
    run(buf)
    return
  found = find_word(buf, addr(w), addr(c))
  if found:
    ##  Word found: compile or execute, depending on flags and state
    var d: ZfCell
    var flags: cint
    dict_get_cell(w, addr(d))
    flags = d
    if COMPILING and (POSTPONE or not (flags and ZF_FLAG_IMMEDIATE)):
      if flags and ZF_FLAG_PRIM:
        dict_get_cell(c, addr(d))
        dict_add_op(d)
      else:
        dict_add_op(c)
      POSTPONE = 0
    else:
      execute(c)
  else:
    ##  Word not found: try to convert to a number and compile or push, depending
    ##  on state
    var v: ZfCell = zf_host_parse_num(buf)
    if COMPILING:
      dict_add_lit(v)
    else:
      zf_push(v)

##
##  Handle one character. Split into words to pass to handle_word(), or pass the
##  char to a deferred prim if it requested a character from the input stream
##

proc handle_char*(c: char) =
  var buf: array[32, char]
  var len: csize = 0
  if input_state == ZF_INPUT_PASS_CHAR:
    input_state = ZF_INPUT_INTERPRET
    run(addr(c))
  elif c != '\x00' and not isspace(c):
    if len < sizeof((buf)) - 1:
      buf[inc(len)] = c
      buf[len] = '\x00'
  else:
    if len > 0:
      len = 0
      handle_word(buf)

##
##  Initialisation
##

proc zf_init*() =
  HERE = USERVAR_COUNT * sizeof((ZfAddr))
  LATEST = 0
  dsp = 0
  rsp = 0
  COMPILING = 0

when defined(zfBootstrap):
  ##
  ##  Functions for bootstrapping the dictionary by adding all primitive ops and the
  ##  user variables.
  ##
  proc add_prim*(name: cstring; op: zf_primitive) =
    var imm: cint = 0
    if name[0] == '_':
      inc(name)
      imm = 1
    create(name, ZF_FLAG_PRIM)
    dict_add_op(op)
    dict_add_op(PRIM_EXIT)
    if imm:
      make_immediate()

  proc add_uservar*(name: cstring; zaddr: ZfAddr) =
    create(name, 0)
    dict_add_lit(zaddr)
    dict_add_op(PRIM_EXIT)

  proc zf_bootstrap*() =
    ##  Add primitives and user variables to dictionary
    var i: ZfAddr = 0
    var p: cstring
    p = prim_names
    while p[]:
      add_prim(p, cast[zf_primitive](inc(i)))
      inc(p, strlen(p) + 1)
    i = 0
    p = uservar_names
    while p[]:
      add_uservar(p, inc(i))
      inc(p, strlen(p) + 1)

else:
  proc zf_bootstrap*() =
    discard

##
##  Eval forth string
##

proc zf_eval*(buf: cstring): zf_result =
  var r: zf_result = cast[zf_result](setjmp(jmpbuf))
  if r == ZF_OK:
    while true:
      handle_char(buf[])
      if buf[] == '\x00':
        return ZF_OK
      inc(buf)
  else:
    COMPILING = 0
    rsp = 0
    dsp = 0
    return r

proc zf_sc*(): ZfCell =
  return dsp

proc zf_dump*(len: ref csize): pointer =
  if len:
    len[] = sizeof((dict))
  return dict

##
##  End
##
