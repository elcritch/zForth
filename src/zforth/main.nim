
import
  zforth

##
##  Evaluate buffer with code, check return value and report errors
##

proc do_eval*(src: cstring; line: cint; buf: cstring): zf_result =
  var msg: cstring = nil
  var rv: zf_result = zf_eval(buf)
  case rv
  of ZF_OK:
    nil
  of ZF_ABORT_INTERNAL_ERROR:
    msg = "internal error"
  of ZF_ABORT_OUTSIDE_MEM:
    msg = "outside memory"
  of ZF_ABORT_DSTACK_OVERRUN:
    msg = "dstack overrun"
  of ZF_ABORT_DSTACK_UNDERRUN:
    msg = "dstack underrun"
  of ZF_ABORT_RSTACK_OVERRUN:
    msg = "rstack overrun"
  of ZF_ABORT_RSTACK_UNDERRUN:
    msg = "rstack underrun"
  of ZF_ABORT_NOT_A_WORD:
    msg = "not a word"
  of ZF_ABORT_COMPILE_ONLY_WORD:
    msg = "compile-only word"
  of ZF_ABORT_INVALID_SIZE:
    msg = "invalid size"
  of ZF_ABORT_DIVISION_BY_ZERO:
    msg = "division by zero"
  else:
    msg = "unknown error"
  if msg:
    fprintf(stderr, "\e[31m")
    if src:
      fprintf(stderr, "%s:%d: ", src, line)
    fprintf(stderr, "%s\e[0m\n", msg)
  return rv

##
##  Load given forth file
##

proc `include`*(fname: cstring) =
  var buf: array[256, char]
  var f: ref FILE = fopen(fname, "rb")
  var line: cint = 1
  if f:
    while fgets(buf, sizeof((buf)), f):
      do_eval(fname, inc(line), buf)
    fclose(f)
  else:
    fprintf(stderr, "error opening file \'%s\': %s\n", fname, strerror(errno))

##
##  Save dictionary
##

proc save*(fname: cstring) =
  var len: csize
  var p: pointer = zf_dump(addr(len))
  var f: ref FILE = fopen(fname, "wb")
  if f:
    fwrite(p, 1, len, f)
    fclose(f)

##
##  Load dictionary
##

proc load*(fname: cstring) =
  var len: csize
  var p: pointer = zf_dump(addr(len))
  var f: ref FILE = fopen(fname, "rb")
  if f:
    fread(p, 1, len, f)
    fclose(f)
  else:
    perror("read")

##
##  Sys callback function
##

proc zf_host_sys*(id: zf_syscall_id; input: cstring): zf_input_state =
  case cast[cint](id)          ##  The core system callbacks
  of ZF_SYSCALL_EMIT:
    putchar(cast[char](zf_pop()))
    fflush(stdout)
  of ZF_SYSCALL_PRINT:
    printf(ZF_CELL_FMT, " ", zf_pop())
  of ZF_SYSCALL_TELL:
    var len: zf_cell = zf_pop()
    var buf: pointer = cast[ref uint8_t](zf_dump(nil)) + cast[cint](zf_pop())
    cast[nil](fwrite(buf, 1, len, stdout))
    fflush(stdout)
  of ZF_SYSCALL_USER + 0:
    printf("\n")
    exit(0)
  of ZF_SYSCALL_USER + 1:
    zf_push(sin(zf_pop()))
  of ZF_SYSCALL_USER + 2:
    if input == nil:
      return ZF_INPUT_PASS_WORD
    `include`(input)
  of ZF_SYSCALL_USER + 3:
    save("zforth.save")
  else:
    printf("unhandled syscall %d\n", id)
  return ZF_INPUT_INTERPRET

##
##  Tracing output
##

proc zf_host_trace*(fmt: string; va: seq[string]) =
  stderr.write("\e[1;30m")
  stderr.write(va)
  stderr.write("\e[0m")

##
##  Parse number
##

proc zf_host_parse_num*(buf: string): zf_cell =
  var v: zf_cell
  var r: cint = sscanf(buf, "%f", addr(v))
  if r == 0:
    zf_abort(ZF_ABORT_NOT_A_WORD)
  return v

proc usage*() =
  fprintf(stderr, "usage: zfort [options] [src ...]\n\nOptions:\n   -h         show help\n   -t         enable tracing\n   -l FILE    load dictionary from FILE\n")

##
##  Main
##

proc main*(argc: cint; argv: cstringArray): cint =
  var i: cint
  var c: cint
  var trace: cint = 0
  var line: cint = 0
  var fname_load: cstring = nil
  ##  Parse command line options
  while (c = getopt(argc, argv, "hl:t")) != -1:
    case c
    of 't':
      trace = 1
    of 'l':
      fname_load = optarg
    of 'h':
      usage()
      exit(0)
  dec(argc, optind)
  inc(argv, optind)
  ##  Initialize zforth
  zf_init(trace)
  ##  Load dict from disk if requested, otherwise bootstrap fort
  ##  dictionary
  if fname_load:
    load(fname_load)
  else:
    zf_bootstrap()
  ##  Include files from command line
  i = 0
  while i < argc:
    `include`(argv[i])
    inc(i)
  ##  Interactive interpreter: read a line using readline library,
  ##  and pass to zf_eval() for evaluation
  when defined(USE_READLINE):
    read_history(".zforth.hist")
    while true:
      var buf: cstring = readline("")
      if buf == nil:
        break
      if strlen(buf) > 0:
        do_eval("stdin", inc(line), buf)
        printf("\n")
        add_history(buf)
        write_history(".zforth.hist")
  else:
    while true:
      var buf: array[4096, char]
      if fgets(buf, sizeof((buf)), stdin):
        do_eval("stdin", inc(line), buf)
        printf("\n")
      else:
        break
  return 0

##
##  End
##
