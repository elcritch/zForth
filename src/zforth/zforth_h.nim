import
  zfconf

##  Abort reasons

type
  zf_result* = enum
    ZF_OK,                    ##  0
    ZF_ABORT_INTERNAL_ERROR,  ##  1
    ZF_ABORT_OUTSIDE_MEM,     ##  2
    ZF_ABORT_DSTACK_UNDERRUN, ##  3
    ZF_ABORT_DSTACK_OVERRUN,  ##  4
    ZF_ABORT_RSTACK_UNDERRUN, ##  5
    ZF_ABORT_RSTACK_OVERRUN,  ##  6
    ZF_ABORT_NOT_A_WORD,      ##  7
    ZF_ABORT_COMPILE_ONLY_WORD, ##  8
    ZF_ABORT_INVALID_SIZE,    ##  9
    ZF_ABORT_DIVISION_BY_ZERO, ##  A
    ZF_ABORT_UNKNOWN_SYS,     ##  B
    ZF_ABORT_UNKNOWN          ##  C
  zf_mem_size* = enum
    ZF_MEM_SIZE_VAR, ZF_MEM_SIZE_CELL, ZF_MEM_SIZE_U8, ZF_MEM_SIZE_U16,
    ZF_MEM_SIZE_U32, ZF_MEM_SIZE_S8, ZF_MEM_SIZE_S16, ZF_MEM_SIZE_S32
  zf_input_state* = enum
    ZF_INPUT_INTERPRET, ZF_INPUT_PASS_CHAR, ZF_INPUT_PASS_WORD
  zf_syscall_id* = enum
    ZF_SYSCALL_EMIT, ZF_SYSCALL_PRINT, ZF_SYSCALL_TELL, ZF_SYSCALL_USER = 128





##  ZForth API functions

proc zf_init*()
proc zf_bootstrap*()
proc zf_dump*(len: ref csize): pointer
proc zf_eval*(buf: cstring): zf_result
proc zf_abort*(reason: zf_result)
proc zf_push*(v: zf_cell)
proc zf_pop*(): zf_cell
proc zf_pick*(n: zf_addr): zf_cell
proc zf_sc*(): zf_cell
##  Host provides these functions

proc zf_host_sys*(id: zf_syscall_id; last_word: cstring): zf_input_state
proc zf_host_trace*(fmt: cstring; va: va_list)
proc zf_host_parse_num*(buf: cstring): zf_cell