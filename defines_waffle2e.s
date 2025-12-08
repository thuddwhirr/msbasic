; Waffle2e MS BASIC Configuration
; Based on eater configuration but adapted for Waffle2e

; Platform identifier
WAFFLE2E := 1

; Configuration flags
CONFIG_2A := 1              ; BASIC 2.0 variant
CONFIG_SCRTCH_ORDER := 2

; Zero page allocation
; We need to avoid kernel's zero page usage ($00-$01 for pointers)
; and be careful not to conflict with user programs
ZP_START0 = $02
ZP_START1 = $04
ZP_START2 = $0C
ZP_START3 = $62
ZP_START4 = $6D

; Extra/override ZP variables
USR := GORESTART

; Constants
SPACE_FOR_GOSUB := $3E
STACK_TOP := $FA
WIDTH := 80                 ; Screen width (80 columns)
WIDTH2 := 72                ; WIDTH - 8 (for margin calculations)
RAMSTART2 := $1C00          ; BASIC program storage starts at USER_RAM (see hardware.inc)

; Memory configuration
; IMPORTANT: Set fixed memory size to avoid BASIC probing I/O space at $4000
; Memory layout:
;   $0200-$1BFF: Kernel data (~6.5KB)
;   $1C00-$3FFF: MS BASIC user RAM (~9KB for programs/variables)
;   $4000-$4FFF: I/O space
MEMTOP := $4000             ; Top of RAM (I/O starts here)
; Note: INPUTBUFFER not defined - MS BASIC will place it in zero page automatically

; Floating point size and derived constants (from defines.s)
.ifdef CONFIG_SMALL
BYTES_FP := 4
CONFIG_SMALL_ERROR := 1
.else
BYTES_FP := 5
.endif

.ifndef BYTES_PER_ELEMENT
BYTES_PER_ELEMENT := BYTES_FP
.endif
BYTES_PER_VARIABLE := BYTES_FP+2
MANTISSA_BYTES := BYTES_FP-1
BYTES_PER_FRAME := 2*BYTES_FP+8
FOR_STACK1 := 2*BYTES_FP+5
FOR_STACK2 := BYTES_FP+4

.ifndef MAX_EXPON
MAX_EXPON = 10
.endif

STACK := $0100
.ifndef STACK2
STACK2 := STACK
.endif

; INPUTBUFFER is automatically placed in zero page by MS BASIC
; High byte is $00 for zero page
INPUTBUFFERX = $00

CR=13
LF=10

.ifndef CRLF_1
CRLF_1 := CR
CRLF_2 := LF
.endif
