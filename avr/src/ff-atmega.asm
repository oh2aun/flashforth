;**********************************************************************
;                                                                     *
;    Filename:      FlashForth.asm                                    *
;    Date:          22.03.2017                                        *
;    File Version:  5.0                                               *
;    MCU:           Atmega                                            *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2017  Mikael Nordman

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License version 3 as 
; published by the Free Software Foundation.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Modified versions of FlashForth must be clearly marked as such, 
; in the name of this file, and in the identification
; displayed when FlashForth starts.
;**********************************************************************

; Include the FlashForth configuration file
.include "config.inc"

; Define the FF version date string
#define DATE "22.03.2017"


; Register definitions
  .def upl = r2         ; not in interrupt 
  .def uph = r3         ; not in interrupt
  .def r_zero = r5      ; read only zero
  .def r_one = r6       ; read only one
  .def r_two = r7       ; read only two
  .def t8 = r8          ; Not in interrupt
  .def wflags  = r9     ; not in interrupt

  .def loadreg0 = r4    ;
  .def loadreg1 = r12
  .def loadreg2 = r13


  .def ibasel=r10       ; Not in interrupt
  .def ibaseh=r11       ; Not in interrupt
  .def ms_count  = r14  ; Not in interrupt
  .def ms_count1 = r15  ; Not in interrupt
  .def t0 = r16
  .def t1 = r17
  .def t2 = r0          ; Not in interrupt
  .def t3 = r1          ; Not in interrupt

  .def al = r18
  .def ah = r19
  .def pl = r20         ; P Register and FOR..LOOP INDEX variable
  .def ph = r21

  .def FLAGS1 = r22     ; Not in interrupt
  .def FLAGS2 = r23     ; Not in interrupt
  .def tosl = r24
  .def tosh = r25
;  xl = r26
;  xh = r27
;  yl = r28  ; StackPointer Ylo
;  yh = r29  ; StackPointer Yhi
;  zl = r30
;  zh = r31
  .def t4 = r26
  .def t5 = r27
  .def t6 = r30
  .def t7 = r31

; Macros
.macro poptos 
    ld tosl, Y+
    ld tosh, Y+
.endmacro

.macro pushtos
    st -Y, tosh
    st -Y, tosl
.endmacro

.macro in_
.if (@1 < $40)
  in @0,@1
.else
  lds @0,@1
.endif
.endmacro

.macro out_
.if (@0 < $40)
  out @0,@1
.else
  sts @0,@1
.endif
.endmacro

.macro sbi_
.if (@0 < $40)
  sbi @0,@1
.else
  in_ @2,@0
  ori @2,exp2(@1)
  out_ @0,@2
.endif
.endmacro

.macro cbi_
.if (@0 < $40)
  cbi @0,@1
.else
  in_ @2,@0
  andi @2,~(exp2(@1))
  out_ @0,@2
.endif
.endmacro

.macro lpm_
.if (FLASHEND < 0x8000) ; Word address
        lpm @0,@1
.else
        elpm @0,@1
.endif
.endmacro

.macro sub_pflash_z
.if (PFLASH > 0)
        subi    zh, high(PFLASH)
.endif
.endmacro

.macro add_pflash_z
.if (PFLASH > 0)
        subi    zh, high(0x10000-PFLASH)
.endif        
.endmacro

.macro sub_pflash_tos
.if (PFLASH > 0)
        subi    tosh, high(PFLASH)
.endif
.endmacro

.macro add_pflash_tos
.if (PFLASH > 0)
        subi    tosh, high(0x10000-PFLASH)
.endif        
.endmacro

.macro rampv_to_c
.if (FLASHEND >= 0x8000)
        bset    0
.else
        bclr    0
.endif
.endmacro

.macro fdw
  .dw ((@0<<1)+PFLASH)
.endmacro

.macro m_pop_zh
.ifdef EIND
        pop     zh
.endif
.endmacro
.macro m_pop_xh
.ifdef EIND
        pop     xh
 .endif
.endmacro
.macro m_pop_t0
.ifdef EIND
        pop     t0
 .endif
.endmacro
.macro m_push_t0
.ifdef EIND
        push    t0
 .endif
.endmacro
.macro mijmp
.ifdef EIND
        eijmp
.else
        ijmp
.endif
.endmacro

; Symbol naming compatilibity
; UART0 symbols for Atmega32
.ifndef UCSR0A
.equ UCSR0A=UCSRA
.equ UDR0_=UDR
.equ UCSR0B=UCSRB
.equ UCSR0C=UCSRC
.equ RXEN0=RXEN
.equ TXEN0=TXEN
.equ RXCIE0=RXCIE
.equ UCSZ00=UCSZ0
.equ USBS0=USBS
.equ UBRR0H=UBRRH
.equ UBRR0L=UBRRL
.equ URSEL_=0x80
.else
.equ UDR0_=UDR0
.equ URSEL_=0
.endif

.ifndef SPMCSR
.equ SPMCSR=SPMCR
.endif

.ifndef SPMEN
.equ SPMEN=SELFPRGEN
.endif

.ifndef EEWE
.equ EEWE=EEPE
.endif

.ifndef EEMWE
.equ EEMWE=EEMPE
.endif

.if OPERATOR_UART == 1
.equ OP_TX_=TX1_
.equ OP_RX_=RX1_
.equ OP_RXQ=RX1Q
.else
.if OPERATOR_UART == 0
.equ OP_TX_=TX0_
.equ OP_RX_=RX0_
.equ OP_RXQ=RX0Q
.endif
.endif

#define ubrr0val (FREQ_OSC/16/BAUDRATE0) - 1
#define ubrr1val (FREQ_OSC/16/BAUDRATE1) - 1

.if FREQ_OSC < 16384000 ; Hz
.equ ms_value_tmr0 = ((FREQ_OSC/1000/64) - 1)
.equ ms_value_tmr1 = ((FREQ_OSC/1000) - 1)
.equ ms_value_tmr2 = ((FREQ_OSC/1000/64) - 1)
.ifdef TCCR0B
.equ ms_pre_tmr0   = 3
.endif
.ifdef TCCR0
.equ ms_pre_tmr0   = 4
.endif
.ifdef TCCR2B
.equ ms_pre_tmr2   = 4
.endif
.ifdef TCCR2
.equ ms_pre_tmr2   = 3
.endif

.else ; FREQ_OSC >= 16384000 Hz

.equ ms_value_tmr0 = ((FREQ_OSC/1000/256) - 1)
.equ ms_value_tmr1 = ((FREQ_OSC/1000) - 1)
.equ ms_value_tmr2 = ((FREQ_OSC/1000/128) - 1)
.ifdef TCCR0B
.equ ms_pre_tmr0   = 4
.endif
.ifdef TCCR0
.equ ms_pre_tmr0   = 6
.endif
.ifdef TCCR2B
.equ ms_pre_tmr2   = 5
.endif
.ifdef TCCR2
.equ ms_pre_tmr2   = 4
.endif
.endif
.equ CPU_LOAD_VAL  = (FREQ_OSC*255/100000)
;..............................................................................
;Program Specific Constants (literals used in code)
;..............................................................................
; Flash page size
.equ PAGESIZEB=PAGESIZE*2    ; Page size in bytes 

; Forth word header flags
.equ NFA= 0x80      ; Name field mask
.equ IMMED= 0x40    ; Immediate mask
.equ INLINE= 0x20   ; Inline mask for 1 and 2 cell code
.equ INLINE4= 0x00   ; Inline mask for 4 cell code
.equ INLINE5= 0x00   ; Inline mask for 5 cell code
.equ COMPILE= 0x10  ; Compile only mask
.equ NFAmask= 0xf   ; Name field length mask

; FLAGS2
.equ fIDLE=     6   ; 0 = busy, 1 = idle
.equ fLOAD=     5   ; Load measurement ready
.equ fLOADled=  4   ; 0 = no load led, 1 = load led on
.equ fFC_tx1=   3   ; 0=Flow Control, 1 = no Flow Control   
.equ fFC_tx0=   2   ; 0=Flow Control, 1 = no Flow Control   
.equ ixoff_tx1= 1                    
.equ ixoff_tx0= 0

; FLAGS1
.equ fLIT=    7     ; Literal compiled
.equ noclear= 6     ; dont clear optimisation flags 
.equ idup=    5     ; Use dupzeroequal instead of zeroequal
.equ izeroeq= 4     ; Use brne instead of breq if zeroequal
.equ istream= 3
.equ fLOCK=   2
.equ fTAILC=  1
.equ idirty=  0

;;; For Flow Control
.equ XON=   0x11
.equ XOFF=  0x13

.equ CR_=0x0d
.equ LF_=0x0a
.equ BS_=0x08
.equ TAB_=0x09

;;; Memory mapping prefixes
.equ PRAM    = 0x0000                 ; 8 Kbytes of ram (atm2560)
.equ PEEPROM = RAMEND+1               ; 4 Kbytes of eeprom (atm2560)
.if (FLASHEND == 0x1ffff)             ; 128 Kwords flash
.equ OFLASH  = PEEPROM+EEPROMEND+1    ; 52 Kbytes available for FlashForth(atm2560)
.equ PFLASH  = 0
.equ RAMPZV  = 3
.equ KERNEL_SIZE=0x0d80
.else
.if (FLASHEND == 0xffff)              ; 64 Kwords flash
.equ OFLASH  = PEEPROM+EEPROMEND+1    ; 56 Kbytes available for FlashForth(atm128)
.equ PFLASH  = 0
.equ RAMPZV  = 1
.equ KERNEL_SIZE=0x0d00
.else
.if (FLASHEND == 0x7fff)              ; 32 Kwords flash
.equ OFLASH = PEEPROM+EEPROMEND+1     ; 56 Kbytes available for FlashForth
.equ PFLASH = 0
.equ RAMPZV  = 0
.equ KERNEL_SIZE=0x0d00
.else
.if (FLASHEND == 0x3fff)              ; 16 Kwords flash
.equ OFLASH = 0x8000                  ; 32 Kbytes available for FlashForth
.equ PFLASH = OFLASH
.equ RAMPZV  = 0
.equ KERNEL_SIZE=0x0c80
.else
.if (FLASHEND == 0x1fff)              ; 8  Kwords flash
.equ OFLASH = 0xC000                  ; 16 Kbytes available for FlashForth
.equ PFLASH = OFLASH
.equ RAMPZV  = 0
.equ KERNEL_SIZE=0x0c80
.endif
.endif
.endif
.endif
.endif
.equ BOOT_SIZE=0x400
.equ BOOT_START=FLASHEND - BOOT_SIZE + 1  ; atm128: 0xfc00, atm328: 0x3c00 
.equ KERNEL_START=BOOT_START - KERNEL_SIZE

;;;  High values for memory areas
.equ FLASH_HI = 0xffff - (BOOT_SIZE*2) - (KERNEL_SIZE*2)
.equ EEPROM_HI =PEEPROM + EEPROMEND
.equ RAM_HI = RAMEND
        
;;; USER AREA for the OPERATOR task
.equ ursize=       RETURN_STACK_SIZE
.equ ussize=       PARAMETER_STACK_SIZE
.equ utibsize=     TIB_SIZE

;;; User variables and area
.equ us0=          -28         ; Start of parameter stack
.equ ur0=          -26         ; Start of ret stack
.equ uemit=        -24         ; User EMIT vector
.equ ukey=         -22         ; User KEY vector
.equ ukeyq=        -20         ; User KEY? vector
.equ ubase=        -18         ; Number Base
.equ utib=         -16         ; TIB address
.equ utask=        -14         ; Task area pointer
.equ ustatus=      -12
.equ uflg=         -11
.equ usource=      -10         ; Two cells
.equ utoin=        -6          ; Input stream
.equ ulink=        -4          ; Task link
.equ ursave=       -2          ; Saved ret stack pointer
.equ uhp=           0          ; Hold pointer


;;; Variables in EEPROM
.equ eeprom=       PEEPROM
.equ dp_start=     eeprom + 0x0000 ; TURNKEY
.equ dp_flash=     eeprom + 0x0002 ; FLASH dictionary pointer
.equ dp_eeprom=    eeprom + 0x0004 ; EEPROM dictionary pointer
.equ dp_ram=       eeprom + 0x0006 ; RAM dictionary pointer
.equ latest=       eeprom + 0x0008 ; Pointer to latest dictionary word
.equ prompt=       eeprom + 0x000a ; Deferred prompt
.equ ehere=        eeprom + 0x000c

;****************************************************
.dseg
ibuf:         .byte PAGESIZEB
ivec:         .byte INT_VECTORS_SIZE

rxqueue0:
rbuf0_wr:    .byte 1
rbuf0_rd:    .byte 1
rbuf0_lv:    .byte 1
rbuf0:       .byte RX0_BUF_SIZE

.ifdef UCSR1A
rxqueue1:
rbuf1_wr:    .byte 1
rbuf1_rd:    .byte 1
rbuf1_lv:    .byte 1
rbuf1:       .byte RX1_BUF_SIZE
.endif

litbuf0:    .byte 1
litbuf1:    .byte 1

dpSTART:    .byte 2
dpFLASH:    .byte 2 ; DP's and LATEST in RAM
dpEEPROM:   .byte 2
dpRAM:      .byte 2
dpLATEST:   .byte 2

iaddrl:     .byte 1
iaddrh:     .byte 1
.ifdef RAMPZ
iaddru:	    .byte 1
ibaseu:	    .byte 1
.endif

.if IDLE_MODE == 1
.if CPU_LOAD == 1       
load_acc:   .byte 3 ; Load measurement accumulator
load_res:   .byte 3 ; Load result
.endif
.endif

cse:        .byte 1 ; Current data section 0=flash, 1=eeprom, 2=ram
state:      .byte 1 ; Compilation state
uvars:      .byte   (-us0)
up0:        .byte   2
urbuf:      .byte   ursize
usbuf:      .byte   ussize
utibbuf:    .byte   utibsize
dpdata:     .byte   2

.eseg
.org 0
        .dw 0xffff  ; Force first cell of eeprom to 0xffff
;*******************************************************************
; Start of kernel
;*******************************************************************
.cseg
.if (FLASHEND == 0x1ffff)
.org 0x17e80
.else
.org KERNEL_START
.endif
;***********************************************************
CMP:
        call    TOR
        rjmp    CMP2
CMP1:
        call    NEQUALSFETCH
        call    MINUS
        call    ZEROSENSE
        breq    CMP2
        jmp     TWODROPZ
CMP2:
        call    XNEXT
        brcc    CMP1
        jmp     TWODROPNZ

.if (FLASHEND == 0x1ffff)
.org KERNEL_START+0x0
.endif
;;; *************************************************
;;; WARM user area data
.equ warmlitsize= 28
WARMLIT:
        .dw      0x0200                ; cse, state
        .dw      utibbuf-4             ; S0
        .dw      usbuf-1               ; R0
        fdw      OP_TX_
        fdw      OP_RX_
        fdw      OP_RXQ
        .dw      BASE_DEFAULT          ; BASE
        .dw      utibbuf               ; TIB
        fdw      OPERATOR_AREA         ; TASK
        .dw      0                     ; ustatus & uflg
        .dw      0                     ; source
        .dw      0                     ; source
        .dw      0                     ; TOIN
        .dw      up0                   ; Task link
; M? -- caddr count    current data space string
;        dw      L_DOTBASE
L_MEMQ:
        .db     NFA|1," "
MEMQ:
        call    CSE_
        call    DOLIT
        fdw     MEMQADDR_N
        call    PLUS
        call    FETCH_A
        call    CFETCHPP
        call    DOLIT
        .dw     NFAmask
        jmp     AND_

.if (FLASHEND == 0x1ffff)
        fdw     PAUSE_L
WDON_L:
        .db     NFA|3,"wd+"
WDON:
        cli
        wdr
        lds     tosh, WDTCSR
        ori     tosh, (1<<WDCE)|(1<<WDE)
        sts     WDTCSR, tosh
        andi    tosl, 7
        ori     tosl, (1<<WDE)
        sts     WDTCSR, tosl
        sei
        jmp     DROP

; WD- ( -- )    stop the watchdog
        fdw     WDON_L
WDOFF_L:
        .db     NFA|3,"wd-"
WDOFF:
        cli
        wdr
.ifdef MCUSR
        out     MCUSR, r_zero
.else
        out     MCUCSR, r_zero
.endif
        ldi     t0, (1<<WDCE)|(1<<WDE)
        sts     WDTCSR, t0
        sts     WDTCSR, r_zero
        sei
        ret

; WDR ( -- )    kick the dog
        fdw     WDOFF_L
CWD_L:
        .db     NFA|INLINE|3,"cwd"
CWD:
        wdr
        ret
.endif
;*********************************************************************
; EXIT --   Compile a return
;        variable link
        .dw     0
EXIT_L:
        .db     NFA|4,"exit",0
EXIT:
        m_pop_t0
        pop     t0
        pop     t0
        ret

        fdw     IFLUSH_L
OPERATOR_L:
        .db     NFA|8,"operator",0
OPERATOR:
        call    DOCREATE
        fdw     OPERATOR_AREA
OPERATOR_AREA:
        .dw     up0
        .dw     0, ursize
        .dw     ussize, utibsize

; idle
        fdw(EXIT_L)
IDLE_L:
        .db     NFA|4,"idle",0
IDLE:
        sbr     FLAGS2, (1<<fIDLE)
        ret
        
; busy
        fdw(IDLE_L)
BUSY_L:
        .db     NFA|4,"busy",0
BUSY:
        cbr     FLAGS2, (1<<fIDLE)
        ret        
; *********************************************
; Bit masking 8 bits, only for ram addresses !
; : mset ( mask addr -- )
;   dup >r c@ swap or r> c!
; ;
        fdw     ICCOMMA_L
MSET_L:
        .db     NFA|4,"mset",0
MSET:
        movw    zl, tosl
        poptos
        ld      t0, z
        or      t0, tosl
        st      z, t0
        poptos
        ret
        
; : mclr  ( mask addr -- )
;  dup >r c@ swap invert and r> c!
; ;
        fdw     MSET_L
MCLR_L:
        .db     NFA|4,"mclr",0
MCLR_:
        movw    zl, tosl
        poptos
        ld      t0, z
        com     tosl
        and     t0, tosl
        st      z, t0
        poptos
        ret

;   LSHIFT      x1 u -- x2
        fdw     MCLR_L
LSHIFT_L:
        .db     NFA|6,"lshift",0
LSHIFT:
        movw    zl, tosl
        poptos
LSHIFT1:
        sbiw    zl, 1
        brmi    LSHIFT2
        lsl     tosl
        rol     tosh
        rjmp    LSHIFT1
LSHIFT2:
        ret

;   RSHIFT      x1 u -- x2
        fdw     LSHIFT_L
RSHIFT_L:
        .db     NFA|6,"rshift",0
RSHIFT:
        movw    zl, tosl
        poptos
RSHIFT1:
        sbiw    zl, 1
        brmi    RSHIFT2
        lsr     tosh
        ror     tosl
        rjmp    RSHIFT1
RSHIFT2:
        ret

;**********************************************
NEQUALSFETCH:
        rcall   CFETCHPP
        rcall   ROT
        rcall   CFETCHPP
        rjmp    ROT
;***************************************************
; N=    c-addr nfa -- n   string:name cmp
;             n=0: s1==s2, n=ffff: s1!=s2
; N= is specificly used for finding dictionary entries
; It can also be used for comparing strings shorter than 16 characters,
; but the first string must be in ram and the second in program memory.
        fdw     RSHIFT_L
NEQUAL_L:
        .db     NFA|2,"n=",0
NEQUAL:
        rcall   NEQUALSFETCH
        andi    tosl, 0xf
        rcall   EQUAL
        rcall   ZEROSENSE
        breq    NEQUAL5
        rcall   ONEMINUS
        rcall   CFETCHPP
        rcall   TOR
        rjmp    NEQUAL4
NEQUAL2:
        rcall   NEQUALSFETCH
        rcall   NOTEQUAL
        rcall   ZEROSENSE
        breq    NEQUAL3
        rcall   TRUE_
        call    LEAVE
        rjmp    NEQUAL4
NEQUAL3:
        rcall   RFETCH
        rcall   ZEROSENSE
        brne    NEQUAL4
        rcall   FALSE_
NEQUAL4:
        call    XNEXT
        brcc    NEQUAL2
        pop     t1
        pop     t0
        rjmp    NEQUAL6
NEQUAL5:
        rcall   TRUE_
NEQUAL6:
        rcall   NIP
        jmp     NIP

; SKIP   c-addr u c -- c-addr' u'
;                          skip matching chars
; u (count) must be smaller than 256
        fdw     NEQUAL_L
SKIP_L:
        .db     NFA|4,"skip",0
SKIP:

        rcall   TOR
SKIP0:
        rcall   DUPZEROSENSE
        breq    SKIP2

        rcall   OVER
        rcall   CFETCH_A

        rcall   DUP
        rcall   DOLIT
        .dw     TAB_
        rcall   EQUAL
        rcall   ZEROSENSE
        brne    SKIP05    
        rcall   RFETCH
        rcall   EQUAL
        rcall   ZEROSENSE
        breq    SKIP2
                rjmp    SKIP1
SKIP05:
        rcall   DROP
SKIP1:
        rcall   ONE
        rcall   SLASHSTRING
        rjmp    SKIP0
SKIP2:
        pop     t0
        pop     t0
        ret


; SCAN   c-addr u c -- c-addr' u'
;                          find matching chars


        fdw     SKIP_L
SCAN_L:
        .db     NFA|4,"scan",0
SCAN:
        rcall   STORE_P_TO_R
        rcall   TOR
        rjmp    SCAN3
SCAN1:
        rcall   CFETCHPP
        rcall   DUP
        rcall   DOLIT
        .dw     TAB_
        rcall   EQUAL
        rcall   ZEROSENSE
        breq    SCAN2
        rcall   DROP
        rjmp    SCAN25
SCAN2:
        call    FETCH_P
        rcall   EQUAL
        rcall   ZEROSENSE
        breq    SCAN3
SCAN25:
        rcall   ONEMINUS
        rjmp    SCAN4
SCAN3:
        call    XNEXT
        brcc    SCAN1
SCAN4:
        rcall   RFROM
        rcall   ONEPLUS
        rcall   R_TO_P
        ret

; : mtst ( mask addr -- flag )
;   c@ and 
; ;
        fdw     SCAN_L
MTST_L:
        .db     NFA|4,"mtst",0
MTST:
        movw    zl, tosl
        ld      tosl, z+
        ld      t0, Y+
        ld      t1, Y+
        and     tosl, t0
        clr     tosh
        ret


        fdw     MTST_L
FCY_L:
        .db     NFA|3,"Fcy"
        rcall   DOCREATE
        .dw     FREQ_OSC / 1000

;;; Check parameter stack pointer
        .db     NFA|3,"sp?"
check_sp:
        rcall   SPFETCH
        call    R0_
        rcall   FETCH_A
        call    S0
        rcall   FETCH_A
        rcall   ONEPLUS
        rcall   WITHIN
        rcall   XSQUOTE
        .db     3,"SP?"
        rcall   QABORT
        ret
;***************************************************
; EMIT  c --    output character to the emit vector
        fdw     FCY_L
EMIT_L:
        .db     NFA|4,"emit",0
EMIT:
        rcall   UEMIT_
        jmp     FEXECUTE

;***************************************************
; KEY   -- c    get char from UKEY vector
        fdw     EMIT_L
KEY_L:
        .db     NFA|3,"key"
KEY:
        rcall   UKEY_
        jmp     FEXECUTE

;***************************************************
; KEY   -- c    get char from UKEY vector
        fdw     KEY_L
KEYQ_L:
        .db     NFA|4,"key?",0
KEYQ:
        rcall   UKEYQ_
        jmp     FEXECUTE

        fdw     KEYQ_L
EXECUTE_L:
        .db     NFA|7,"execute"
EXECUTE:
        movw    zl, tosl
        sub_pflash_z
        poptos
        rampv_to_c
        ror     zh
        ror     zl
        mijmp

        fdw     EXECUTE_L
FEXECUTE_L:
        .db     NFA|3,"@ex"
FEXECUTE:
        rcall   FETCH_A
        jmp     EXECUTE

        fdw     FEXECUTE_L
VARIABLE_L:
        .db     NFA|8,"variable",0
VARIABLE_:
        rcall   HERE
        rcall   CELL
        rcall   ALLOT
        jmp     CONSTANT_

        fdw     VARIABLE_L
TWOVARIABLE_L:
        .db     NFA|9,"2variable"
TWOVARIABLE_:
        rcall   HERE
        rcall   DOLIT
        .dw     0x4
        rcall   ALLOT
        jmp     CONSTANT_

        fdw     TWOVARIABLE_L
CONSTANT_L:
        .db     NFA|8,"constant",0
CONSTANT_:
        rcall   COLON
        call    LITERAL
        jmp     SEMICOLON

        fdw     CONSTANT_L
TWOCONSTANT_L:
        .db     NFA|9,"2constant"
TWOCONSTANT_:
        rcall   SWOP
        rcall   COLON
        call    LITERAL
        call    LITERAL
        jmp     SEMICOLON

; DOCREATE, code action of CREATE
; Fetch the next cell from program memory to the parameter stack
DOCREATE_L:
        .db     NFA|3, "(c)"
DOCREATE:
        m_pop_zh
        pop     zh
        pop     zl
        rcall   FETCHLIT
        m_pop_zh
        pop     zh
        pop     zl
        mijmp

;;; Resolve the runtime action of the word created by using does>
DODOES_L:
        .db     NFA|3, "(d)"
DODOES:
        m_pop_xh
        pop     xh
        pop     xl
        m_pop_zh
        pop     zh
        pop     zl
        rcall   FETCHLIT
        movw    z, x
        mijmp    ; (z)

FETCHLIT:
        pushtos
        lsl     zl
        rol     zh
        lpm_    tosl, z+
        lpm_    tosh, z+
        ret

        .db     NFA|3, "(,)"
DOCOMMAXT:
        m_pop_t0
        pop     zh
        pop     zl
        rcall   FETCHLIT
        ror     zh
        ror     zl
        push    zl
        push    zh
        m_push_t0
        rjmp     COMMAXT

;   SP@     -- addr         get parameter stack pointer
        fdw     TWOCONSTANT_L
SPFETCH_L:
        .db     NFA|3,"sp@"
SPFETCH:
        movw    z, y
        pushtos
        movw    tosl, z
        ret

;   SP!     addr --         store stack pointer
        .db     NFA|3,"sp!"
SPSTORE:
        movw    y, tosl
        ret

;   RPEMPTY     -- EMPTY THE RETURN STACK       
        .db     NFA|3,"rp0"
RPEMPTY:
        m_pop_xh
        pop     xh
        pop     xl
        call    R0_
        rcall   FETCH_A
        out     spl, tosl
        out     sph, tosh
        poptos
        movw    zl, xl
        mijmp

;   RP@ Fetch the return stack pointer        
        fdw     SPFETCH_L
RPFETCH_L:
        .db     NFA|INLINE|COMPILE|3,"rp@"
RPFETCH:
        pushtos
        in      tosl, spl
        in      tosh, sph
        ret

;   ><  Swap bytes        
        fdw     RPFETCH_L
SWAPB_L:
        .db     NFA|INLINE|2,"><",0
SWAPB:
        mov     t0, tosl
        mov     tosl, tosh
        mov     tosh, t0
        ret

; DICTIONARY POINTER FOR the current section
; Flash -- sets the data section to flash
        fdw     SWAPB_L
FLASH_L:
ROM_N:  
        .db     NFA|5,"flash"
ROM_:
        sts     cse, r_zero
        ret

; EEPROM -- sets the data section to EEPROM data memory
        fdw     FLASH_L
EEPROM_L:
EROM_N: 
        .db     NFA|6,"eeprom",0
EROM:
        sts     cse, r_two
        ret
        
; RAM -- sets the data section to RAM memory
        fdw     EEPROM_L
RAM_L:
FRAM_N: 
        .db     NFA|3,"ram"
FRAM:
        ldi     t0, 4
        sts     cse, t0
        ret

; DP    -- a-addr          
; Fetched from EEPROM
        fdw     RAM_L
DP_L:
        .db     NFA|2,"dp",0
DP:
        rcall   IDP
        rcall   CSE_
        jmp     PLUS


;;; 
        .db     NFA|3,"cse"
CSE_:
        pushtos
        lds     tosl, cse
        clr     tosh
        ret

; HERE    -- addr    get current data space ptr
;   DP @ ;
        fdw     DP_L
HERE_L:
        .db     NFA|4,"here",0
HERE:
        rcall   DP
        jmp     FETCH

; ,   x --             append cell to current data space
;   HERE ! CELL ALLOT ;
        fdw     HERE_L
COMMA_L:
        .db     NFA|1,","
COMMA:
        rcall   HERE
        rcall   STORE_A
        rcall   CELL
        jmp     ALLOT

; C,  c --             append char to current data space
;   HERE C! 1 ALLOT ;
        fdw     COMMA_L 
CCOMMA_L:
        .db     NFA|2,"c,",0
CCOMMA:
        rcall   HERE
        rcall   CSTORE_A
        rcall   ONE
        jmp     ALLOT


; CELL     -- n                 size of one cell
        fdw     CCOMMA_L
CELL_L:
        .db     NFA|4,"cell",0
CELL:
        pushtos
        ldi     tosl, 2
        ldi     tosh, 0
        ret

; ALIGN    --                         align DP
        fdw     CELL_L
ALIGN_L:
        .db     NFA|5,"align"
ALIGN:
        rcall   HERE
        rcall   ALIGNED
        rcall   DP
        jmp     STORE

; ALIGNED  addr -- a-addr       align given addr
        fdw     ALIGN_L
ALIGNED_L:
        .db     NFA|7,"aligned"
ALIGNED:
        adiw    tosl, 1
        cbr     tosl, 1
        ret

; CELL+    a-addr1 -- a-addr2      add cell size
;   2 + ;
        fdw     ALIGNED_L
CELLPLUS_L:
        .db     NFA|INLINE|5,"cell+"
CELLPLUS:
        adiw    tosl, 2
        ret

; CELLS    n1 -- n2            cells->adrs units
        fdw     CELLPLUS_L
CELLS_L:
        .db     NFA|INLINE|5,"cells"
CELLS:
        lsl     tosl
        rol     tosh
        ret

; CHAR+    c-addr1 -- c-addr2   add char size
        fdw     CELLS_L
CHARPLUS_L:
        .db     NFA|INLINE|5,"char+"
CHARPLUS:
        adiw    tosl, 1
        ret

; CHARS    n1 -- n2            chars->adrs units
        fdw     CHARPLUS_L
CHARS_L:
        .db     NFA|INLINE|5,"chars"
CHARS:  ret

        fdw     CHARS_L
COMMAXT_L:
        .db     NFA|3, "cf,"
COMMAXT:
        rcall   DUP
        rcall   IHERE
        rcall   MINUS
        rcall   ABS_ 
        rcall   DOLIT
        .dw     0xff0
        rcall   GREATER
        rcall   ZEROSENSE
        breq    STORECF1
STORECFF1: 
;        rcall   CALL_
        rcall   DOLIT
.ifdef EIND
        .dw     0x940F  ; On Atmega 2560 all code is on 128 - 256 Kword area.
.else
        .dw     0x940E  ; call jmp:0x940d
.endif
        call    ICOMMA
        sub_pflash_tos
        rampv_to_c
        ror     tosh
        ror     tosl
        rjmp    STORECF2
STORECF1:
        rcall   IHERE
        rcall   MINUS
        rcall   TWOMINUS
        rcall   TWOSLASH
        ;rcall   RCALL_
        andi    tosh, 0x0f
        ori     tosh, 0xd0
STORECF2:
        jmp    ICOMMA


; !COLON   --       change code field to docolon
;   -6 IALLOT ; 
;       .dw    link
;link   set     $
        .db     NFA|2,"!:",0
STORCOLON:
        rcall   DOLIT
        .dw     0xfffa         ;  -6
        jmp     IALLOT


; 2@    a-addr -- x1 x2            fetch 2 cells
;   DUP @ SWAP CELL+ @ ;
;   the lower address will appear on top of stack
        fdw     COMMAXT_L
TWOFETCH_L:
        .db     NFA|2,"2@",0
TWOFETCH:
        rcall   DUP
        rcall   FETCH_A
        rcall   SWOP
        rcall   CELLPLUS
        jmp     FETCH_A

; 2!    x1 x2 a-addr --            store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
        fdw     TWOFETCH_L
TWOSTORE_L:
        .db     NFA|2,"2!",0
TWOSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   CELLPLUS
        rcall   STORE_A
        jmp     STORE

; 2DROP  x1 x2 --                   drop 2 cells
;   DROP DROP ;
        fdw     TWOSTORE_L
TWODROP_L:
        .db     NFA|5,"2drop"
TWODROP:
        rcall   DROP
        jmp     DROP

; 2DUP   x1 x2 -- x1 x2 x1 x2    dup top 2 cells
;   OVER OVER ;
        fdw     TWODROP_L
TWODUP_L:
        .db     NFA|4,"2dup",0
TWODUP:
        rcall   OVER
        jmp     OVER

; 2SWAP   x1 x2 x3 x4 -- x3 x4 x1 x2    dup top 2 cells
        fdw     TWODUP_L
TWOSWAP_L:
        .db     NFA|5,"2swap"
TWOSWAP:
        rcall   ROT
        rcall   TOR
        rcall   ROT
        rcall   RFROM
        ret

; INPUT/OUTPUT ==================================

; SPACE   --                      output a space
;   BL EMIT ;
        fdw     TWOSWAP_L
SPACE_L:
        .db     NFA|5,"space"
SPACE_:  
        rcall   BL
        jmp     EMIT

; SPACES   n --                  output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
        fdw     SPACE_L
SPACES_L:
        .db     NFA|6,"spaces",0
SPACES:
SPCS1:
        rcall   DUPZEROSENSE
        breq    SPCS2
        rcall   SPACE_
        rcall   ONEMINUS
        rjmp    SPCS1
SPCS2:  jmp     DROP


; umin     u1 u2 -- u           unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
        fdw     SPACES_L
UMIN_L:
        .db     NFA|4,"umin",0
UMIN:
        rcall   TWODUP
        rcall   UGREATER
        rjmp    MINMAX

; umax    u1 u2 -- u            unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
        fdw     UMIN_L
UMAX_L:
        .db     NFA|4,"umax",0
UMAX:
        rcall   TWODUP
        rcall   ULESS
MINMAX:
        rcall   ZEROSENSE
        breq    UMAX1
        rcall   SWOP
UMAX1:  jmp     DROP

        fdw     UMAX_L
ONE_L:
        .db     NFA|INLINE4|1,"1"
ONE:
        pushtos
        ldi     tosl, 1
        ldi     tosh, 0
        ret

; ACCEPT  c-addr +n -- +n'  get line from terminal
        fdw     ONE_L
ACCEPT_L:
        .db     NFA|6,"accept",0
ACCEPT:
        rcall   OVER
        rcall   PLUS
        rcall   OVER
ACC1:
        rcall   KEY

        cpi     tosl, CR_
        brne    ACC_LF
        
        rcall   TRUE_
        rcall   FCR
        rcall   CSTORE_A
        rcall   DROP
        rjmp    ACC6
ACC_LF:
        cpi     tosl, LF_
        brne    ACC2
        rcall   DROP

        rcall   FCR
        rcall   CFETCH_A
        rcall   ZEROSENSE
        breq    ACC6
        rcall   FALSE_
        rcall   FCR
        rcall   CSTORE_A
        rjmp    ACC1
ACC2:
        rcall   FALSE_
        rcall   FCR
        rcall   CSTORE_A
        rcall   DUP
        rcall   EMIT
        rcall   DUP
        rcall   DOLIT
        .dw     BS_
        rcall   EQUAL
        rcall   ZEROSENSE
        breq    ACC3
        rcall   DROP
        rcall   ONEMINUS
        rcall   TOR
        rcall   OVER
        rcall   RFROM
        rcall   UMAX
        rjmp    ACC1
ACC3:
        rcall   OVER
        rcall   CSTORE_A
        rcall   ONEPLUS
        rcall   OVER
        rcall   UMIN
        rcall   TWODUP
        rcall   NOTEQUAL
        rcall   ZEROSENSE
        brne     ACC1
ACC6:
        rcall   NIP
        rcall   SWOP
        jmp     MINUS

        .db     NFA|3,"fcr"
FCR:
        rcall   DOUSER
        .dw     uflg


; TYPE    c-addr u --   type line to terminal u < $100
; : type for c@+ emit next drop ;

        fdw      ACCEPT_L
TYPE_L:
        .db     NFA|4,"type",0
TYPE:
        rcall   TOR
        rjmp    TYPE2       ; XFOR
TYPE1:  
        rcall   CFETCHPP
        rcall   EMIT
TYPE2:
        call    XNEXT
        brcc    TYPE1
        pop     t1
        pop     t0
        jmp     DROP


; (S"    -- c-addr u      run-time code for S"
        .db      NFA|3,"(s",0x22
XSQUOTE:
        m_pop_zh
        pop     zh
        pop     zl
        lsl     zl
        rol     zh
        lpm_    t0, z+
        pushtos
        movw    tosl, zl
        add_pflash_tos
        pushtos
        mov     tosl, t0
        clr     tosh
        add     zl, t0
        adc     zh, tosh
        adiw    zl, 1
        rampv_to_c
        ror     zh
        ror     zl
        mijmp

        fdw     TYPE_L
SQUOTE_L:
        .db      NFA|IMMED|COMPILE|2,"s",0x22,0
SQUOTE:
        rcall   DOCOMMAXT
        fdw     XSQUOTE
        rcall   ROM_
        rcall   CQUOTE
        jmp     FRAM

        fdw     SQUOTE_L
CQUOTE_L:
        .db     NFA|2,",",0x22,0
CQUOTE: 
        rcall   DOLIT
        .dw     0x22
        rcall   PARSE
        rcall   HERE
        rcall   OVER
        rcall   ONEPLUS
        rcall   ALIGNED
        rcall   ALLOT
        jmp     PLACE


        fdw     CQUOTE_L
DOTQUOTE_L:
        .db      NFA|IMMED|COMPILE|2,".",0x22,0
DOTQUOTE:
        rcall   SQUOTE
        rcall   DOCOMMAXT
        fdw     TYPE
        ret

        fdw     DOTQUOTE_L
ALLOT_L:
        .db     NFA|5,"allot"
ALLOT:
        rcall   DP
        jmp     PLUSSTORE

        fdw     ALLOT_L
DROP_L:
        .db     NFA|INLINE|4,"drop",0
DROP:
        poptos
        ret

        fdw     DROP_L
SWOP_L:
        .db     NFA|INLINE5|4,"swap",0
SWOP:
        ld      t0, y+
        ld      t1, y+
        pushtos
        movw    tosl, t0
        ret

        fdw     SWOP_L
OVER_L:
        .db     NFA|INLINE4|4,"over",0
OVER:
        pushtos
        ldd     tosl, y+2
        ldd     tosh, y+3
        ret

        fdw     OVER_L
ROT_L:
        .db     NFA|3, "rot"
ROT:
        rcall   TOR
        rcall   SWOP
        rcall   RFROM
        jmp     SWOP

        fdw     ROT_L
TOR_L:
        .db     NFA|COMPILE|2,">r",0
TOR:
        m_pop_zh
        pop     zh
        pop     zl
        push    tosl
        push    tosh
        poptos
        mijmp

        fdw     TOR_L
RFROM_L:
        .db     NFA|COMPILE|2,"r>",0
RFROM:
        m_pop_zh
        pop     zh
        pop     zl
        pushtos
        pop     tosh
        pop     tosl
        mijmp

        fdw     RFROM_L
RFETCH_L:
        .db     NFA|COMPILE|2,"r@",0
RFETCH:
        m_pop_zh
        pop     zh
        pop     zl
        pushtos
        pop     tosh
        pop     tosl
        push    tosl
        push    tosh
        mijmp

;   ABS     n   --- n1      absolute value of n
        fdw     DUP_L
ABS_L:
        .db     NFA|3,"abs"
ABS_:
        rcall   DUP
        jmp     QNEGATE

        fdw     ABS_L
PLUS_L:
        .db     NFA|INLINE4|1, "+"

PLUS:
        ld      t0, Y+        
        ld      t1, Y+
        add     tosl, t0
        adc     tosh, t1
        ret

; m+  ( d n -- d1 )
        fdw     PLUS_L
MPLUS_L:
        .db     NFA|2, "m+",0
MPLUS:
        rcall   STOD
        jmp     DPLUS

        fdw     MPLUS_L
MINUS_L:
        .db     NFA|INLINE5|1, "-"
MINUS:
        ld      t0, Y+
        ld      t1, Y+
        sub     t0, tosl
        sbc     t1, tosh
        movw    tosl, t0
        ret

FROM_LITBUF:
        lds     r0, litbuf0
        lds     r1, litbuf1
        ret
PLUSC_:
        rcall   FROM_LITBUF
        com     r0
        com     r1
        add     r0, r_one
        adc     r1, r_zero
        rcall   ANDIC1
        rjmp    MINUSC_1
MINUSC_:
        rcall   ANDIC0
MINUSC_1:
        ori     tosh, 0x50
        rcall   ICOMMA_
        rcall   DUP
        mov     tosl, r1
        rcall   ANDIC2
        ori     tosl, 0x90
        ori     tosh, 0x40
        rjmp    ICOMMA_
ANDIC0:
        rcall   FROM_LITBUF
ANDIC1:
        rcall   IDPMINUS
        rcall   IDPMINUS
        mov     tosl, r0
ANDIC2:
        mov     tosh, tosl
        swap    tosh
        andi    tosl, 0x0f
        andi    tosh, 0x0f
        ori     tosl, 0x80
        ret
ANDIC_:
        rcall   ANDIC0
        ori     tosh, 0x70
        rcall   ICOMMA_
        rcall   DUP
        mov     tosl, r1
        rcall   ANDIC2
        ori     tosl, 0x90
        ori     tosh, 0x70
        rjmp    ICOMMA_
ORIC_:
        rcall   ANDIC0
        ori     tosh, 0x60
        rcall   ICOMMA_
        rcall   DUP
        mov     tosl, r1
        rcall   ANDIC2
        ori     tosl, 0x90
        ori     tosh, 0x60
ICOMMA_:
        jmp     ICOMMA

        fdw     MINUS_L
AND_L:
        .db     NFA|INLINE4|3, "and"
AND_:
        ld      t0, Y+
        ld      t1, Y+
        and     tosl, t0
        and     tosh, t1
        ret

        fdw     AND_L
OR_L:
        .db     NFA|INLINE4|2, "or",0
OR_:
        ld      t0, Y+
        ld      t1, Y+
        or      tosl, t0
        or      tosh, t1
        ret

        fdw     OR_L
XOR_L:
        .db     NFA|INLINE4|3, "xor"
XOR_:
        ld      t0, Y+
        ld      t1, Y+
        eor     tosl, t0
        eor     tosh, t1
        ret

        fdw     XOR_L
INVERT_L:
        .db     NFA|INLINE|6, "invert",0
INVERT:
        com     tosl
        com     tosh
        ret

        fdw     INVERT_L
NEGATE_L:
        .db     NFA|6, "negate",0
NEGATE:
        com     tosl
        com     tosh
        adiw    tosl, 1
        ret

        fdw     NEGATE_L
ONEPLUS_L:
        .db     NFA|INLINE|2, "1+",0
ONEPLUS:
        adiw    tosl, 1
        ret

        fdw     ONEPLUS_L
ONEMINUS_L:
        .db     NFA|INLINE|2, "1-",0
ONEMINUS:
        sbiw    tosl, 1
        ret

        fdw     ONEMINUS_L
TWOPLUS_L:
        .db     NFA|INLINE|2, "2+",0
TWOPLUS:
        adiw    tosl, 2
        ret

        fdw     TWOPLUS_L
TOBODY_L:
        .db     NFA|INLINE|5, ">body"
TOBODY:
        adiw    tosl, 4
        ret

        fdw     TOBODY_L
TWOSTAR_L:
        .db     NFA|INLINE|2, "2*",0
TWOSTAR:
        lsl     tosl
        rol     tosh
        ret

        fdw     TWOSTAR_L
TWOSLASH_L:
        .db     NFA|INLINE|2, "2/",0
TWOSLASH:
        asr     tosh
        ror     tosl
        ret

        fdw     TWOSLASH_L
PLUSSTORE_L:
        .db     NFA|2,"+!",0
PLUSSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   FETCH_A
        rcall   PLUS
        rcall   SWOP
        jmp     STORE

        fdw     PLUSSTORE_L
WITHIN_L:
        .db     NFA|6,"within",0
WITHIN:
        rcall   OVER
        rcall   MINUS
        rcall   TOR
        rcall   MINUS
        rcall   RFROM
        jmp     ULESS

        fdw     WITHIN_L
NOTEQUAL_L:
        .db     NFA|2,"<>",0
NOTEQUAL:
        rcall   EQUAL
        jmp     ZEROEQUAL

        fdw     ZEROLESS_L
EQUAL_L:
        .db     NFA|1, "="
EQUAL:
        rcall   MINUS
        jmp     ZEROEQUAL


        fdw     EQUAL_L
LESS_L:
        .db     NFA|1,"<"
LESS:
        rcall   MINUS
        jmp     ZEROLESS

        fdw     LESS_L
GREATER_L:
        .db     NFA|1,">"
GREATER:
        rcall   SWOP
        jmp     LESS

        fdw     GREATER_L
ULESS_L:
        .db     NFA|2,"u<",0
ULESS:
        rcall   MINUS       ; Carry is valid after MINUS
        sbc     tosl, tosl
        sbc     tosh, tosh
        ret

        fdw     ULESS_L
UGREATER_L:
        .db     NFA|2, "u>",0
UGREATER:
        rcall   SWOP
        jmp     ULESS

        fdw     UGREATER_L
STORE_P_L:
        .db     NFA|2,"!p",0
STORE_P:
        movw    pl, tosl
        poptos
        ret

        fdw     STORE_P_L
STORE_P_TO_R_L:
        .db     NFA|COMPILE|4,"!p>r",0
STORE_P_TO_R:
        m_pop_zh
        pop     zh
        pop     zl
        push    pl
        push    ph
        movw    pl, tosl
        poptos
        mijmp

        fdw     STORE_P_TO_R_L
R_TO_P_L:
        .db     NFA|COMPILE|3,"r>p"
R_TO_P:
        m_pop_zh
        pop     zh
        pop     zl
        pop     ph
        pop     pl
        mijmp

        fdw     R_TO_P_L
PFETCH_L:
        .db     NFA|2,"p@",0
PFETCH:
        pushtos
        movw    tosl, pl
        jmp     FETCH

        fdw     PFETCH_L
PSTORE_L:
        .db     NFA|2,"p!",0
PSTORE:
        pushtos
        movw    tosl, pl
        jmp     STORE

        fdw     PSTORE_L
PCSTORE_L:
        .db     NFA|3,"pc!"
PCSTORE:
        pushtos
        movw    tosl, pl
        jmp     CSTORE

        fdw     PCSTORE_L
PPLUS_L:
        .db     NFA|INLINE|2,"p+",0
PPLUS:
        add     pl, r_one
        adc     ph, r_zero
        ret   

        fdw     PPLUS_L
PNPLUS_L:
        .db     NFA|3,"p++"
PNPLUS:
        add     pl, tosl
        adc     ph, tosh
        poptos
        ret

        fdw     PNPLUS_L
UEMIT_L:
        .db     NFA|5,"'emit"
UEMIT_:
        rcall   DOUSER
        .dw     uemit
        
        fdw     UEMIT_L
UKEY_L:
        .db     NFA|4,"'key",0
UKEY_:
        rcall   DOUSER
        .dw     ukey

        fdw     UKEY_L
UKEYQ_L:
        .db     NFA|5,"'key?"
UKEYQ_:
        rcall   DOUSER
        .dw     ukeyq

        .db     NFA|3,"?0="
ZEROSENSE:
        sbiw    tosl, 0
        poptos
        ret

        .db     NFA|3,"d0="
DUPZEROSENSE:
        sbiw    tosl, 0
        ret

        fdw     UKEYQ_L
UMSTAR_L:
        .db     NFA|3,"um*"
UMSTAR:
        jmp     umstar0

        fdw     UMSTAR_L
UMSLASHMOD_L:
        .db     NFA|6,"um/mod",0
UMSLASHMOD:
        jmp     umslashmod0


        fdw     UMSLASHMOD_L
USLASHMOD_L:
        .db     NFA|5,"u/mod"
USLASHMOD:
        rcall   FALSE_
        rcall   SWOP
        jmp     umslashmod0

        fdw     USLASHMOD_L
STAR_L:
        .db     NFA|1,"*"
STAR: 
        rcall   UMSTAR
        jmp     DROP

        fdw     STAR_L
USLASH_L:
        .db     NFA|2,"u/",0
USLASH:
        rcall   USLASHMOD
        jmp     NIP

        fdw     USLASH_L
USSMOD_L:
        .db     NFA|6,"u*/mod",0
USSMOD:
        rcall   TOR
        rcall   UMSTAR
        rcall   RFROM
        jmp     UMSLASHMOD


        fdw     USSMOD_L
SLASH_L:
        .db     NFA|1,"/"
SLASH: 
        rcall   TWODUP
        rcall   XOR_
        rcall   TOR
        rcall   ABS_
        rcall   SWOP
        rcall   ABS_
        rcall   SWOP
        rcall   USLASH
        rcall   RFROM
        jmp     QNEGATE

        fdw     SLASH_L
NIP_L:
        .db     NFA|INLINE|3,"nip"
NIP:
        ld      t0, y+
        ld      t0, y+
        ret
    
        fdw     NIP_L
TUCK_L:
        .db     NFA|4,"tuck",0
TUCK:
        rcall   SWOP
        jmp     OVER

        fdw     TUCK_L
QNEGATE_L:
        .db     NFA|7,"?negate"
QNEGATE:
        rcall   ZEROLESS
        rcall   ZEROSENSE
        breq    QNEGATE1
        rcall   NEGATE
QNEGATE1:
        ret

        fdw     QNEGATE_L
MAX_L:
        .db     NFA|3,"max"
MAX:    rcall   TWODUP
        rcall   LESS
        rjmp    MINMAX

        fdw     MAX_L
MIN_L:
        .db     NFA|3,"min"
MIN:    rcall   TWODUP
        rcall   GREATER
        rjmp    MINMAX

        .db     NFA|2,"c@",0
CFETCH_A:       
        jmp     CFETCH

        .db     NFA|2,"c!",0
CSTORE_A:       
        jmp     CSTORE

        fdw     MIN_L
UPTR_L:
        .db     NFA|2,"up",0
UPTR:   rcall   DOCREATE
        .dw     2 ; upl

        fdw     UPTR_L
HOLD_L:
        .db     NFA|4,"hold",0
HOLD:   rcall   TRUE_
        rcall   HP
        rcall   PLUSSTORE
        rcall   HP
        rcall   FETCH_A
        jmp     CSTORE

; <#    --              begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
        fdw     HOLD_L
LESSNUM_L:
        .db     NFA|2,"<#",0
LESSNUM: 
        rcall   PAD
        rcall   HP
        jmp     STORE

; digit   n -- c            convert to 0..9a..z
        fdw     LESSNUM_L
TODIGIT_L:
        .db     NFA|5,"digit"
TODIGIT:
        cpi     tosl, 0xa
        brlt    TODIGIT1
        adiw    tosl, 0x27
TODIGIT1:
        adiw    tosl, 0x30
        ret

; #     ud1 -- ud2     convert 1 digit of output
;   base @ ud/mod rot >digit hold ;
        fdw     TODIGIT_L
NUM_L:
        .db     NFA|1,"#"
NUM:
        rcall   BASE
        rcall   FETCH_A
        rcall   UDSLASHMOD
        rcall   ROT
        rcall   TODIGIT
        jmp     HOLD

; #S    ud1 -- ud2      convert remaining digits
;   begin # 2dup or 0= until ;
        fdw     NUM_L
NUMS_L:
        .db     NFA|2,"#s",0
NUMS:
        rcall   NUM
        rcall   TWODUP
        rcall   OR_
        rcall   ZEROSENSE
        brne    NUMS
        ret

; #>    ud1 -- c-addr u    end conv., get string
;   2drop hp @ pad over - ;
        fdw     NUMS_L
NUMGREATER_L:
        .db     NFA|2,"#>", 0
NUMGREATER:
        rcall   TWODROP
        rcall   HP
        rcall   FETCH_A
        rcall   PAD
        rcall   OVER
        jmp     MINUS

; SIGN  n --               add minus sign if n<0
;   0< IF 2D HOLD THEN ; 
        fdw     NUMGREATER_L
SIGN_L:
        .db     NFA|4,"sign",0
SIGN:
        cpi     tosh, 0
        brpl    SIGN1
        rcall   DOLIT
        .dw     0x2D
        rcall   HOLD
SIGN1:
        jmp     DROP

; U.    u --                  display u unsigned
;   <# 0 #S #> TYPE SPACE ;
        fdw     SIGN_L
UDOT_L:
        .db     NFA|2,"u.",0
UDOT:
        rcall   LESSNUM
        rcall   FALSE_
        rcall   NUMS
        rcall   NUMGREATER
        rcall   TYPE
        jmp     SPACE_


; U.R    u +n --      display u unsigned in field of n. 1<n<=255 
;    0 swap <# 1- for # next #s #> type space ;
        fdw     UDOT_L
UDOTR_L:
        .db     NFA|3,"u.r"
UDOTR:
        rcall   LESSNUM
        rcall   ONEMINUS
        rcall   TOR
        rcall   FALSE_
        rjmp    UDOTR2
UDOTR1:
        rcall   NUM
UDOTR2: 
        rcall   XNEXT
        brcc    UDOTR1
        pop     t1
        pop     t0
        rcall   NUMS
        rcall   NUMGREATER
        rcall   TYPE
        jmp     SPACE_

; .     n --                    display n signed
;   <# DUP ABS #S SWAP SIGN #> TYPE SPACE ;
        fdw     UDOTR_L
DOT_L:
        .db     NFA|1,"."
DOT:    rcall   LESSNUM
        rcall   DUP
        rcall   ABS_
        rcall   FALSE_
        rcall   NUMS
        rcall   ROT
        rcall   SIGN
        rcall   NUMGREATER
        rcall   TYPE
        jmp     SPACE_

        FDW     DOT_L
DECIMAL_L:
        .db     NFA|7,"decimal"
DECIMAL: 
        rcall   TEN
        rcall   BASE
        jmp     STORE

; HEX     --              set number base to hex
;   #16 BASE ! ;
        Fdw     DECIMAL_l
HEX_L:
        .db     NFA|3,"hex"
HEX:
        rcall   DOLIT
        .dw     16
        rcall   BASE
        jmp     STORE

; BIN     --              set number base to binary
;   #2 BASE ! ;
        Fdw     HEX_L
BIN_L:
        .db     NFA|3,"bin"
BIN:    rcall   CELL
        rcall   BASE
        jmp     STORE

; RSAVE   -- a-addr     Saved return stack pointer
        fdw     BIN_L
RSAVE_L:
        .db     NFA|5,"rsave"
RSAVE_: rcall   DOUSER
        .dw     ursave


; ULINK   -- a-addr     link to next task
        fdw     RSAVE_L
ULINK_L:
        .db     NFA|5,"ulink"
ULINK_: rcall   DOUSER
        .dw     ulink


; TASK       -- a-addr              TASK pointer
        fdw     ULINK_L
TASK_L:
        .db     NFA|4,"task",0
TASK:   rcall   DOUSER
        .dw     utask


; HP       -- a-addr                HOLD pointer
        fdw     TASK_L
HP_L:
        .db     NFA|2,"hp",0
HP:     rcall   DOUSER
        .dw     uhp

; PAD     -- a-addr        User Pad buffer
        fdw     HP_L
PAD_L:
        .db     NFA|3,"pad"
PAD:
        rcall   TIB
        rcall   TIBSIZE
        jmp     PLUS

; BASE    -- a-addr       holds conversion radix
        fdw     PAD_L
BASE_L:
        .db     NFA|4,"base",0
BASE:
        rcall   DOUSER
        .dw     ubase

; USER   n --
        fdw     BASE_L
USER_L:
        .db     NFA|4,"user",0
USER:
        rcall   CREATE
        rcall   CELL
        rcall   NEGATE
        rcall   IALLOT
        rcall   ICOMMA_
        rcall   XDOES
DOUSER:
        m_pop_zh
        pop     zh
        pop     zl
        rcall   FETCHLIT
        add     tosl, upl
        adc     tosh, uph
        ret

; SOURCE   -- adr n         current input buffer
;   'SOURCE 2@ ;        length is at higher adrs
        fdw     USER_L
SOURCE_L:
        .db     NFA|6,"source",0
SOURCE:
        rcall   TICKSOURCE
        jmp     TWOFETCH


; /STRING  a u n -- a+n u-n          trim string
;   swap over - >r + r>
        fdw      SOURCE_L
SLASHSTRING_L:
        .db     NFA|7,"/string"
SLASHSTRING:
        rcall   SWOP
        rcall   OVER
        rcall   MINUS
        rcall   TOR
        rcall   PLUS
        rcall   RFROM
        ret

; \     Skip the rest of the line
        fdw     SLASHSTRING_L
BSLASH_L:
        .db     NFA|IMMED|1,0x5c
BSLASH:
        rcall   SOURCE
        rcall   TOIN
        rcall   STORE_A
        sbr     FLAGS1, (1<<noclear)  ; dont clear flags in case of \
        jmp     DROP

; PARSE  char -- c-addr u
        fdw     BSLASH_L
PARSE_L:
        .db     NFA|5,"parse"
PARSE:
        rcall   DUP             ; c c
        rcall   SOURCE          ; c c a u
        rcall   TOIN            ; c c a u a
        rcall   FETCH_A         ; c c a u n
        rcall   SLASHSTRING     ; c c a u   new tib addr/len
        push    tosl
        push    tosh            ; c c a u                  R: u (new tib len
        rcall   ROT             ; c a u c
        rcall   SKIP            ; c a u        
        rcall   OVER            ; c a u a
        rcall   TOR             ; c a u                    R: u a (start of word
        rcall   ROT             ; a u c
        rcall   SCAN            ; a u      end of word, tib left       
        rcall   DUPZEROSENSE
        breq    PARSE1
        rcall   ONEMINUS
PARSE1: rcall   RFROM           ; a u a
        rcall   RFROM           ; a u a u
        rcall   ROT             ; a a u u
        rcall   MINUS           ; a a n  ( addition to toin
        rcall   TOIN
        rcall   PLUSSTORE       ; aend astart
        rcall   TUCK            ; astart aend astart
        jmp     MINUS           ; astart wlen
     

; WORD   char -- c-addr        word delimited by char and/or TAB
        fdw     PARSE_L
WORD_L:
        .db     NFA|4,"word",0
WORD:
        rcall   PARSE           ; c-addr wlen
        rcall   SWOP
        rcall   ONEMINUS
        rcall   TUCK
        jmp     CSTORE          ; Write the length into the TIB ! 

; CMOVE  src dst u --  copy u bytes from src to dst
; cmove swap !p for c@+ pc! p+ next drop ;
        fdw     WORD_L
CMOVE_L:
        .db     NFA|5,"cmove"
CMOVE:
        rcall   SWOP
        rcall   STORE_P_TO_R
        rcall   TOR
        rjmp    CMOVE2
CMOVE1:
        rcall   CFETCHPP
        rcall   PCSTORE
        rcall   PPLUS
CMOVE2:
        rcall   XNEXT
        brcc    CMOVE1
        pop     t1
        pop     t0
        rcall   R_TO_P
        jmp     DROP


; place  src n dst --     place as counted str
        fdw     CMOVE_L
PLACE_L:
        .db     NFA|5,"place"
PLACE: 
        rcall   TWODUP
        rcall   CSTORE_A
        rcall   CHARPLUS
        rcall   SWOP
        jmp     CMOVE

; :     c@+ ( addr -- addr+1 n ) dup 1+ swap c@ ;
        fdw     PLACE_L
CFETCHPP_L:
        .db     NFA|3,"c@+"
CFETCHPP:
        rcall   DUP
        rcall   ONEPLUS
        rcall   SWOP
        jmp     CFETCH

; :     @+ ( addr -- addr+2 n ) dup 2+ swap @ ;
        fdw     CFETCHPP_L
FETCHPP_L:
        .db     NFA|2,"@+",0
FETCHPP:
        rcall   DUP
        rcall   TWOPLUS
        rcall   SWOP
        jmp     FETCH

        .db     NFA|1,"!"
STORE_A:        
        jmp     STORE

; N>C   nfa -- cfa    name adr -> code field
        fdw    FETCHPP_L
NTOC_L:
        .db     NFA|3,"n>c"
NFATOCFA:
        rcall   CFETCHPP
        andi    tosl, 0x0f
        rcall   PLUS
        jmp     ALIGNED

; C>N   cfa -- nfa    code field addr -> name field addr
        fdw    NTOC_L
CTON_L:
        .db     NFA|3,"c>n"
CFATONFA:
        rcall   TWOMINUS
        rcall   DUP
        rcall   CFETCH_A
        call    TO_A
        sbrs    al, 7
        breq    CFATONFA
        ret

; findi   c-addr nfa -- c-addr 0   if not found
;                          xt  1      if immediate
;                          xt -1      if "normal"
        fdw     CTON_L
BRACFIND_L:
        .db     NFA|3,"(f)"
findi:
findi1:
FIND_1: 
        rcall   TWODUP
        rcall   NEQUAL
        rcall   DUPZEROSENSE
        breq    findi2
        rcall   DROP
        rcall   TWOMINUS ;;;      NFATOLFA
        rcall   FETCH_A
        rcall   DUP
findi2:
        rcall   ZEROSENSE
        brne    findi1
        rcall   DUPZEROSENSE
        breq    findi3
        rcall   NIP
        rcall   DUP
        rcall   NFATOCFA
        rcall   SWOP
        rcall   IMMEDQ
        rcall   ZEROEQUAL
        rcall   ONE
        rcall   OR_
findi3: 
        ret

; IMMED?    nfa -- f        fetch immediate flag
        fdw     BRACFIND_L
IMMEDQ_L:
        .db     NFA|6,"immed?",0
IMMEDQ: 
        rcall   CFETCH_A
        mov     wflags, tosl  ; COMPILE and INLINE flags for the compiler
        andi    tosl, IMMED
        ret

; FIND   c-addr -- c-addr 0   if not found
;                  xt  1      if immediate
;                  xt -1      if "normal"
        fdw     IMMEDQ_L
FIND_L:
        .db     NFA|4,"find",0
FIND:   
        rcall   DOLIT
        fdw     kernellink
        rcall   findi
        rcall   DUPZEROSENSE
        brne    FIND1
        rcall   DROP
        rcall   LATEST_
        rcall   FETCH_A
        rcall   findi
FIND1:
        ret

; DIGIT?   c -- n -1   if c is a valid digit
        fdw     FIND_L
DIGITQ_L:
        .db     NFA|6,"digit?",0
DIGITQ:
                                ; 1 = 0x31    a = 0x61
        cpi     tosl, 0x40
        brlt    DIGITQ1
        sbiw    tosl, 0x27
DIGITQ1:        
        sbiw    tosl, 0x30      ; 1
        brpl    DIGITQ2
        rjmp    FALSE_
DIGITQ2:
        rcall   DUP             ; 1 1
        rcall   BASE            ; 1 1 base
        rcall   FETCH_A         ; 1 1 10
        jmp     LESS            ; 1 ffff


; SIGN?   adr n -- adr' n' f   get optional sign
; + leaves $0000 flag
; - leaves $0002 flag
        fdw     DIGITQ_L
SIGNQ_L:
        .db     NFA|5,"sign?"
SIGNQ:
        rcall   OVER
        rcall   CFETCH_A
        mov     t0, tosl
        rcall   DROP
        cpi     t0, '-'
        breq    SIGNQMINUS
        cpi     t0, '+'
        breq    SIGNQPLUS
        rjmp    SIGNQEND
SIGNQMINUS:
        rcall   SLASHONE
        rjmp    TRUE_
SIGNQPLUS:
        rcall   SLASHONE
SIGNQEND:
        jmp     FALSE_
SLASHONE:
        rcall   ONE
        jmp     SLASHSTRING

; UD*  ud u -- ud
        fdw     SIGNQ_L
UDSTAR_L:
        .db     NFA|3,"ud*"
UDSTAR:
        push    tosl
        push    tosh
        rcall   UMSTAR
        rcall   DROP
        rcall   SWOP
        rcall   RFROM
        rcall   UMSTAR
        rcall   ROT
        jmp     PLUS
        
; UD/MOD  ud u --u(rem) ud(quot)
        fdw     UDSTAR_L
UDSLASHMOD_L:
        .db     NFA|6,"ud/mod",0
UDSLASHMOD:
        rcall   TOR             ; ud.l ud.h 
        rcall   FALSE_          ; ud.l ud.h 0
        rcall   RFETCH          ; ud.l ud.h 0 u
        rcall   UMSLASHMOD      ; ud.l r.h q.h
        rcall   ROT             ; r.h q.h ud.l
        rcall   ROT             ; q.h ud.l r.h
        rcall   RFROM           ; q.h ud.l r.h u
        rcall   UMSLASHMOD      ; q.h r.l q.l
        jmp     ROT             ; r.l q.l q.h
        
; >NUMBER  0 0 adr u -- ud.l ud.h adr' u'
;                       convert string to number
        fdw     UDSLASHMOD_L
TONUMBER_L:
        .db     NFA|7,">number"
TONUMBER:
        ldi     al, 1
TONUM1:
        rcall   DUPZEROSENSE      ; ud.l ud.h adr u
        breq    TONUM3
        rcall   TOR
        push    tosl             ; dup >r
        push    tosh             ; ud.l ud.h adr
        rcall   CFETCH_A
        cpi     tosl, '.'
        breq    TONUM_SKIP
        rcall   DIGITQ          ; ud.l ud.h digit flag
        rcall   ZEROSENSE
        brne    TONUM2
        rcall   DROP
        rcall   RFROM
        rcall   RFROM
        rjmp    TONUM3
TONUM2: 
        rcall   TOR             ; ud.l ud.h digit
        rcall   BASE
        rcall   FETCH_A
        rcall   UDSTAR
        rcall   RFROM
        rcall   MPLUS
        ldi     al, 0
        rjmp    TONUM_CONT
TONUM_SKIP:
        rcall   DROP
TONUM_CONT:
        rcall   RFROM
        rcall   RFROM
        rcall   SLASHONE
        rjmp    TONUM1
TONUM3:
        add     tosl, al
        ret

; NUMBER?  c-addr -- n 1
;                 -- dl dh 2
;                 -- c-addr 0  if convert error
        fdw     TONUMBER_L
NUMBERQ_L:
        .db     NFA|7,"number?"
NUMBERQ:
        rcall   DUP             ; a a
        rcall   FALSE_          ; a a 0 0
        rcall   FALSE_          ; a a 0 0
        rcall   ROT             ; a 0 0 a
        rcall   CFETCHPP        ; a 0 0 a' u
        rcall   SIGNQ           ; a 0 0 a' u f
        rcall   TOR             ; a 0 0 a' u

        rcall   BASE
        rcall   FETCH_A
        rcall   TOR             ; a 0 0 a' u
        
        rcall   OVER
        rcall   CFETCH_A
        
        sbiw    tosl, '#'
        cpi     tosl, 3
        brsh    BASEQ1

        rcall   CELLS
        rcall   DOLIT
        fdw     BASEQV
        rcall   PLUS
        rcall   FEXECUTE

        rcall   SLASHONE
        rjmp    BASEQ2
BASEQ1:
        rcall   DROP
BASEQ2:                         ; a 0 0 a' u
        rcall   TONUMBER        ; a ud.l ud.h  a' u
        rcall   RFROM           ; a ud.l ud.h  a' u oldbase
        rcall   BASE            ; a ud.l ud.h  a' u oldbase addr
        rcall   STORE_A         ; a ud.l ud.h  a' u
        rcall   ZEROSENSE       ; a ud.l ud.h  a' u
        breq    QNUMD
QNUM_ERR:                       ; Not a number
        rcall   RFROM           ; a ud.l ud.h a' u sign
        rcall   TWODROP
        rcall   TWODROP
        rcall   FALSE_          ; a 0           Not a number
        rjmp    QNUM3
QNUMD:                          ; Single or Double number
                                ; a ud.l ud.h a'
        sbiw    tosl, 1
        rcall   CFETCH_A        ; a ud.l ud.h c
        call    TO_A
        rcall   RFROM           ; a a' u ud.l ud.d sign
        rcall   ZEROSENSE
        breq    QNUMD1
        rcall   DNEGATE
QNUMD1:
        cpi     al, '.'         ; a d.l d.h
        brne    QNUM1
        rcall   ROT             ; d.l d.h a
        ldi     tosl, 2
        ldi     tosh, 0         ; d.l d.h 2    Double number
        rjmp    QNUM3
QNUM1:                          ; single precision dumber
                                ; a d.l d.h
        rcall   DROP            ; a n
        rcall   NIP             ; n
        rcall   ONE             ; n 1           Single number
QNUM3:  
        ret


        .db     NFA|4,"swap",0
SWOP_A:
        jmp     SWOP

; TI#  -- n                      size of TIB
; : ti# task @ 8 + @ ;
        fdw     NUMBERQ_L
TIBSIZE_L:
        .db     NFA|3,"ti#"
TIBSIZE:
        rcall   TASK
        rcall   FETCH_A
        adiw    tosl, 8
        jmp     FETCH

; TIB     -- a-addr        Terminal Input Buffer
        fdw     TIBSIZE_L
TIB_L:
        .db     NFA|3,"tib"
TIB:
        rcall   TIU
        jmp     FETCH
        
; TIU     -- a-addr        Terminal Input Buffer user variable 
        fdw     TIB_L
TIU_L:
        .db     NFA|3,"tiu"
TIU:
        rcall   DOUSER
        .dw     utib       ; pointer to Terminal input buffer

; >IN     -- a-addr        holds offset into TIB
; In RAM
        fdw     TIU_L
TOIN_L:
        .db     NFA|3,">in"
TOIN:
        rcall   DOUSER
        .dw     utoin

; 'SOURCE  -- a-addr        two cells: len, adrs
; In RAM ?
        fdw     TOIN_L
TICKSOURCE_L:
        .db     NFA|7,"'source"
TICKSOURCE:
        rcall   DOUSER
        .dw     usource       ; two cells !!!!!!

WORDQ:
        rcall   DUP
        m_pop_t0
        pop     zh
        pop     zl
        rcall   FETCHLIT
        ror     zh
        ror     zl
        rcall   EQUAL
        rcall   ZEROSENSE
        mijmp

;  INTERPRET  c-addr u --    interpret given buffer
        fdw     TICKSOURCE_L
INTERPRET_L:
        .db     NFA|9,"interpret"
INTERPRET: 
        rcall   TICKSOURCE
        rcall   TWOSTORE
        rcall   FALSE_
        rcall   TOIN
        rcall   STORE_A
IPARSEWORD:
        rcall   INIT_012
        rcall   BL
        rcall   WORD

        rcall   DUP
        rcall   CFETCH_A
        rcall   ZEROSENSE
        brne    IPARSEWORD1
        rjmp    INOWORD
IPARSEWORD1:
        rcall   FIND            ; sets also wflags
        rcall   DUPZEROSENSE    ; 0 = not found, -1 = normal, 1 = immediate
        brne    IPARSEWORD2     ; NUMBER?
        rjmp    INUMBER
IPARSEWORD2:
        rcall   ONEPLUS         ; 0 = normal 2 = immediate
        rcall   STATE_
        rcall   ZEROEQUAL
        rcall   OR_
        rcall   ZEROSENSE
        breq    ICOMPILE_1      ; Compile a word
        
                                ; Execute a word
                                ; immediate&compiling or interpreting
        sbrs    wflags, 4       ; Compile only check
        rjmp    IEXECUTE        ; Not a compile only word
        rcall   STATE_          ; Compile only word check
        rcall   XSQUOTE
        .db     12,"COMPILE ONLY",0
        rcall   QABORT
IEXECUTE:
        cbr     FLAGS1, (1<<noclear)
        rcall   EXECUTE
        sbrc    FLAGS1, noclear ;  set by \ and by (
        rjmp    IPARSEWORD
        cbr     FLAGS1, (1<<izeroeq) ; Clear 0= encountered in compilation
        cbr     FLAGS1, (1<<idup)    ; Clear DUP encountered in compilation
        rjmp    ICLRFLIT
ICOMPILE_1:
        cbr     FLAGS1, (1<<izeroeq) ; Clear 0= encountered in compilation
        rcall   WORDQ
        fdw     ZEROEQUAL       ; Check for 0=, modifies IF and UNTIL to use bnz
        breq    ICOMPILE_2
        sbr     FLAGS1, (1<<izeroeq) ; Mark 0= encountered in compilation
        rjmp    ICOMMAXT
ICOMPILE_2:
        sbrs    FLAGS1, fLIT
        rjmp    ICOMPILE_6
        rcall   WORDQ
        fdw     AND_    
        breq    ICOMPILE_3
        rcall   ANDIC_
        rjmp    ICLRFLIT
ICOMPILE_3:
        rcall   WORDQ
        fdw     OR_
        breq    ICOMPILE_4
        rcall   ORIC_
        rjmp    ICLRFLIT
ICOMPILE_4:
        rcall   WORDQ
        fdw     PLUS
        breq    ICOMPILE_5
        rcall   PLUSC_
        rjmp    ICLRFLIT
ICOMPILE_5:
        rcall   WORDQ
        fdw     MINUS
        breq    ICOMPILE_6
        rcall   MINUSC_
        rjmp    ICLRFLIT
ICOMPILE_6:
        cbr     FLAGS1, (1<<idup)    ; Clear DUP encountered in compilation
        rcall   WORDQ
        fdw     DUP             ; Check for DUP, modies IF and UNTIl to use DUPZEROSENSE
        breq    ICOMPILE
        sbr     FLAGS1, (1<<idup)    ; Mark DUP encountered during compilation
ICOMPILE:
        sbrs    wflags, 5       ; Inline check
        rjmp    ICOMMAXT
        call    INLINE0
        rjmp    ICLRFLIT
ICOMMAXT:
        rcall   COMMAXT_A
        cbr     FLAGS1, (1<<fTAILC)  ; Allow tailjmp  optimisation
        sbrc    wflags, 4            ; Compile only ?
        sbr     FLAGS1, (1<<fTAILC)  ; Prevent tailjmp  optimisation
ICLRFLIT:
        cbr     FLAGS1, (1<<fLIT)
        rjmp    IPARSEWORD
INUMBER: 
        cbr     FLAGS1, (1<<izeroeq) | (1<<idup) | (1<<fLIT)
        rcall   DROP
        rcall   NUMBERQ
        rcall   DUPZEROSENSE
        breq    IUNKNOWN
        rcall   STATE_
        rcall   ZEROSENSE
        breq    INUMBER1
        mov     t0, tosl
        poptos
        sbrs    t0, 1
        rjmp    ISINGLE
IDOUBLE:
        rcall   SWOP_A
        call    LITERAL
ISINGLE:        
        call    LITERAL
        rjmp    IPARSEWORD

INUMBER1:
        rcall   DROP
        rjmp    ICLRFLIT

IUNKNOWN:
        rcall   DROP 
        rcall   DP_TO_RAM
        rcall   CFETCHPP
        rcall   TYPE
        rcall   FALSE_
        rcall   QABORTQ         ; Never returns & resets the stacks
INOWORD: 
        rcall   INIT_012
        jmp     DROP

        .db     NFA|1,"@"
FETCH_A:        
        jmp     FETCH

;;;    bitmask -- 
        fdw     INTERPRET_L
SHB_L:
        .db     NFA|3,"shb"     ; Set header bit
SHB:
        rcall   LATEST_
        rcall   FETCH_A
        rcall   DUP
        rcall   CFETCH_A
        rcall   ROT
        rcall   OR_
        rcall   SWOP_A
        jmp     CSTORE
        
        fdw     SHB_L
IMMEDIATE_L:
        .db     NFA|9,"immediate" ; 
IMMEDIATE:
        rcall   DOLIT
        .dw     IMMED
        jmp     SHB

;***************************************************************
        fdw     IMMEDIATE_L
INLINED_L:
        .db     NFA|7,"inlined" ; 
INLINED:
        rcall   DOLIT
        .dw     INLINE
        jmp     SHB

;; .st ( -- ) output a string with current data section and current base info
;;; : .st base @ dup decimal <#  [char] , hold #s  [char] < hold #> type 
;;;     <# [char] > hold cse @ #s #> type base ! ;
        fdw     INLINED_L
DOTSTATUS_L:
        .db     NFA|3,".st"
DOTSTATUS:
        rcall   DOLIT
        .dw     '<'
        rcall   EMIT
        call    DOTBASE
        rcall   EMIT
        rcall   DOLIT
        .dw     ','
        rcall   EMIT
        call    MEMQ
        rcall   TYPE
        rcall   DOLIT
        .dw     '>'
        rcall   EMIT
        jmp     DOTS

        .db     NFA|2,">r",0
TOR_A:  jmp     TOR


;;; TEN ( -- n ) Leave decimal 10 on the stack
;        .db     NFA|1,"a"
TEN:
        rcall   DOCREATE
        .dw     10

; dp> ( -- ) Copy ini, dps and latest from eeprom to ram
;        .dw     link
; link    set     $
        .db     NFA|3,"dp>"
DP_TO_RAM:
        rcall   DOLIT
        .dw     dp_start
        rcall   INI
        rcall   TEN
        jmp     CMOVE

; >dp ( -- ) Copy only changed turnkey, dp's and latest from ram to eeprom
;        .dw     link
; link    set     $
        .db     NFA|3,">dp"
DP_TO_EEPROM:
        rcall   DOLIT
        .dw     dp_start
        rcall   STORE_P_TO_R
        rcall   INI
        rcall   DOLIT
        .dw     4
        rcall   TOR
DP_TO_EEPROM_0: 
        rcall   FETCHPP
        rcall   DUP
        rcall   PFETCH
        rcall   NOTEQUAL
        rcall   ZEROSENSE
        breq    DP_TO_EEPROM_1
.if DEBUG_FLASH == 1
        rcall   DOLIT
        .dw     'E'
        call    EMIT
.endif
        rcall   PSTORE
        rjmp    DP_TO_EEPROM_2
DP_TO_EEPROM_1:
        rcall   DROP
DP_TO_EEPROM_2:
        rcall   PTWOPLUS
DP_TO_EEPROM_3:
        rcall   XNEXT
        brcc    DP_TO_EEPROM_0
        pop     t1
        pop     t0
        rcall   R_TO_P
        jmp     DROP

        fdw     DOTSTATUS_L
FALSE_L:
        .db     NFA|5,"false"
FALSE_:                     ; TOS is 0000 (FALSE)
        pushtos
        clr     tosl
        clr     tosh
        ret

        fdw     FALSE_L
TRUE_L:
        .db     NFA|4,"true",0
TRUE_:                      ; TOS is ffff (TRUE)
        pushtos
        ser     tosl
        ser     tosh
        ret

; QUIT     --    R: i*x --    interpret from kbd
        fdw     TRUE_L
QUIT_L:
        .db     NFA|4,"quit",0
QUIT:
        rcall   RPEMPTY
        rcall   LEFTBRACKET
        rcall   FRAM
QUIT0:  
        ;; Copy INI and DP's from eeprom to ram
        rcall   DP_TO_RAM
QUIT1: 
        rcall   check_sp
        rcall   CR
        rcall   TIB
        rcall   DUP
        rcall   TIBSIZE
        sbiw    tosl, 10     ; Reserve 10 bytes for hold buffer
        rcall   ACCEPT
        rcall   SPACE_
        rcall   INTERPRET
        rcall   STATE_
        rcall   ZEROSENSE
        brne    QUIT1
        rcall   IFLUSH
        rcall   DP_TO_EEPROM
         
        rcall    XSQUOTE
        .db     3," ok"
        rcall    TYPE
        rcall   PROMPT_
        jmp     QUIT0


        fdw     QUIT_L
PROMPT_L:
        .db     NFA|6,"prompt",0
PROMPT_:
        call    DEFER_DOES
        .dw     prompt

; ABORT    i*x --   R: j*x --   clear stk & QUIT
        fdw     PROMPT_L
ABORT_L:
        .db     NFA|5,"abort"
ABORT:
        rcall   S0
        rcall   FETCH_A
        rcall   SPSTORE
        jmp     QUIT            ; QUIT never rets

; ?ABORT   f --       abort & print ?
        fdw     ABORT_L
QABORTQ_L:
        .db     NFA|7,"?abort?"
QABORTQ:
        rcall   XSQUOTE
        .db     1,"?"
        jmp     QABORT


; ?ABORT   f c-addr u --       abort & print msg if flag is false
        fdw     QABORTQ_L
QABORT_L:
        .db     NFA|6,"?abort",0
QABORT:
        rcall   ROT
        rcall   ZEROSENSE
        brne    QABO1
QABORT1:        
        rcall   SPACE_
        rcall   TYPE
        rcall   ABORT  ; ABORT never returns
QABO1:  jmp     TWODROP

; ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;         i*x x1 --       R: j*x --      x1<>0
        fdw     QABORT_L
ABORTQUOTE_L:
        .db     NFA|IMMED|COMPILE|6,"abort",0x22,0
ABORTQUOTE:
        rcall   SQUOTE
        rcall   DOCOMMAXT
        fdw     QABORT
        ret

;***************************************************
; LIT   -- x    fetch inline 16 bit literal to the stack
        fdw     ABORTQUOTE_L
DOLIT_L:
        .db     NFA|3, "lit"
DOLIT:
        m_pop_zh
        pop     zh
        pop     zl
        rcall   FETCHLIT
        ror     zh
        ror     zl
        mijmp    ; (z)

; DUP must not be reachable from user code with rcall
        fdw     RFETCH_L
DUP_L:
        .db     NFA|INLINE|3, "dup"
DUP:
        pushtos
        ret

        fdw     NOTEQUAL_L
ZEROEQUAL_L:
        .db     NFA|2, "0=",0
ZEROEQUAL:
        sbiw    tosl, 1
        sbc     tosl, tosl
        sbc     tosh, tosh
        ret

        fdw     ZEROEQUAL_L
ZEROLESS_L:
        .db     NFA|2, "0<",0
ZEROLESS:
        lsl     tosh
        sbc     tosl, tosl
        sbc     tosh, tosh
        ret


; '    -- xt             find word in dictionary
        fdw     DOLIT_L
TICK_L:
        .db     NFA|1,0x27    ; 27h = '
TICK:
        rcall   BL
        rcall   WORD
        rcall   FIND
        jmp     QABORTQ

; CHAR   -- char           parse ASCII character
        fdw     TICK_L
CHAR_L:
        .db     NFA|4,"char",0
CHAR:
        rcall   BL
        rcall   PARSE
        rcall   DROP
        jmp     CFETCH

; (    --                     skip input until )
        fdw     CHAR_L
PAREN_L:
        .db     NFA|IMMED|1,"("
PAREN:
        rcall   DOLIT
        .dw     ')'
        rcall   PARSE
        sbr     FLAGS1, (1<<noclear) ; dont clear flags in case of (
        jmp     TWODROP

; IHERE    -- a-addr    ret Code dictionary ptr
;   IDP @ ;
        fdw     PAREN_L
IHERE_L:
        .db     NFA|5,"ihere"
IHERE:
        rcall   IDP
        rjmp    FETCH_A

; [CHAR]   --          compile character DOLITeral
        fdw     IHERE_L
BRACCHAR_L:
        .db     NFA|IMMED|COMPILE|6,"[char]",0
BRACCHAR:
        rcall   CHAR
        jmp     LITERAL

; COMPILE,  xt --         append codefield
        .db     NFA|3,"cf,"
COMMAXT_A:
        jmp     COMMAXT

; CR      --                      output newline
        fdw     BRACCHAR_L
CR_L:
        .db     NFA|2,"cr",0
CR:
        rcall   DOLIT
        .dw     0x0d       ; CR \r
        rcall   EMIT
        rcall   DOLIT
        .dw     0x0a       ; LF \n
EMIT_A:
        jmp     EMIT

; CREATE   --         create an empty definition
; Create a definition header and append 
; doCREATE and the current data space dictionary pointer
; in FLASH.
;  Examples :   
; : table create 10 cells allot does> swap cells + ;
; ram table table_a     flash table table_b    eeprom table table_c
; ram variable  qqq
; eeprom variable www ram
; flash variable  rrr ram 
; eeprom create calibrationtable 30 allot ram
; 
        fdw     CR_L
CREATE_L:
        .db     NFA|6,"create",0
CREATE:
        rcall   BL
        rcall   WORD            ; Parse a word

        rcall   DUP             ; Remember parsed word at rhere
        rcall   FIND
        rcall   NIP
        rcall   ZEROEQUAL
        rcall   XSQUOTE
        .db     15,"ALREADY DEFINED"
        rcall   QABORT         ; ABORT if word has already been defined
        rcall   DUP             ; Check the word length 
        rcall   CFETCH_A
        rcall   ONE
        rcall   DOLIT
        .dw     16
        rcall   WITHIN
        rcall   QABORTQ          ; Abort if there is no name for create

        rcall   IHERE
        rcall   ALIGNED
        rcall   IDP             ; Align the flash DP.
        rcall   STORE_A

        rcall   LATEST_
        rcall   FETCH_A
        rcall   ICOMMA_          ; Link field
        rcall   CFETCHPP        ; str len
        rcall   IHERE
        rcall   DUP             
        rcall   LATEST_         ; new 'latest' link
        rcall   STORE_A         ; str len ihere
        rcall   PLACE           ; 
        rcall   IHERE           ; ihere
        rcall   CFETCH_A
        rcall   DOLIT
        .dw     NFA
        rcall   SHB
        rcall   ONEPLUS
        rcall   ALIGNED
        rcall   IALLOT          ; The header has now been created
        rcall   DOLIT             
        fdw     DOCREATE        ; compiles the runtime routine to fetch the next dictionary cell to the parameter stack
        rcall   STORECFF1       ; Append an exeution token, CALL !
        rcall   ALIGN
        rcall   HERE            ; compiles the current dataspace dp into the dictionary
        rcall   CSE_
        rcall   ZEROSENSE
        brne    CREATE2
        rcall   TWOPLUS
CREATE2:
        jmp     ICOMMA          ; dp now points to a free cell

;***************************************************************
; POSTPONE
        fdw    CREATE_L
POSTPONE_L:
        .db     NFA|IMMED|COMPILE|8,"postpone",0
POSTPONE:
        rcall   BL
        rcall   WORD
        rcall   FIND
        rcall   DUP
        rcall   QABORTQ
        rcall   ZEROLESS
        rcall   ZEROSENSE
        breq    POSTPONE1
        rcall   DOCOMMAXT
        fdw     DOCOMMAXT
        rjmp    ICOMMA_
POSTPONE1:
        jmp     COMMAXT


IDP_L:
        .db     NFA|3,"idp"
IDP:
        rcall   DOCREATE
        .dw     dpFLASH

;***************************************************************
; (DOES>)  --      run-time action of DOES>
;        .dw    link
;link   set     $
        .db     NFA|7,"(does>)"
XDOES:
        m_pop_zh
        rcall   RFROM
        rcall   LATEST_
        rcall   FETCH_A
        rcall   NFATOCFA
        rcall   IDP
        rcall   FETCH_A
        rcall   TOR_A
        rcall   IDP
        rcall   STORE_A
        lsl     tosl
        rol     tosh
        rcall   STORECFF1 ; Always stores a 4 byte call
        rcall   RFROM
        rcall   IDP
        jmp     STORE


; DOES>    --      change action of latest def'n
        fdw     POSTPONE_L
DOES_L:
        .db     NFA|IMMED|COMPILE|5,"does>"
DOES:   rcall   DOCOMMAXT
        fdw     XDOES
        rcall   DOCOMMAXT
        fdw     DODOES
        ret


;*****************************************************************
; [        --      enter interpretive state
        fdw     DOES_L
LEFTBRACKET_L:
        .db     NFA|IMMED|1,"["
LEFTBRACKET:
        sts     state, r_zero
        ret


; ]        --      enter compiling state
        fdw     LEFTBRACKET_L
RIGHTBRACKET_L:
        .db     NFA|1,"]"
RIGHTBRACKET:
        sts     state, r_one
        ret

; :        --           begin a colon definition
        fdw     RIGHTBRACKET_L
COLON_L:
        .db     NFA|1,":"
COLON:
        rcall   CREATE
        rcall   RIGHTBRACKET
        jmp     STORCOLON

; :noname        -- a          define headerless forth code
        fdw     COLON_L
NONAME_L:
        .db     NFA|7,":noname"
NONAME:
        rcall   IHERE
        jmp     RIGHTBRACKET

; ;        --             end a colon definition
        fdw     NONAME_L
SEMICOLON_L:
        .db     NFA|IMMED|COMPILE|1,";"
SEMICOLON:
        rcall   LEFTBRACKET
        sbrc    FLAGS1, fTAILC
        rjmp    ADD_RETURN_1
        rcall   IHERE
        rcall   MINUS_FETCH
        movw    t0, tosl
        andi    t1, 0xf0
        subi    t1, 0xd0
        breq    RCALL_TO_JMP
        poptos
        rcall   MINUS_FETCH
.ifdef EIND
        subi    tosl, 0x0f
.else
        subi    tosl, 0x0e
.endif
        sbci    tosh, 0x94
        brne    ADD_RETURN
CALL_TO_JMP:
.ifdef EIND
        ldi     tosl, 0x0d
.else
        ldi     tosl, 0x0c
.endif
        ldi     tosh, 0x94
        rcall   SWOP
        jmp     STORE
RCALL_TO_JMP:
        rcall   NIP
        andi    tosh, 0x0f
        sbrc    tosh, 3
        ori     tosh, 0xf0
        rcall   TWOSTAR
        rcall   IHERE
        rcall   PLUS
        rcall   DOLIT
        .dw     -2
        rcall   IALLOT
        rcall   DOLIT
.ifdef EIND
        .dw     0x940d
.else
        .dw     0x940c      ; jmp:0x940c
.endif
        rcall   ICOMMA__
        sub_pflash_tos
        rampv_to_c
        ror     tosh
        ror     tosl
        rjmp    ICOMMA__
ADD_RETURN:
        rcall   TWODROP
ADD_RETURN_1:
        rcall   DOLIT   ; Compile a ret
        .dw     0x9508
ICOMMA__:
        jmp    ICOMMA



        fdw     SEMICOLON_L
MINUS_FETCH_L:
        .db     NFA|2,"-@",0
MINUS_FETCH:
        rcall   TWOMINUS
        rcall   DUP
        jmp     FETCH

; [']  --         find word & compile as DOLITeral
        fdw     MINUS_FETCH_L
BRACTICK_L:
        .db     NFA|IMMED|COMPILE|3,"[']"
BRACTICK:
        rcall   TICK       ; get xt of 'xxx'
        jmp     LITERAL

; 2-    n -- n-2
        fdw     BRACTICK_L
TWOMINUS_L:
        .db     NFA|INLINE|2,"2-",0
TWOMINUS:
        sbiw    tosl, 2
        ret

        
; BL      -- char                 an ASCII space
        fdw     TWOMINUS_L
BL_l:
        .db     NFA|2,"bl",0
BL:
        call    DOCREATE
        .dw     ' '

; STATE   -- flag                 holds compiler state
        fdw     BL_L
STATE_L:
        .db     NFA|5,"state"
STATE_:
        pushtos
        lds     tosl, state
        lds     tosh, state
        ret

; LATEST    -- a-addr           
        fdw     STATE_L
LATEST_L:
        .db     NFA|6,"latest",0
LATEST_:
        call    DOCREATE
        .dw     dpLATEST

; S0       -- a-addr      start of parameter stack
        fdw     LATEST_L
S0_L:
        .db     NFA|2,"s0",0
S0:
        rcall   DOUSER
        .dw     us0
        
; R0       -- a-addr      start of parameter stack
        fdw     S0_L
R0_L:
        .db     NFA|2,"r0",0
R0_:
        rcall   DOUSER
        .dw     ur0
        
; ini -- a-addr       ini variable contains the user-start xt
; In RAM
;        .dw     link
;link    set     $
        .db     NFA|3,"ini"
INI:
         call   DOCREATE
        .dw     dpSTART

; ticks  -- u      system ticks (0-ffff) in milliseconds
        fdw     R0_L
TICKS_L:
        .db     NFA|5,"ticks"
TICKS:  
        pushtos
        in_     t0, SREG
        cli
        mov     tosl, ms_count
        mov     tosh, ms_count1
        out_    SREG, t0
        ret

        
; ms  +n --      Pause for n millisconds
; : ms ( +n -- )     
;   ticks -
;   begin
;     pause dup ticks - 0<
;   until drop ;
;
        fdw     TICKS_L
MS_L:
        .db     NFA|2,"ms",0
MS:
        rcall   TICKS
        rcall   PLUS
MS1:    
        rcall   PAUSE
        rcall   DUP
        rcall   TICKS
        rcall   MINUS
        rcall   ZEROLESS
        rcall   ZEROSENSE
        breq    MS1
        jmp     DROP

;  .id ( nfa -- ) 
        fdw     MS_L
DOTID_L:
        .db     NFA|3,".id"
DOTID:
        rcall   CFETCHPP
        andi    tosl, 0x0f
        rcall   TOR
        rjmp    DOTID3
DOTID1:
        rcall   CFETCHPP
        rcall   TO_PRINTABLE
        rcall   EMIT_A
DOTID3:
        rcall   XNEXT
        brcc    DOTID1  
        pop     t1
        pop     t0
        jmp     DROP

 ; >pr   c -- c      Filter a character to printable 7-bit ASCII
        fdw     DOTID_L
TO_PRINTABLE_L:
        .db     NFA|3,">pr"
TO_PRINTABLE:
        clr     tosh   
        cpi     tosl, 0
        brmi    TO_PRINTABLE1
        cpi     tosl, 0x20
        brpl    TO_PRINTABLE2
TO_PRINTABLE1:
        ldi     tosl, '.'
TO_PRINTABLE2:
        ret

;;;;;;;;;;;;;;
LIKEQ:
        rcall   CFETCHPP
        rcall   DOLIT
        .dw     0x0f
        rcall   AND_
        rcall   SWOP
        rcall   STORE_P
        rcall   SWOP
        rcall   CFETCHPP
        rcall   ROT
        rcall   OVER
        rcall   MINUS
        rcall   ONEPLUS
        rcall   FALSE_
        rcall   MAX
        rcall   TOR
        rjmp    LIKEQ3
LIKEQ1:
        rcall   TWODUP
        rcall   FETCH_P
        rcall   PPLUS
        rcall   SWOP
        call    CMP
        breq    LIKEQ3
TWODROPNZ:
        clz
        rjmp    LIKEQ4
LIKEQ3:
        rcall   XNEXT
        brcc    LIKEQ1
TWODROPZ:
        sez
LIKEQ4:
        pop     t1
        pop     t0
        rjmp    TWODROP__

;;;;;;;;;;;;;;;;;;;;
LIKES:
        rcall   TWODUP
        rcall   LIKEQ
        breq    LIKES1
        rcall   DUP
        rcall   DOTID
        rcall   SPACE_
LIKES1:
        rcall   TWOMINUS
        rcall   FETCH_A
        rcall   DUPZEROSENSE
        brne    LIKES
TWODROP__:
        jmp     TWODROP

 ; WORDS    -- filter
        fdw     TO_PRINTABLE_L
WORDS_L:
        .db     NFA|5,"words"
        rcall   BL
        rcall   WORD
        rcall   DUP
        rcall   DOLIT
        fdw     kernellink
        rcall   WDS1
        rcall   LATEST_
        rcall   FETCH_A
WDS1:   rcall   CR
        jmp     LIKES

; .S      --           print stack contents
; : .s space sp@ s0 @ 2- begin 2dup < while -@ u. repeat 2drop ;
        fdw     WORDS_L
DOTS_L:
        .db     NFA|2,".s",0
DOTS:
        rcall   SPACE_
        rcall   DUP
        call    SPFETCH
        rcall   S0
        rcall   FETCH_A
        rcall   TWOMINUS
DOTS1:
        rcall   TWODUP
        rcall   LESS
        rcall   ZEROSENSE
        breq    DOTS2
        rcall   MINUS_FETCH
        rcall   UDOT
        rjmp    DOTS1
DOTS2:  
        rcall   DROP
        jmp     TWODROP

;   DUMP  ADDR U --       DISPLAY MEMORY
        fdw     DOTS_L
DUMP_L:
        .db     NFA|4,"dump",0
DUMP:
        rcall   DOLIT
        .dw     16
        rcall   USLASH
        rcall   TOR
        rjmp    DUMP7
DUMP1:  
        rcall   CR
        rcall   DUP
        rcall   DOLIT
        .dw     4
        rcall   UDOTR
        rcall   DOLIT
        .dw     ':'
        rcall   EMIT_A
        rcall   DOLIT
        .dw     15
        rcall   TOR
DUMP2:
        rcall   CFETCHPP
        rcall   DOLIT
        .dw     2
        rcall   UDOTR
        rcall   XNEXT
        brcc    DUMP2
        pop     t1
        pop     t0

        rcall   DOLIT
        .dw     16
        rcall   MINUS
        rcall   DOLIT
        .dw     15
        rcall   TOR
DUMP4:  
        rcall   CFETCHPP
        rcall   TO_PRINTABLE
        rcall   EMIT_A
        rcall   XNEXT
        brcc    DUMP4
        pop     t1
        pop     t0
DUMP7:
        rcall   XNEXT
        brcc    DUMP1
        pop     t1
        pop     t0
        jmp     DROP

; IALLOT   n --    allocate n bytes in ROM
;       .dw     link
;link   set     $
        .db     NFA|1," "
IALLOT:
        rcall   IDP
        jmp     PLUSSTORE
    

;***************************************************************
;  Store the execcution vector addr to the return stack
; leave the updated return stack pointer on the data stack
; x>r ( addr rsp -- rsp' )
        fdw     DUMP_L
X_TO_R_L:
        .db     NFA|3,"x>r"
X_TO_R:
        movw    zl, tosl
        poptos
        rcall   TO_XA
        adiw    zl, 1
        st      -z, tosl
        st      -z, tosh
.ifdef EIND
        st      -z, r_one
.endif
        st      -z, r_zero
        movw    tosl, zl
        ret
;***************************************************************
        fdw     X_TO_R_L
TO_XA_L:
        .db NFA|3,">xa"
TO_XA:
         sub_pflash_tos
         rampv_to_c
         ror tosh
         ror tosl
         ret

         fdw     TO_XA_L
XA_FROM_L:
        .db NFA|3,"xa>"
XA_FROM:
         lsl     tosl
         rol     tosh
         add_pflash_tos
         ret
;***************************************************************
         fdw    XA_FROM_L
PFL_L:
        .db     NFA|3,"pfl"
PFL:
         call   DOCREATE
        .dw     OFLASH
;***************************************************************
        fdw    PFL_L
ZFL_L:
        .db     NFA|3, "zfl"
ZFL:
         call   DOCREATE
        .dw     RAMPZV
;***************************************************************
; ,?0=    -- addr  Compile ?0= and make make place for a branch instruction
        .db     NFA|4, ",?0=",0    ; Just for see to work !
COMMAZEROSENSE:
        sbrc    FLAGS1, idup
        rjmp    COMMAZEROSENSE1
        rcall   DOLIT
        fdw     ZEROSENSE
        rjmp    COMMAZEROSENSE2
COMMAZEROSENSE1:
        rcall   IDPMINUS
        rcall   DOLIT
        fdw     DUPZEROSENSE
COMMAZEROSENSE2:
        cbr     FLAGS1, (1<<idup)
        rjmp    INLINE0

IDPMINUS:
        rcall   DOLIT
        .dw     -4
        rjmp    IALLOT

;       rjmp, ( rel-addr -- )
RJMPC:
        rcall   TWOSLASH
        andi    tosh, 0x0f
        ori     tosh, 0xc0
        rjmp    ICOMMA__


BRCCC:
        rcall   DOLIT
        .dw     0xf008      ; brcc pc+2
        rjmp    ICOMMA__
;BREQC:
;        rcall   DOLIT
;        .dw     0xf009      ; breq pc+2
;        sbrc    FLAGS1, izeroeq
;        ori     tosh, 4     ; brne pc+2
;        jmp     ICOMMA
BRNEC:
        rcall   DOLIT
        .dw     0xf409      ; brne pc+2
        sbrc    FLAGS1, izeroeq
        andi    tosh, ~4
        rjmp    ICOMMA__

; IF       -- adrs   conditional forward branch
; Leaves address of branch instruction 
; and compiles the condition byte
        fdw     ZFL_L
IF_L:
        .db     NFA|IMMED|COMPILE|2,"if",0
IF_:
        sbrc    FLAGS1, izeroeq
        rcall   IDPMINUS
        rcall   COMMAZEROSENSE
        rcall   BRNEC
        cbr     FLAGS1, (1<<izeroeq)
        rcall   IHERE
        rcall   FALSE_
        jmp     RJMPC           ; Dummy, replaced by THEN with rjmp 

; ELSE     adrs1 -- adrs2    branch for IF..ELSE
; Leave adrs2 of bra instruction and store bz in adrs1
; Leave adress of branch instruction and FALSE flag on stack
        fdw     IF_L
ELSE_L:
        .db     NFA|IMMED|COMPILE|4,"else",0
ELSE_:
        rcall   IHERE
        rcall   FALSE_
        rcall   RJMPC
        rcall   SWOP_A      ; else-addr  if-addr 
        jmp     THEN_

; THEN     adrs  --        resolve forward branch
        fdw     ELSE_L
THEN_L:
        .db     NFA|IMMED|COMPILE|4,"then",0
THEN_:
        sbr     FLAGS1, (1<<fTAILC)  ; Prevent tailjmp  optimisation
        rcall   IHERE
        rcall   OVER
        rcall   MINUS
        rcall   TWOMINUS
        rcall   TWOSLASH
        rcall   DOLIT
        .dw     0xc000      ;  back-addr mask 
        rcall   OR_
        rcall   SWOP_A
        jmp     STORE

; BEGIN    -- adrs        target for bwd. branch
        fdw     THEN_L
BEGIN_L:
        .db     NFA|IMMED|COMPILE|5,"begin"
BEGIN:
        jmp     IHERE

; UNTIL    adrs --   Branch bakwards if true
        fdw     BEGIN_L
UNTIL_L:
        .db     NFA|IMMED|COMPILE|5,"until"
UNTIL:
        sbr     FLAGS1, (1<<fTAILC)  ; Prevent tailjmp  optimisation
        sbrc    FLAGS1, izeroeq
        rcall   IDPMINUS
        rcall   COMMAZEROSENSE
        rcall   BRNEC
        cbr     FLAGS1, (1<<izeroeq)
        jmp     AGAIN_

                                ; AGAIN    adrs --      uncond'l backward branch
;   unconditional backward branch
        fdw     UNTIL_L
AGAIN_L:
        .db     NFA|IMMED|COMPILE|5,"again"
AGAIN_:
        sbr     FLAGS1, (1<<fTAILC)  ; Prevent tailjmp  optimisation
        rcall   IHERE
        rcall   MINUS
        rcall   TWOMINUS
        jmp     RJMPC

; WHILE    addr1 -- addr2 addr1         branch for WHILE loop
; addr1 : address of BEGIN
; addr2 : address where to store bz instruction
        fdw     AGAIN_L
WHILE_L:
        .db     NFA|IMMED|COMPILE|5,"while"
WHILE_:
        rcall   IF_
        jmp     SWOP

; REPEAT   addr2 addr1 --     resolve WHILE loop
        fdw     WHILE_L
REPEAT_L:
        .db     NFA|IMMED|COMPILE|6,"repeat",0
REPEAT_:
        rcall   AGAIN_
        jmp     THEN_

        fdw     REPEAT_L
INLINE_L:
        .db      NFA|IMMED|COMPILE|6,"inline",0
        cbr      FLAGS1, (1<<izeroeq)
        cbr      FLAGS1, (1<<idup)
        rcall    TICK
        jmp      INLINE0
; in, ( addr -- ) begin @+ dup $9508 <> while i, repeat 2drop ;
        fdw      INLINE_L
INLINEC_L:
        .db      NFA|3,"in,"
INLINE0:        
        rcall   FETCHPP
        rcall   DUP
        rcall   DOLIT
        .dw     0x9508
        rcall   NOTEQUAL
        rcall   ZEROSENSE
        breq    INLINE1
        rcall   ICOMMA
        rjmp    INLINE0
INLINE1:
        jmp     TWODROP

; FOR   -- bc-addr bra-addr
        fdw     INLINEC_L
FOR_L:
        .db     NFA|IMMED|COMPILE|3,"for"
FOR:
        call    DOCOMMAXT
        fdw     TOR
        rcall   IHERE
        rcall   FALSE_
        rcall   RJMPC
        rcall   IHERE
        jmp     SWOP

; NEXT bra-addr bc-addr --
        fdw     FOR_L
NEXT_L:
        .db     NFA|IMMED|COMPILE|4,"next", 0
NEXT:
        rcall   THEN_
        call    DOCOMMAXT
        fdw     XNEXT
        rcall   BRCCC

        rcall   AGAIN_

        rcall   DOLIT
        fdw     XNEXT1
        jmp     INLINE0
; (next) decrement top of return stack
        .db     NFA|7,"(next) "
XNEXT:  
        m_pop_zh
        pop     zh
        pop     zl
        pop     xh
        pop     xl
        sbiw    xl, 1
        push    xl
        push    xh
        mijmp
        ret
XNEXT1:
        pop     t1
        pop     t0
        ret

; leave clear top of return stack
        fdw     NEXT_L
LEAVE_L:
        .db     NFA|COMPILE|5,"endit"
LEAVE:
        m_pop_zh
        pop     zh
        pop     zl
        pop     t1
        pop     t0
        push    r_zero
        push    r_zero
        mijmp
;***************************************************
; RDROP compile a pop
        fdw      LEAVE_L
RDROP_L:
        .db      NFA|IMMED|COMPILE|5,"rdrop"
RDROP:
        rcall   DOLIT
        fdw     XNEXT1
        jmp     INLINE0
;***************************************************
        fdw     RDROP_L
STOD_L:
        .db     NFA|3,"s>d"
STOD:
        sbrs    tosh, 7
        rjmp    FALSE_
        rjmp    TRUE_
;***************************************************
        fdw     STOD_L
DNEGATE_L:
        .db     NFA|7,"dnegate"
DNEGATE:
        rcall   DINVERT
        call    ONE
        jmp     MPLUS
;***************************************************
        fdw     DNEGATE_L
QDNEGATE_L:
        .db     NFA|8,"?dnegate",0
QDNEGATE:
        rcall   ZEROLESS
        rcall   ZEROSENSE
        breq    QDNEGATE1
        rcall   DNEGATE
QDNEGATE1:
        ret

;***************************************************
        fdw     QDNEGATE_L
DABS_L:
        .db     NFA|4,"dabs",0
DABS:
        rcall   DUP
        jmp     QDNEGATE
;***************************************************
        fdw     DABS_L
DPLUS_L:
        .db     NFA|2,"d+",0
DPLUS:
        ld      xl, Y+
        ld      xh, Y+
        ld      t6, Y+
        ld      t7, Y+
        ld      t0, Y+
        ld      t1, Y+
        add     xl, t0
        adc     xh, t1
        adc     tosl, t6
        adc     tosh, t7
        st      -Y, xh
        st      -Y, xl
        ret

;***************************************************
        fdw     DPLUS_L
DMINUS_L:
        .db     NFA|2,"d-",0
DMINUS:
        rcall   DNEGATE
        jmp     DPLUS
;***************************************************
        fdw     DMINUS_L
DTWOSLASH_L:
        .db     NFA|3,"d2/"
        ld      t0, y+
        ld      t1, y+
        asr     tosh
        ror     tosl
        ror     t1
        ror     t0
        st      -y, t1
        st      -y, t0
        ret
;***************************************************
        fdw     DTWOSLASH_L
DTWOSTAR_L:
        .db     NFA|3,"d2*"
        ld      t0, y+
        ld      t1, y+
        lsl     t0
        rol     t1
        rol     tosl
        rol     tosh
        st      -y, t1
        st      -y, t0
        ret
;***************************************************
        fdw     DTWOSTAR_L
DINVERT_L:
        .db     NFA|7,"dinvert"
DINVERT:
        ld      t0, y+
        ld      t1, y+
        com     t0
        com     t1
        com     tosl
        com     tosh
        st      -y, t1
        st      -y, t0
        ret
;***************************************************
        fdw     DINVERT_L
DZEROEQUAL_L:
        .db     NFA|3,"d0="
DZEROEQUAL:
        ld      xl, y+
        ld      xh, y+
        or      tosl, tosh
        or      tosl, xl
        or      tosl, xh
        brne    DZEROLESS_FALSE
DZEROEQUAL_TRUE:
        ser     tosl
        ser     tosh
        ret

;***************************************************
        fdw     DZEROEQUAL_L
DZEROLESS_L:
        .db     NFA|3,"d0<"
DZEROLESS:
        ld      xl, y+
        ld      xh, y+
        cpi     tosh, 0
        brmi    DZEROEQUAL_TRUE
DZEROLESS_FALSE:
        clr     tosl
        clr     tosh
        ret
;***************************************************
        fdw     DZEROLESS_L
DEQUAL_L:
        .db     NFA|2,"d=",0
        rcall   DMINUS
        jmp     DZEROEQUAL
;***************************************************
        fdw     DEQUAL_L
DLESS_L:
        .db     NFA|2,"d<",0
DLESS:
        rcall   DMINUS
        jmp     DZEROLESS
;***************************************************
        fdw     DLESS_L
DGREATER_L:
        .db     NFA|2,"d>",0
DGREATER:
        call    TWOSWAP
        jmp     DLESS
;***************************************************
        fdw     DGREATER_L
UDDOT_L:
        .db     NFA|3,"ud."
        rcall   LESSNUM
        rcall   NUMS
        rcall   NUMGREATER
        call    TYPE
        jmp     SPACE_
;***************************************************
        fdw     UDDOT_L
DDOT_L:
        .db     NFA|2,"d.",0
        rcall   LESSNUM
        push    tosl     ; dup >r
        push    tosh
        rcall   DABS
        rcall   NUMS
        call    RFROM
        rcall   SIGN
        rcall   NUMGREATER
        call    TYPE
        jmp     SPACE_
;****************************************************
        fdw      DDOT_L
MEMHI_L:
        .db     NFA|2,"hi",0
MEMHI:
        rcall   DOLIT
        fdw     FLASHHI
        call    CSE_
        call    PLUS
        jmp     FETCH
FLASHHI:
        .dw      FLASH_HI
        .dw      EEPROM_HI
        .dw      RAM_HI

.if FLASHEND > 0x3fff
;;; x@ ( addrl addru -- x )
        fdw     A_FROM_L
XFETCH_L:
        .db     NFA|2, "x@",0
.ifdef RAMPZ
	out_    RAMPZ, tosl
.endif
	poptos
        movw    z, tosl
        lpm_    tosl, z+     ; Fetch from Flash directly
        lpm_    tosh, z+
.ifdef RAMPZ
        ldi     t0, RAMPZV
        out_    RAMPZ, t0
.endif
	ret
	
;;; x! ( x addrl addru -- )
        fdw     XFETCH_L
XSTORE_L:
        .db     NFA|2, "x!",0
    	mov     t0, tosl
        call    DROP
        rcall   XUPDATEBUF
        rjmp    ISTORE1
.endif

;***************************************************

        fdw      MEMHI_L
L_FETCH_P:
        .db      NFA|INLINE|2,"@p", 0
FETCH_P:
        pushtos
        movw    tosl, pl
        ret
;***************************************************
        fdw     L_FETCH_P
L_PCFETCH:
        .db     NFA|3,"pc@" ; ( -- c ) Fetch char from pointer
PCFETCH:
        pushtos
        movw    tosl, pl
        jmp     CFETCH
;***************************************************
        fdw      L_PCFETCH
L_PTWOPLUS:
kernellink:
        .db     NFA|INLINE|3,"p2+" ; ( n -- ) Add 2 to p
PTWOPLUS:
        add     pl, r_two
        adc     ph, r_zero
        ret

;***************************************************
; marker --- name
        .dw     0
L_MARKER:
lastword:
        .db     NFA|6,"marker",0
MARKER:
        call    ROM_
        rcall   CREATE
        rcall   DOLIT
        .dw     dp_start
        call    HERE
        rcall   TEN
        rcall   CMOVE
        rcall   TEN
        call    ALLOT
        call    FRAM
        rcall   XDOES
        call    DODOES
        rcall   INI
        rcall   TEN
        jmp     CMOVE

.if IDLE_MODE == 1
.if CPU_LOAD_LED == 1
;;; Enable load led
        fdw     BUSY_L
LOADON_L:
        .db     NFA|5,"load+"
        sbr     FLAGS2, (1<<fLOADled)
        ret

;;; Disable load led
        fdw     LOADON_L
LOADOFF_L:
        .db     NFA|5,"load-"
        cbr     FLAGS2, (1<<fLOADled)
.if CPU_LOAD_LED == 1
        cbi_    CPU_LOAD_DDR, CPU_LOAD_BIT
.if CPU_LOAD_LED_POLARITY == 1
        cbi_    CPU_LOAD_PORT, CPU_LOAD_BIT
.else
        sbi_    CPU_LOAD_PORT, CPU_LOAD_BIT
.endif
.endif
        ret
.endif
;;; 
.if CPU_LOAD == 1
.if CPU_LOAD_LED == 1
        fdw     LOADOFF_L
.else
        fdw     BUSY_L
.endif
LOAD_L:
        .db     NFA|4,"load",0
        rcall DUP
        lds     tosl, load_res
        lds     tosh, load_res+1
        rcall DUP
        lds     tosl, load_res+2
        clr     tosh
        rcall DUP
        ldi     tosl, low(CPU_LOAD_VAL)
        ldi     tosh, high(CPU_LOAD_VAL)
        call    UMSLASHMOD
        jmp     NIP 
.endif
.endif

.ifdef UCSR1A
;***************************************************
; TX1   c --    output character to UART 1
        fdw     RX0Q_L
TX1_L:
        .db     NFA|3,"tx1"
TX1_:
        cpi     tosl, XON
        breq    XXON_TX1_TOS
        cpi     tosl, XOFF
        breq    XXOFF_TX1_TOS
TX1_LOOP:
        rcall   PAUSE
        in_     t0, UCSR1A
        sbrs    t0, UDRE1
        rjmp    TX1_LOOP
        out_    UDR1, tosl
        poptos
        ret

XXON_TX1_TOS:
        poptos
        rjmp    XXON_TX1_1
XXON_TX1:
        sbrs    FLAGS2, ixoff_tx1
        ret
XXON_TX1_1:
        cbr     FLAGS2, (1<<ixoff_tx1)
        ldi     zh, XON
        rjmp    TX1_SEND

XXOFF_TX1_TOS:
        poptos
        rjmp    XXOFF_TX1_1
XXOFF_TX1:
        sbrc    FLAGS2, ixoff_tx1
        ret     
XXOFF_TX1_1:
        sbr     FLAGS2, (1<<ixoff_tx1)
        ldi     zh, XOFF
TX1_SEND:
        in_     zl, UCSR1A
        sbrs    zl, UDRE1
        rjmp    TX1_SEND
        out_    UDR1, zh
        ret
;***************************************************
; RX1    -- c    get character from the serial line
        fdw     TX1_L
RX1_L:
        .db     NFA|3,"rx1"
RX1_:
        rcall   PAUSE
        rcall   RX1Q
        call    ZEROSENSE
        breq    RX1_
        pushtos
        ldi     zl, low(rbuf1)
        ldi     zh, high(rbuf1)
        lds     xl, rbuf1_rd
        add     zl, xl
        adc     zh, r_zero
        ld      tosl, z
        clr     tosh
        in_     t0, SREG
        cli
        inc     xl
        andi    xl, (RX1_BUF_SIZE-1)
        sts     rbuf1_rd, xl
        lds     xl, rbuf1_lv
        dec     xl
        sts     rbuf1_lv, xl
        out_    SREG, t0
        ret
;***************************************************
; RX1?  -- n    return the number of characters in queue
        fdw     RX1_L
RX1Q_L:
        .db     NFA|4,"rx1?",0
RX1Q:
        lds     xl, rbuf1_lv
        cpse    xl, r_zero
        jmp     TRUE_
.if U1FC_TYPE == 1
        rcall   XXON_TX1
.endif
.if U1FC_TYPE == 2
        cbi_    U1RTS_PORT, U1RTS_BIT
.endif
        jmp     FALSE_

;****************************************************
RX1_ISRR:
        ldi     zl, low(rbuf1)
        ldi     zh, high(rbuf1)
        lds     xl, rbuf1_wr
        add     zl, xl
        adc     zh, r_zero
        in_     xh, UDR1
.if OPERATOR_UART == 1
.if CTRL_O_WARM_RESET == 1
        cpi     xh, 0xf
        brne    pc+2
        rjmp    RESET_
.endif
.endif
        st      z, xh
        inc     xl
        andi    xl, (RX1_BUF_SIZE-1)
        sts     rbuf1_wr, xl
        lds     xl, rbuf1_lv
        inc     xl
        sts     rbuf1_lv, xl
        cpi     xl, RX1_BUF_SIZE-2
        brne    PC+2
        rcall   RX1_OVF
        cpi     xl, RX0_OFF_FILL
        brmi    RX1_ISR_SKIP_XOFF
.if U1FC_TYPE == 1
        rcall   XXOFF_TX1_1
.endif
.if U1FC_TYPE == 2
        sbi_    U1RTS_PORT, U1RTS_BIT
.endif
RX1_ISR_SKIP_XOFF:
        rjmp    FF_ISR_EXIT
RX1_OVF:
        ldi     zh, '|'
        rjmp    TX1_SEND
TX1_ISR:
.endif
;***************************************************
RQ_EMIT:
        sbrs    t2, PORF
        rjmp    RQ_EXTR
        rcall   DOLIT
        .dw     'P'
        rcall   EMIT_A
RQ_EXTR:
        sbrs    t2, EXTRF
        rjmp    RQ_BORF
        rcall   DOLIT
        .dw     'E'
        rcall   EMIT_A
RQ_BORF:
        sbrs    t2, BORF
        rjmp    RQ_WDRF
        rcall   DOLIT
        .dw     'B'
        rcall   EMIT_A
RQ_WDRF:
        sbrs    t2, WDRF
        rjmp    RQ_DIVZERO
        rcall   DOLIT
        .dw     'W'
        rcall   EMIT_A
RQ_DIVZERO:
        sbrs    t3, 6 ; T bit MATH error
        rjmp    RQ_END
        rcall   DOLIT
        .dw     'M'
        rcall   EMIT_A
RQ_END: 
        jmp    SPACE_

;*****************************************************
.if IDLE_MODE == 1
IDLE_LOAD:
.if CPU_LOAD == 1       
        sbrs    FLAGS2, fLOAD
        rjmp    CPU_LOAD_END
        in_     t0, SREG
        cli
        cbr     FLAGS2, (1<<fLOAD)
        sts     load_res, loadreg0
        sts     load_res+1,loadreg1
        sts     load_res+2, loadreg2
        clr     loadreg0
        clr     loadreg1
        clr     loadreg2
        out_    SREG, t0
CPU_LOAD_END:
.endif
.if CPU_LOAD_LED == 1
        sbrs    FLAGS2, fLOADled
        rjmp    LOAD_LED_END
        sbi_    CPU_LOAD_DDR, CPU_LOAD_BIT
.if CPU_LOAD_LED_POLARITY == 1
        cbi_    CPU_LOAD_PORT, CPU_LOAD_BIT
.else
        sbi_    CPU_LOAD_PORT, CPU_LOAD_BIT
.endif
LOAD_LED_END:
.endif
        sbrs    FLAGS2, fIDLE
        rjmp    IDLE_LOAD1
        ldi     t0, low(up0)
        cp      upl, t0
        brne    IDLE_LOAD1
.ifdef SMCR
        ldi     t0, (1<<SE)
        out_    SMCR, t0
.else
        in_     t0, MCUCR
        sbr     t0, (1<<SE)
        out_    MCUCR, t0
.endif
.if CPU_LOAD == 1
        out_    TCCR1B, r_zero    ; Stop load counter
.endif
        sleep               ; IDLE mode
.ifdef SMCR
        out_    SMCR, r_zero
.else
        in_     t0, MCUCR
        cbr     t0, (1<<SE)
        out_    MCUCR, r_zero
.endif
IDLE_LOAD1:
.if CPU_LOAD_LED == 1
        sbrc    FLAGS2, fLOADled
.if CPU_LOAD_LED_POLARITY == 1
        sbi_    CPU_LOAD_PORT, CPU_LOAD_BIT
.else
        cbi_    CPU_LOAD_PORT, CPU_LOAD_BIT
.endif
.endif
        ret
.endif
end_of_dict:

;FF_DP code:
dpcode:
;****************************************************
;        org h'f00000'
;        de  h'ff', h'ff'
;        de  dp_user_dictionary&0xff, (dp_user_dictionary>>8)&0xff
;        de  dpeeprom&0xff, (dpeeprom>>8)&0xff
;        de  (dpdata)&0xff, ((dpdata)>>8)&0xff
;        de  lastword_lo, lastword_hi
;        de  DOTSTATUS;&0xff;, (DOTSTATUS>>8)&0xff

; .end
;********************************************************** 
.cseg
.org BOOT_START
RESET_:     jmp  WARM_
.org BOOT_START + 0x02
            rcall FF_ISR
.org BOOT_START + 0x04
            rcall FF_ISR
.org BOOT_START + 0x06
            rcall FF_ISR
.org BOOT_START + 0x08
.if MS_TIMER_ADDR == 0x08
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x0a
            rcall FF_ISR
.org BOOT_START + 0x0c
            rcall FF_ISR
.org BOOT_START + 0x0e
.if MS_TIMER_ADDR == 0x0e
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x10
            rcall FF_ISR
.org BOOT_START + 0x12
.if MS_TIMER_ADDR == 0x12
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x14
.if MS_TIMER_ADDR == 0x14
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x16
.if MS_TIMER_ADDR == 0x16
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x18
.if MS_TIMER_ADDR == 0x18
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x1a
.if MS_TIMER_ADDR == 0x1a
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x1c
.if MS_TIMER_ADDR == 0x1c
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x1e
.if MS_TIMER_ADDR == 0x1e
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x20
.if MS_TIMER_ADDR == 0x20
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x22
.if MS_TIMER_ADDR == 0x22
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.org BOOT_START + 0x24
            rcall FF_ISR
.if 0x26 < INT_VECTORS_SIZE
.org BOOT_START + 0x26
            rcall FF_ISR
.endif
.if 0x28 < INT_VECTORS_SIZE
.org BOOT_START + 0x28
            rcall FF_ISR
.endif
.if 0x2a < INT_VECTORS_SIZE
.org BOOT_START + 0x2a
.if MS_TIMER_ADDR == 0x2a
            rjmp  MS_TIMER_ISR
.else
            rcall FF_ISR
.endif
.endif
.if 0x2c < INT_VECTORS_SIZE
.org BOOT_START + 0x2c
            rcall FF_ISR
.endif
.if 0x2e < INT_VECTORS_SIZE
.org BOOT_START + 0x2e
            rcall FF_ISR
.endif
.if 0x30 < INT_VECTORS_SIZE
.org BOOT_START + 0x30
            rcall FF_ISR
.endif
.if 0x32 < INT_VECTORS_SIZE
.org BOOT_START + 0x32
            rcall FF_ISR
.endif
.if 0x34 < INT_VECTORS_SIZE
.org BOOT_START + 0x34
            rcall FF_ISR
.endif
.if 0x36 < INT_VECTORS_SIZE
.org BOOT_START + 0x36
            rcall FF_ISR
.endif
.if 0x38 < INT_VECTORS_SIZE
.org BOOT_START + 0x38
            rcall FF_ISR
.endif
.if 0x3a < INT_VECTORS_SIZE
.org BOOT_START + 0x3a
            rcall FF_ISR
.endif
.if 0x3c < INT_VECTORS_SIZE
.org BOOT_START + 0x3c
            rcall FF_ISR
.endif
.if 0x3e < INT_VECTORS_SIZE
.org BOOT_START + 0x3e
            rcall FF_ISR
.endif
.if 0x40 < INT_VECTORS_SIZE
.org BOOT_START + 0x40
            rcall FF_ISR
.endif
.if 0x42 < INT_VECTORS_SIZE
.org BOOT_START + 0x42
            rcall FF_ISR
.endif
.if 0x44 < INT_VECTORS_SIZE
.org BOOT_START + 0x44
            rcall FF_ISR
.endif
.if 0x46 < INT_VECTORS_SIZE
.org BOOT_START + 0x46
            rcall FF_ISR
.endif
.if 0x48 < INT_VECTORS_SIZE
.org BOOT_START + 0x48
            rcall FF_ISR
.endif
.if 0x4a < INT_VECTORS_SIZE
.org BOOT_START + 0x4a
            rcall FF_ISR
.endif
.if 0x4c < INT_VECTORS_SIZE
.org BOOT_START + 0x4c
            rcall FF_ISR
.endif
.if 0x4e < INT_VECTORS_SIZE
.org BOOT_START + 0x4e
            rcall FF_ISR
.endif
.if 0x50 < INT_VECTORS_SIZE
.org BOOT_START + 0x50
            rcall FF_ISR
.endif
.if 0x52 < INT_VECTORS_SIZE
.org BOOT_START + 0x52
            rcall FF_ISR
.endif
.if 0x54 < INT_VECTORS_SIZE
.org BOOT_START + 0x54
            rcall FF_ISR
.endif
.if 0x56 < INT_VECTORS_SIZE
.org BOOT_START + 0x56
            rcall FF_ISR
.endif
.if 0x58 < INT_VECTORS_SIZE
.org BOOT_START + 0x58
            rcall FF_ISR
.endif
.if 0x5a < INT_VECTORS_SIZE
.org BOOT_START + 0x5a
            rcall FF_ISR
.endif
.if 0x5c < INT_VECTORS_SIZE
.org BOOT_START + 0x5c
            rcall FF_ISR
.endif
.if 0x5e < INT_VECTORS_SIZE
.org BOOT_START + 0x5e
            rcall FF_ISR
.endif
.if 0x60 < INT_VECTORS_SIZE
.org BOOT_START + 0x60
            rcall FF_ISR
.endif
.if 0x62 < INT_VECTORS_SIZE
.org BOOT_START + 0x62
            rcall FF_ISR
.endif
.if 0x64 < INT_VECTORS_SIZE
.org BOOT_START + 0x64
            rcall FF_ISR
.endif
.if 0x66 < INT_VECTORS_SIZE
.org BOOT_START + 0x66
            rcall FF_ISR
.endif
.if 0x68 < INT_VECTORS_SIZE
.org BOOT_START + 0x68
            rcall FF_ISR
.endif
.if 0x6a < INT_VECTORS_SIZE
.org BOOT_START + 0x6a
            rcall FF_ISR
.endif
.if 0x6c < INT_VECTORS_SIZE
.org BOOT_START + 0x6c
            rcall FF_ISR
.endif
.if 0x6e < INT_VECTORS_SIZE
.org BOOT_START + 0x6e
            rcall FF_ISR
.endif
.if 0x70 < INT_VECTORS_SIZE
.org BOOT_START + 0x70
            rcall FF_ISR
.endif

.org BOOT_START + INT_VECTORS_SIZE - 1
FF_ISR_EXIT:
        pop     tosh
        pop     tosl
        pop     t1
        pop     t0
        pop     zh
        pop     zl
MS_TIMER_ISR_EXIT:
        ld      xl, y+
        ld      xh, y+
        out_    SREG, xh
        ld      xh, y+
        reti
        
FF_ISR:
.if IDLE_MODE == 1
.if CPU_LOAD == 1
        out_    TCCR1B, r_one   ; Start load counter
.endif
.endif
        st      -y, xh
        in_     xh, SREG
        st      -y, xh
        st      -y, xl
        m_pop_xh
        pop     xh
        pop     xl
        push    zl
        push    zh
        push    t0
        push    t1
        push    tosl
        push    tosh
.if low(ivec) == 0x80
        ldi     xh, low(ivec-1)
        add     xl, xh
.else
        subi    xl, 1
.endif
        ldi     xh, high(ivec)
        ld      zl, x+
        ld      zh, x+
        mijmp   ;(z)

;;; *************************************************
MS_TIMER_ISR:
.if IDLE_MODE == 1
.if CPU_LOAD == 1
        out_    TCCR1B, r_one   ; Start load counter
.endif
.endif
        st      -y, xh
        in_     xh, SREG
        st      -y, xh
        st      -y, xl
        add     ms_count,  r_one
        adc     ms_count1, r_zero
.if CPU_LOAD == 1
LOAD_ADD:
        in_     xl, TCNT1L
        in_     xh, TCNT1H
        out_    TCNT1H, r_zero
        out_    TCNT1L, r_two

        add     loadreg0, xl
        adc     loadreg1, xh
        adc     loadreg2, r_zero

        tst     ms_count
        brne    LOAD_ADD_END
        sbr     FLAGS2, (1<<fLOAD)
LOAD_ADD_END:
.endif
        rjmp    MS_TIMER_ISR_EXIT
;;; ***************************************************
RX0_ISR:
        ldi     zl, low(rbuf0)
        ldi     zh, high(rbuf0)
        lds     xl, rbuf0_wr
        add     zl, xl
        adc     zh, r_zero
        in_     xh, UDR0_
.if OPERATOR_UART == 0
.if CTRL_O_WARM_RESET == 1
        cpi     xh, 0xf
        brne    pc+2
        rjmp    RESET_
.endif
.endif
        st      z, xh
        inc     xl
        andi    xl, (RX0_BUF_SIZE-1)
        sts     rbuf0_wr, xl
        lds     xl, rbuf0_lv
        inc     xl
        sts     rbuf0_lv, xl
        cpi     xl, RX0_BUF_SIZE-2
        brne    PC+2
        rcall   RX0_OVF
        cpi     xl, RX0_OFF_FILL
        brmi    RX0_ISR_SKIP_XOFF
.if U0FC_TYPE == 1
        rcall   XXOFF_TX0_1
.endif
.if U0FC_TYPE == 2
        sbi_    U0RTS_PORT, U0RTS_BIT
.endif
RX0_ISR_SKIP_XOFF:
        rjmp    FF_ISR_EXIT
RX0_OVF:
        ldi     zh, '|'
        rjmp    TX0_SEND
TX0_ISR:

.ifdef UCSR1A
RX1_ISR: rjmp   RX1_ISRR
.endif
;***************************************************
; TX0   c --    output character to UART 0
.if IDLE_MODE == 1
.if CPU_LOAD == 1
        fdw(LOAD_L)
.else
.if CPU_LOAD_LED == 1
        fdw(LOADOFF_L)
.else
        fdw(BUSY_L)
.endif
.endif
.else
        fdw(EXIT_L)
.endif
TX0_L:
        .db     NFA|3,"tx0"
TX0_:
.if U0FC_TYPE == 1
        cpi     tosl, XON
        breq    XXON_TX0_TOS
        cpi     tosl, XOFF
        breq    XXOFF_TX0_TOS
.endif
TX0_LOOP:
        rcall   PAUSE
        in_     t0, UCSR0A
        sbrs    t0, 5        ; UDRE0, UDRE USART Data Register Empty
        rjmp    TX0_LOOP
        out_    UDR0_, tosl
        poptos
        ret

.if U0FC_TYPE == 1
XXON_TX0_TOS:
        poptos
        rjmp    XXON_TX0_1
XXON_TX0:
        sbrs    FLAGS2, ixoff_tx0
        ret
XXON_TX0_1:
        cbr     FLAGS2, (1<<ixoff_tx0)
        ldi     zh, XON
        rjmp    TX0_SEND

XXOFF_TX0_TOS:
        poptos
        rjmp    XXOFF_TX0_1
XXOFF_TX0:
        sbrc    FLAGS2, ixoff_tx0
        ret     
XXOFF_TX0_1:
        sbr     FLAGS2, (1<<ixoff_tx0)
        ldi     zh, XOFF
.endif
TX0_SEND:
        in_     zl, UCSR0A
        sbrs    zl, 5        ; UDRE0, UDRE USART Data Register Empty
        rjmp    TX0_SEND
        out_    UDR0_, zh
        ret
;***************************************************
; RX0    -- c    get character from the UART 0 buffer
        fdw(TX0_L)
RX0_L:
        .db     NFA|3,"rx0"
RX0_:
        rcall   PAUSE
        rcall   RX0Q
        call    ZEROSENSE
        breq    RX0_
        pushtos
        ldi     zl, low(rbuf0)
        ldi     zh, high(rbuf0)
        lds     xl, rbuf0_rd
        add     zl, xl
        adc     zh, r_zero
        ld      tosl, z
        clr     tosh
        in_     t0, SREG
        cli
        inc     xl
        andi    xl, (RX0_BUF_SIZE-1)
        sts     rbuf0_rd, xl
        lds     xl, rbuf0_lv
        dec     xl
        sts     rbuf0_lv, xl
        out_    SREG, t0
        ret
;***************************************************
; RX0?  -- n    return the number of characters in queue
        fdw     RX0_L
RX0Q_L:
        .db     NFA|4,"rx0?",0
RX0Q:
        lds     xl, rbuf0_lv
        cpse    xl, r_zero
        jmp     TRUE_
.if U0FC_TYPE == 1
        rcall   XXON_TX0
.endif
.if U0FC_TYPE == 2
        cbi_    U0RTS_PORT, U0RTS_BIT
.endif
        jmp     FALSE_


;*************************************************************
 ISTORERR:
        rcall   DOTS
        call    XSQUOTE
        .db     3,"AD?"
        call    TYPE
        rjmp    ABORT
        
; Coded for max 256 byte pagesize !
;if (ibaselo != (iaddrlo&(~(PAGESIZEB-1))))(ibaseh != iaddrh)(ibaseu != iaddru)
;   if (idirty)
;       writebuffer_to_imem
;   endif
;   fillbuffer_from_imem
;   ibaselo = iaddrlo&(~(PAGESIZEB-1))
;   ibasehi = iaddrhi
;endif
IUPDATEBUF:
	sub_pflash_tos
.ifdef  RAMPZ
	ldi     t0, RAMPZV
.endif
XUPDATEBUF:
        sts     iaddrl, tosl
        sts     iaddrh, tosh
.ifdef RAMPZ
        sts     iaddru, t0
	cpi     t0, RAMPZV
	brne    XUPDATEBUF2
.endif
        cpi     tosh, high(FLASH_HI-PFLASH+1) ; Dont allow kernel writes
        brcc    ISTORERR
XUPDATEBUF2:	
        lds     t0, iaddrl
        andi    t0, ~(PAGESIZEB-1)
        cpse    t0, ibasel
        rjmp    IFILL_BUFFER
        lds     t0, iaddrh
        cpse    t0, ibaseh
        rjmp    IFILL_BUFFER
.ifdef RAMPZ
        lds     t0, iaddru
        lds     t1, ibaseu
        cpse    t0, t1
        rjmp    IFILL_BUFFER
.endif
        ret

IFILL_BUFFER:
        rcall   IFLUSH
        lds     t0, iaddrl
        andi    t0, ~(PAGESIZEB-1)
        mov     ibasel, t0
        lds     ibaseh, iaddrh
.ifdef RAMPZ
	lds     t0, iaddru
	sts     ibaseu, t0
	out_    RAMPZ, t0
.endif
IFILL_BUFFER_1:
        ldi     t0, PAGESIZEB&0xff ; 0x100 max PAGESIZEB
        movw    zl, ibasel
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
IFILL_BUFFER_2:
        lpm_    t1, z+
        st      x+, t1
        dec     t0
        brne    IFILL_BUFFER_2
.ifdef RAMPZ
        ldi     t0, RAMPZV
        out_    RAMPZ, t0
.endif
        ret

IWRITE_BUFFER:
.if OPERATOR_UART == 0
.if U0FC_TYPE == 1
        rcall   DOLIT
        .dw     XOFF
        call    EMIT
.endif
.if U0FC_TYPE == 2
        sbi_    U0RTS_PORT, U0RTS_BIT
.endif
.else  ;; UART1
.if U1FC_TYPE == 1
        rcall   DOLIT
        .dw     XOFF
        call    EMIT
.endif
.if U1FC_TYPE == 2
        sbi_    U1RTS_PORT, U1RTS_BIT
.endif
.endif
        rcall   DOLIT
        .dw     10
        rcall   MS
        ; Disable interrupts
        cli
        movw    zl, ibasel
.ifdef RAMPZ
	lds     t0, ibaseu
	out_    RAMPZ, t0
.endif
        ldi     t1, (1<<PGERS) | (1<<SPMEN) ; Page erase
        rcall   DO_SPM
        ldi     t1, (1<<RWWSRE) | (1<<SPMEN); re-enable the RWW section
        rcall   DO_SPM

        ; transfer data from RAM to Flash page buffer
        ldi     t0, low(PAGESIZEB);init loop variable
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
        push    r0
        push    r1
IWRITE_BUFFER1:
        ld      r0, x+
        ld      r1, x+
        ldi     t1, (1<<SPMEN)
        rcall   DO_SPM
        adiw    zl, 2
        subi    t0, 2
        brne    IWRITE_BUFFER1

        ; execute page write
        subi    zl, low(PAGESIZEB) ;restore pointer
        sbci    zh, high(PAGESIZEB)
        ldi     t1, (1<<PGWRT) | (1<<SPMEN)
        rcall   DO_SPM
        ; re-enable the RWW section
        rcall   IWRITE_BUFFER3

        ; read back and check, optional
        ldi     t0, low(PAGESIZEB);init loop variable
        subi    xl, low(PAGESIZEB) ;restore pointer
        sbci    xh, high(PAGESIZEB)
IWRITE_BUFFER2:
        lpm_    r0, z+
        ld      r1, x+
        cpse    r0, r1
        rjmp    WARM_     ; reset
        subi    t0, 1
        brne    IWRITE_BUFFER2
        pop     r1
        pop     r0
	ser     t0
	mov     ibaseh, t0
.ifdef RAMPZ
	sts     ibaseu, t0
.endif
.ifdef RAMPZ
        ldi     t0, RAMPZV
        out_    RAMPZ, t0
.endif
        cbr     FLAGS1, (1<<idirty)
        // reenable interrupts
        sei
.if OPERATOR_UART == 0
.if U0FC_TYPE == 1
        rcall   DOLIT
        .dw     XON
        call    EMIT
.endif
.if U0FC_TYPE == 2
        cbi_    U0RTS_PORT, U0RTS_BIT
.endif
.else
.if U1FC_TYPE == 1
        rcall   DOLIT
        .dw     XON
        call    EMIT
.endif
.if U1FC_TYPE == 2
        cbi_    U1RTS_PORT, U1RTS_BIT
.endif
.endif
.if DEBUG_FLASH == 1
        rcall   DOLIT
        .dw     'F'
        call    EMIT
.endif
         ret
        ; ret to RWW section
        ; verify that RWW section is safe to read
IWRITE_BUFFER3:
        in_     t8, SPMCSR
        sbrs    t8, RWWSB ; If RWWSB is set, the RWW section is not ready yet
        ret
        ; re-enable the RWW section
        ldi     t1, (1<<RWWSRE) | (1<<SPMEN)
        rcall   DO_SPM
        rjmp    IWRITE_BUFFER3

DO_SPM:
        in_     t8, SPMCSR
        sbrc    t8, SPMEN
        rjmp    DO_SPM       ; Wait for previous write to complete
        out_    SPMCSR, t1
        spm
        ret
; WD+ ( n -- )  n < 8 start watchdog timer
.if (FLASHEND < 0x1ffff)
        fdw     PAUSE_L
WDON_L:
        .db     NFA|3,"wd+"
WDON:
        cli
        wdr
        lds     tosh, WDTCSR
        ori     tosh, (1<<WDCE)|(1<<WDE)
        sts     WDTCSR, tosh
        andi    tosl, 7
        ori     tosl, (1<<WDE)
        sts     WDTCSR, tosl
        sei
        jmp     DROP

; WD- ( -- )    stop the watchdog 
        fdw     WDON_L
WDOFF_L:
        .db     NFA|3,"wd-"
WDOFF:
        cli
        wdr
.ifdef MCUSR
        out     MCUSR, r_zero
.else
        out     MCUCSR, r_zero
.endif
        ldi     t0, (1<<WDCE)|(1<<WDE)
        sts     WDTCSR, t0
        sts     WDTCSR, r_zero
        sei
        ret

; WDR ( -- )    kick the dog
        fdw     WDOFF_L
CWD_L:
        .db     NFA|INLINE|3,"cwd"
CWD:
        wdr
        ret

.endif
        fdw     CWD_L
IFLUSH_L:
        .db     NFA|6,"iflush",0
IFLUSH:
        sbrc    FLAGS1, idirty
        rjmp    IWRITE_BUFFER
        ret

;***************************************************
.ifdef UCSR1A
        fdw     RX1Q_L
.else
        fdw     RX0Q_L
.endif
EMPTY_L:
        .db     NFA|5,"empty"
EMPTY:
        rcall   DOLIT
        fdw     COLDLIT
        rcall   DOLIT
        .dw     dp_start
        rcall   DOLIT
        .dw     coldlitsize
        call    CMOVE
        jmp     DP_TO_RAM

; Init constant registers
INIT_012:
        clr     r_zero
        ldi     zl, 1
        ldi     zh, 2
        movw    r_one, zl
        ret
;*******************************************************
        fdw     EMPTY_L
WARM_L:
        .db     NFA|4,"warm",0
WARM_:
; Zero memory
        cli           ; Disable interrupts
        clr     xl
        clr     xh
        ldi     yl, 25
        ldi     yh, 0
WARM_1:
        st      x+, yh
        subi    yl, 1
        brne    WARM_1

        in_     t3, SREG
.ifdef MCUCSR
        in_     t2, MCUCSR
        sts     MCUCSR, r_zero
.endif
.ifdef MCUSR
        in_     t2, MCUSR
        sts     MCUSR, r_zero
.endif
        ldi     xl, 0x1C  ; clear ram from y register upwards
WARM_2:
        st      x+, r_zero
        cpi     xh, 0x10  ; up to 0xfff, 4 Kbytes 
        brne    WARM_2

; Init empty flash buffer
	    dec     ibaseh
.ifdef RAMPZ
	sts     ibaseu, ibaseh
.endif

; Init Stack pointer
        ldi     yl, low(utibbuf-4)
        ldi     yh, high(utibbuf-4)

; Init Return stack pointer
        ldi     t0, low(usbuf-1)
        ldi     t1, high(usbuf-1)
        out     spl, t0
        out     sph, t1

        rcall   INIT_012
        call    WDOFF

; Init user pointer
        ldi     t0, low(up0)
        ldi     t1, high(up0)
        movw    upl, t0
; Set RAMPZ for correct flash addressing
.ifdef RAMPZ
        ldi     t0, RAMPZV
        out_    RAMPZ, t0
.endif
.ifdef EIND
        out_    EIND, r_one
.endif
; init warm literals
        rcall   DOLIT
        fdw     WARMLIT
        rcall   DOLIT
        .dw     cse
        rcall   DOLIT
        .dw     warmlitsize
        call    CMOVE
; init cold data to eeprom
        rcall   DOLIT
        .dw     dp_start
        rcall   FETCH
        rcall   TRUE_
        call    EQUAL
        call    ZEROSENSE
        breq    WARM_3  
        rcall   EMPTY
WARM_3:
; Move interrupts to boot flash section
        out_    MCUCR, r_one   ; (1<<IVCE)
        out_    MCUCR, r_two   ; (1<<IVSEL)
; Start watchdog timer
.if MS_TIMER == 0
.ifdef TIMSK0
        out_    TCCR0A, r_two  ; CTC
        ldi     t0, ms_pre_tmr0
        out_    TCCR0B, t0
        ldi     t0, ms_value_tmr0
        out_    OCR0A, t0
        out_    TIMSK0, r_two ; (1<<OCIE0A)
.endif
.ifdef TIMSK
        ldi     t0, (ms_pre_tmr0 | ( 1<<WGM01 ))
        out_    TCCR0, t0
        ldi     t0, ms_value_tmr0
        out_    OCR0, t0
        ldi     t0, (1<<OCIE0)
        out_    TIMSK, t0
.endif
.endif
.if MS_TIMER == 1
; Init ms timer
        ldi     t0, 9      ; CTC, clk/1
        out_    TCCR1B, t0
        ldi     t0, high(ms_value_tmr1)
        out_    OCR1AH, t0
        ldi     t0, low(ms_value_tmr1)
        out_    OCR1AL, t0
.ifdef TIMSK
        ldi     t0, (1<<OCIE1A)
        out_    TIMSK, t0
.endif
.ifdef TIMSK1
        out_    TIMSK1, r_two ; (1<<OCIE1A)
.endif
.endif
.if MS_TIMER == 2
; Init ms timer
.ifdef TIMSK2
        out_    TCCR2A, r_two   ; CTC
        ldi     t0, ms_pre_tmr2
        out_    TCCR2B, t0
        ldi     t0, ms_value_tmr2
        out_    OCR2A, t0
        out_    TIMSK2, r_two ; t0, (1<<OCIE2A)
.endif
.ifdef TIMSK
        ldi     t0, (ms_pre_tmr2 | ( 1<<WGM21 ))
        out_    TCCR2, t0
        ldi     t0, ms_value_tmr2
        out_    OCR2, t0
        ldi     t0, (1<<OCIE2)
        out_    TIMSK, t0
.endif
.endif

; Init UART 0
.ifdef UBRR0L
        rcall   DOLIT
        .dw     RX0_ISR
        rcall   DOLIT
.ifdef URXC0addr
        .dw     URXC0addr+ivec
.else
        .dw     URXCaddr+ivec
.endif
        rcall   STORE
;;;     Set baud rate
;        out_    UBRR0H, r_zero
        ldi     t0, ubrr0val
        out_    UBRR0L, t0
        ; Enable receiver and transmitter, rx1 interrupts
        ldi     t0, (1<<RXEN0)|(1<<TXEN0)|(1<<RXCIE0)
        out_    UCSR0B,t0
        ; Set frame format: 8data, 1stop bit
        ldi     t0, (3<<UCSZ00)|URSEL_
        out_    UCSR0C,t0
.if U0FC_TYPE == 1
        sbr     FLAGS2, (1<<ixoff_tx0)
.endif
.if U0FC_TYPE == 2
        sbi_    U0RTS_DDR, U0RTS_BIT
.endif
.endif
; Init UART 1
.ifdef UBRR1L
        rcall   DOLIT
        .dw     RX1_ISR
        rcall   DOLIT
        .dw     URXC1addr+ivec
        rcall   STORE
        ; Set baud rate
;        out_    UBRR1H, r_zero
        ldi     t0, ubrr1val
        out_    UBRR1L, t0
        ; Enable receiver and transmitter, rx1 interrupts
        ldi     t0, (1<<RXEN1)|(1<<TXEN1)|(1<<RXCIE1)
        out_    UCSR1B,t0
        ; Set frame format: 8data, 1stop bit
        ldi     t0, (3<<UCSZ10)
        out_    UCSR1C,t0
.if U1FC_TYPE == 1
        sbr     FLAGS2, (1<<ixoff_tx1)
.endif
.if U1FC_TYPE == 2
        sbi_    U1RTS_DDR, U1RTS_BIT
.endif
.endif
        rcall   DP_TO_RAM
        sei

        rcall   RQ_EMIT
        rcall   VER
; Turnkey ?
        rcall   TURNKEY
        call    ZEROSENSE
        breq    STARTQ2
        call    XSQUOTE
        .db     3,"ESC"
        call    TYPE
        rcall   DOLIT
        .dw     TURNKEY_DELAY
        rcall   MS
        call    KEYQ
        call    ZEROSENSE
        breq    STARTQ1
        call    KEY
        rcall   DOLIT
        .dw     0x1b
        call    EQUAL
        call    ZEROSENSE
        brne    STARTQ2
STARTQ1:
        rcall   TURNKEY
        call    EXECUTE
STARTQ2:
        jmp     ABORT

.equ partlen = strlen(partstring)
.equ datelen = strlen(DATE)

        fdw     WARM_L
VER_L:
        .db     NFA|3,"ver"
VER:
        call    XSQUOTE
         ;      1234567890123456789012345678901234567890
        ;.db 34,"FlashForth Atmega 5.0 ",DATE,0xd,0xa,0
        .db     partlen+datelen+16,"FlashForth 5 ",partstring," ", DATE,0xd,0xa
        jmp     TYPE

; ei  ( -- )    Enable interrupts
        fdw     VER_L
EI_L:
        .db     NFA|INLINE|2,"ei",0
        sei
        ret
        
; di  ( -- )    Disable interrupts
        fdw     EI_L
DI_L:
        .db     NFA|INLINE|2,"di",0
        cli
        ret
;*******************************************************
; ;i  ( -- )    End definition of user interrupt routine
        fdw     DI_L
IRQ_SEMI_L:
        .db     NFA|IMMED|2,";i",0
IRQ_SEMI:
        rcall   DOLIT
.ifdef EIND
        .dw     0x940D     ; jmp
.else
        .dw     0x940C     ; jmp
.endif
        rcall   ICOMMA
        rcall   DOLIT
        .dw     FF_ISR_EXIT
        rcall   ICOMMA
        jmp     LEFTBRACKET


; int!  ( addr n  --  )   store to interrupt vector number
        fdw     IRQ_SEMI_L
IRQ_V_L:
        .db     NFA|4,"int!",0
IRQ_V:
        movw    zl, tosl
        sbiw    zl, 1
        lsl     zl
.if low(ivec) == 0x80
        ldi     zh, low(ivec)
        add     zl,  zh
.endif
        ldi     zh, high(ivec)
        poptos
        rcall   TO_XA
        jmp     STORE_RAM_2

; DOLITERAL  x --           compile DOLITeral x as native code
        fdw     IRQ_V_L
LITERAL_L:
        .db     NFA|IMMED|7,"literal"
LITERAL:
        rcall   DOLIT
        fdw     DUP
        rcall   INLINE0
        sts     litbuf0, tosl
        sts     litbuf1, tosh
        sbr     FLAGS1, (1<<fLIT)
        call    DUP
        mov     tosh, tosl
        swap    tosh
        andi    tosh, 0xf
        andi    tosl, 0xf
        ori     tosh, 0xe0
        ori     tosl, 0x80
        rcall   ICOMMA
        mov     tosl, tosh
        swap    tosh
        andi    tosh, 0xf
        andi    tosl, 0xf
        ori     tosh, 0xe0
        ori     tosl, 0x90
        jmp     ICOMMA

#if 0
LITERALruntime:
        st      -Y, tosh    ; 0x939a
        st      -Y, tosl    ; 0x938a
        ldi     tosl, 0x12  ; 0xe1r2 r=8 (r24)
        ldi     tosh, 0x34  ; 0xe3r4 r=9 (r25)
#endif

;*****************************************************************
ISTORE:
        rcall   IUPDATEBUF
ISTORE1:
        poptos
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
        lds     t0, iaddrl
        andi    t0, (PAGESIZEB-1)
        add     xl, t0
        st      x+, tosl
        st      x+, tosh
        rjmp    ICSTORE_POP

        fdw     LITERAL_L
TO_A_L:
        .db     NFA|2, ">a",0
TO_A:
        mov     al, tosl
        mov     ah, tosh
        poptos
        ret

        fdw     TO_A_L
STORE_L:
        .db     NFA|1, "!"
STORE:
        cpi     tosh, high(PEEPROM)
        brcc    STORE1
STORE_RAM:
        movw    zl, tosl
        poptos
STORE_RAM_2:
        std     Z+1, tosh
        std     Z+0, tosl
        poptos
        ret
STORE1:
        rcall   LOCKEDQ
        cpi     tosh, high(OFLASH)
        brcc    ISTORE
ESTORE:
        call    TWODUP
        rcall   ECSTORE
        adiw    tosl, 1
        ldd     t0, Y+1
        std     y+0, t0
        rjmp    ECSTORE

LOCKEDQ:
        sbrs    FLAGS1, fLOCK
        ret
        rcall   DOTS
        call    XSQUOTE
        .db     3,"AD?"
        call    TYPE
        rjmp    STARTQ2        ; goto    ABORT
        
;***********************************************************
IFETCH:
        movw    z, tosl
        sub_pflash_z
.ifdef RAMPZ
	lds     t0, ibaseu
	cpi     t0, RAMPZV
	brne    IIFETCH
.endif
        cpse    zh, ibaseh
        rjmp    IIFETCH
        mov     t0, zl
        andi    t0, ~(PAGESIZEB-1)
        cp      t0, ibasel
        brne    IIFETCH
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
        andi    zl, (PAGESIZEB-1)
        add     xl, zl
        ld      tosl, x+
        ld      tosh, x+
        ret
IIFETCH:
        lpm_    tosl, z+     ; Fetch from Flash directly
        lpm_    tosh, z+
        ret
                
        fdw     STORE_L
A_FROM_L:
        .db     NFA|2, "a>",0
A_FROM:
        pushtos
        mov     tosl, al
        mov     tosh, ah
        ret

.if FLASHEND > 0x3fff
        fdw     XSTORE_L
.else
        fdw     A_FROM_L
.endif
FETCH_L:
        .db     NFA|1, "@"
FETCH:
        cpi     tosh, high(PEEPROM)
        brcc    FETCH1
FETCH_RAM:
        movw    zl, tosl
FETCH_RAM_2:
        ld      tosl, z+
        ld      tosh, z+
        ret
FETCH1:
        cpi     tosh, high(OFLASH)
        brcc    IFETCH
EFETCH:
        sbic    eecr, eewe
        rjmp    EFETCH
        subi    tosh, high(PEEPROM)
        out     eearl, tosl
        out     eearh, tosh
        sbi     eecr, eere
        in      t0, eedr
        inc     tosl
        out     eearl, tosl
        sbi     eecr, eere
        in      tosh, eedr
        mov     tosl, t0
        ret

ICFETCH:
        rcall   IFETCH
        clr     tosh
        ret

        fdw     FETCH_L
CFETCH_L:
        .db     NFA|2, "c@",0
CFETCH:
        cpi     tosh, high(PEEPROM)
        brcc    CFETCH1
CFETCH_RAM:
        movw    zl, tosl
        ld      tosl, z+
        clr     tosh
        ret
CFETCH1:
        cpi     tosh, high(OFLASH)
        brcc    ICFETCH
ECFETCH:
        rcall   EFETCH
        clr     tosh
        ret

ICSTORE:
        rcall   IUPDATEBUF
        poptos
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
        lds     t0, iaddrl
        andi    t0, (PAGESIZEB-1)
        add     xl, t0
        st      x+, tosl
ICSTORE_POP:
        sbr     FLAGS1, (1<<idirty)
        rjmp    CSTORE_POP

        fdw     CFETCH_L
CSTORE_L:
        .db     NFA|2, "c!",0
CSTORE:
        cpi     tosh, high(PEEPROM)
        brcc    CSTORE1
CSTORE_RAM:
        movw zl, tosl
        poptos
        st      Z, tosl
CSTORE_POP:
        poptos
        ret
CSTORE1:
        rcall   LOCKEDQ
        cpi     tosh, high(OFLASH)
        brcc    ICSTORE
ECSTORE:
        sbic    eecr, eewe
        rjmp    ECSTORE
        subi    tosh, high(PEEPROM)
        out     eearl, tosl
        out     eearh, tosh
        poptos
        out     eedr, tosl
        sbi     eecr, eemwe
        sbi     eecr, eewe
        rjmp    CSTORE_POP

;;; Disable writes to flash and eeprom
        fdw     CSTORE_L

FLOCK_L:
        .db     NFA|3,"fl-"
        sbr     FLAGS1, (1<<fLOCK)
        ret

;;; Enable writes to flash and eeprom
        fdw     FLOCK_L
FUNLOCK_L:
        .db     NFA|3,"fl+"
        cbr     FLAGS1, (1<<fLOCK)
        ret



        fdw     FUNLOCK_L
VALUE_L:
        .db     NFA|5,"value"
VALUE:
        rcall   CREATE
        call    COMMA
        rcall   XDOES
VALUE_DOES:
        call    DODOES
        jmp     FETCH

        fdw     VALUE_L
DEFER_L:
        .db     NFA|5,"defer"
DEFER:
        rcall   CREATE
        call    DOLIT
        fdw     ABORT
        call    COMMA
        rcall   XDOES
DEFER_DOES:
        call    DODOES
        jmp     FEXECUTE

        fdw     DEFER_L
IS_L:
        .db     NFA|IMMED|2,"is",0
IS:
        call    TICK
        call    TWOPLUS
        call    TWOPLUS
        rcall   FETCH
        rcall   STATE_
        call    ZEROSENSE
        breq    IS1
        rcall   LITERAL
        call    DOCOMMAXT
        fdw     STORE
        rjmp    IS2
IS1:
        rcall   STORE
IS2:
        ret

        fdw     IS_L
TO_L:
        .db     NFA|IMMED|2,"to",0
TO:
        jmp     IS

        fdw     TO_L
TURNKEY_L:
        .db     NFA|7,"turnkey"
TURNKEY:
        call    VALUE_DOES      ; Must be call for IS to work.
        .dw     dpSTART


;;; *******************************************************
; PAUSE  --     switch task
        fdw     TURNKEY_L
PAUSE_L:
        .db     NFA|5,"pause"
PAUSE:
.if IDLE_MODE == 1
        rcall   IDLE_LOAD
.endif
        in_     t1, SREG
        cli
        wdr               ; watchdog reset
        push    yh        ; SP
        push    yl
        push    tosh      ; TOS
        push    tosl
        push    ph        ; P
        push    pl
        movw    zl, upl
        in      t0, sph
        st      -z, t0
        in      t0, spl
        st      -z, t0
        ld      xh, -z     ; UP
        ld      xl, -z
        movw    upl, xl
        ld      t0, -x
        out     sph, t0
        ld      t0, -x
        out     spl, t0
        pop     pl
        pop     ph
        pop     tosl
        pop     tosh
        pop     yl
        pop     yh
        out_    SREG, t1
        ret


        fdw     OPERATOR_L
ICOMMA_L:
        .db     NFA|2, "i,",0
ICOMMA:
        call    IHERE
        rcall   STORE
        call    CELL
        jmp     IALLOT


;   IHERE ! 1 CHARS IALLOT ;
        fdw     ICOMMA_L
ICCOMMA_L:
        .db     NFA|3,"ic,"
ICCOMMA:
        call    IHERE
        rcall   CSTORE
        call    ONE
        jmp     IALLOT

L_DOTBASE:
        .db      NFA|1," "
DOTBASE:
        call    BASE
        rcall   FETCH
        cpi     tosl, 0x10
        brne    DOTBASE1
        ldi     tosl,'$'
        rjmp    DOTBASEEND
DOTBASE1:
        cpi     tosl, 0xa
        brne    DOTBASE2
        ldi     tosl, '#'
        rjmp    DOTBASEEND
DOTBASE2:
        cpi     tosl, 0x2
        brne    DOTBASE3
        ldi     tosl, '%'
        rjmp    DOTBASEEND
DOTBASE3:
        ldi     tosl, '?'
DOTBASEEND:
        ret

MEMQADDR_N:
        fdw     ROM_N
        fdw     EROM_N
        fdw     FRAM_N
;*******************************************************
umstar0:
        push t2
        push t3
        ld  t0, Y+
        ld  t1, Y+
        mul tosl,t0
        movw t4, r0 ; r0=t2, r1=t3
        clr t6
        clr t7
        mul tosh, t0
        add t5, r0
        adc t6, r1
        adc t7, r_zero
        mul tosl, t1
        add t5, r0
        adc t6, r1
        adc t7, r_zero
        mul tosh, t1
        add t6, r0
        adc t7, r1
        st -Y, t5
        st -Y, t4
        movw tosl, t6
        pop t3
        pop t2
        ret
;***********************************************************
; unsigned 32/16 -> 16/16 division
umslashmod0:
        clt
        tst  tosl
        brne umslashmodstart
        tst  tosh
        brne umslashmodstart
        set  ; Set T flag
        jmp  WARM_
umslashmodstart:
        movw t4, tosl

        ld t3, Y+
        ld t6, Y+

        ld tosl, Y+
        ld tosh, Y+

; unsigned 32/16 -> 16/16 division
        ; set loop counter
        ldi t0,$10 ;6

umslashmod1:
        ; shift left, saving high bit
        clr t7
        lsl tosl
        rol tosh
        rol t3
        rol t6
        rol t7

        ; try subtracting divisor
        cp  t3, t4
        cpc t6, t5
        cpc t7,r_zero

        brcs umslashmod2

        ; dividend is large enough
        ; do the subtraction for real
        ; and set lowest bit
        inc tosl
        sub t3, t4
        sbc t6, t5

umslashmod2:
        dec  t0
        brne umslashmod1 ;16=17=272

umslashmod3:
        ; put remainder on stack
        st -Y,t6
        st -Y,t3
        ; Quotient is already in tos ; 6 + 272 + 4 =282 cycles
        ret
BASEQV:
        fdw     DECIMAL
        fdw     HEX
        fdw     BIN


;;; *************************************
;;; EMPTY dictionary data
; *******************************************************************
.equ coldlitsize=12
COLDLIT:
STARTV: .dw      0
DPC:    .dw      OFLASH
DPE:    .dw      ehere
DPD:    .dw      dpdata
LW:     fdw      lastword
STAT:   fdw      DOTSTATUS
;*******************************************************************
; BOOT sector END **************************************************

KERNEL_END:
