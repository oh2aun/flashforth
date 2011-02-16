;**********************************************************************
;                                                                     *
;    Filename:      FlashForth.asm                                    *
;    Date:          5.10.2010                                         *
;    File Version:  0.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2010  Mikael Nordman
;
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


.include "m128def.inc"
; Macros

  .def zerol = r2
  .def zeroh = r3
  .def upl = r4
  .def uph = r5

  .def al  = r6
  .def ah  = r7
  .def bl  = r8
  .def bh  = r9

  .def ibase =r12
  .def ibasel=r12
  .def ibaseh=r13
  .def iaddr =r14
  .def iaddrl=r14
  .def iaddrh=r15

  .def t0 = r16
  .def t1 = r17
  .def t2 = r18
  .def t3 = r19

  .def i_lo = r10
  .def i_hi = r11
  .def p_lo = r20
  .def p_hi = r21

  .def flags0 = r22
  .def flags1 = r23
  .def tos  = r24
  .def tosl = r24  ; ParameterStackLow
  .def tosh = r25  ; ParameterStackHi
;  .def spl = r28  ; StackPointer Ylo
;  .def sph = r29  ; StackPointer Yhi
   

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


;..............................................................................
;Program Specific Constants (literals used in code)
;..............................................................................
; Flash page size
.equ PAGESIZEB=PAGESIZE*2    ; Page size in bytes 
.equ flashPageMask=0x00      ; One byte, no mask needed on 8 bit processor


; Forth word header flags
.equ NFA= 0x80      ; Name field mask
.equ IMMED= 0x40    ; Immediate mask
.equ INLINE= 0x20   ; Inline mask
.equ COMPILE= 0x10  ; Compile only mask
.equ NFL= 0xf       ; Name field length mask

; flags0
.equ iCR=     7     ; ACCEPT has found CR
.equ noclear= 6     ; dont clear optimisation flags 
.equ idup=    5     ; Use dupzeroequal instead of zeroequal
.equ izeroeq= 4     ; Use bnz instead of bz if zeroequal
.equ istream= 3
.equ idoxoff= 2
.equ ixoff=   1
.equ idirty=  0


; Task flags
.equ running= 0

;;; For Flow Control
.equ XON=   0x11
.equ XOFF=  0x13

;;; Memory mapping prefixes
.equ PRAM    = 0x0000  ; 4 Kbytes of ram
.equ PFLASH  = 0x1000  ; 56 Kbytes of flash + 4 Kbytes hidden boot flash
.equ PEEPROM = 0xf000  ; 4 Kbytes of eeprom

;;; Sizes of the serial RX and TX character queues
.equ rbuf_size= 64
.equ tbuf_size= 64

;;; USER AREA for the OPERATOR task
.equ uaddsize=     0          ; No additional user variables 
.equ ursize=       72         ; 36 cells return stack size ( 2 cells per rcall )
.equ ussize=       72         ; 36 cells parameter stack
.equ utibsize=     72         ; 72 character Terminal Input buffer

;;; User variables and area
.equ us0=          - 30         ; Start of parameter stack
.equ ur0=          - 28         ; Start of return stack
.equ uemit=        - 26         ; User EMIT vector
.equ uemitq_=      - 24         ; User EMIT? vector
.equ ukey=         - 22         ; User KEY vector
.equ ukeyq=        - 20         ; User KEY? vector
.equ ulink=        - 18         ; Task link
.equ ubase=        - 16         ; Number Base
.equ utib=         - 14         ; TIB address
.equ utask=        - 12         ; Task area pointer
.equ ursave=       - 10         ; Saved return stack pointer
.equ ussave=       - 8          ; Saved parameter stack pointer
.equ usource=      - 6          ; Two cells
.equ utoin=        - 2          ; Input stream
.equ uhp=            0          ; Hold pointer
.equ urbuf=        -us0 + 2               ; return stack
.equ usbuf=        ustart-us0+ursize + 2        ; Parameter stack
.equ usbuf0=       usbuf - 2
.equ utibbuf=      ustart-us0+ursize+ussize + 2 ; Terminal Input buffer

;;;  Initial USER area pointer (operator)
.equ u0=           ustart-us0
.equ uareasize=    -us0+ursize+ussize+utibsize + 2

;;; Start of free ram
.equ dpdata=       ustart-us0+ursize+ussize+utibsize + 2

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
ibuf:       .byte PAGESIZEB
txqueue:
tbuf_len:   .byte 2
tbuf_wr:    .byte 2
tbuf_rd:    .byte 2
tbuf_lv:    .byte 2
tbuf:       .byte tbuf_size

rxqueue:
rbuf_len:   .byte 2
rbuf_wr:    .byte 2
rbuf_rd:    .byte 2
rbuf_lv:    .byte 2
rbuf:       .byte rbuf_size

FLAGS_1:     .byte 1
FLAGS_2:     .byte 1
Preg:       .byte 2
ms_count:   .byte 2
intcon1dbg: .byte 2
upcurrdbg:  .byte 2
rpdbg:      .byte 2
spdbg:      .byte 2
dpSTART:    .byte 2
dpFLASH:    .byte 2 ; DP's and LATEST in RAM
dpEEPROM:   .byte 2
dpRAM:      .byte 2
dpLATEST:   .byte 2
sect:       .byte 2 ; Current data section 0=flash, 1=eeprom, 2=ram
state:      .byte 2 ; Compilation state
upcurr:     .byte 2 ; Current USER area pointer
ustart:     .byte uareasize ; The operator user area

.cseg
.org 0
RESET_:     .dw  WARM
INT0_:      .dw  RESET_FF
INT1_:      .dw  RESET_FF
INT2_:      .dw  RESET_FF
INT3_:      .dw  RESET_FF
INT4_:      .dw  RESET_FF
INT5_:      .dw  RESET_FF
INT6_:      .dw  RESET_FF
INT7_:      .dw  RESET_FF
TIMER2COMP_: .dw RESET_FF
TIMER2OVF_:  .dw RESET_FF
TIMER1CAPT_: .dw RESET_FF
TIMER1COMPA_: .dw RESET_FF
TIMER1COMPB_: .dw RESET_FF
TIMER1OVF_:   .dw RESET_FF
TIMER0COMP_:  .dw RESET_FF
SPISTC_:      .dw RESET_FF
USART0RX_:    .dw RESET_FF
USART0UDRE_:  .dw RESET_FF
USART0TX_:    .dw RESET_FF
ADC_:         .dw RESET_FF
EEREADY_:     .dw RESET_FF
ANALOGCOMP_:  .dw RESET_FF
TIMER1COMPC_: .dw RESET_FF
TIMER3CAPT_:  .dw RESET_FF
TIMER3COMPA_: .dw RESET_FF
TIMER3COMPB_: .dw RESET_FF
TIMER3COMPC_: .dw RESET_FF
TIMER3OVF_:   .dw RESET_FF
USART1RX_:    .dw RESET_FF
USART1UDRE_:  .dw RESET_FF
USART1TX_:    .dw RESET_FF
TWI_:         .dw RESET_FF
SPMREADY_:    .dw RESET_FF


RESET_FF:
        
WARM_L:
WARM:
        nop

;;; *************************************
;;; COLD dictionary data
.equ coldlitsize= 6
;.section user_eedata
COLDLIT:
STARTV: .dw      0
DPC:    .dw      KERNEL_END+PFLASH
DPE:    .dw      ehere
DPD:    .dw      dpdata
LW:     .dw      lastword+PFLASH
STAT:   .dw      DOTSTATUS+PFLASH
;;; *************************************************
;;; WARM user area data
.equ warmlitsize= 13
WARMLIT:
        .dw      0x0002                ; CSE RAM
        .dw      0x0000                ; STATE
        .dw      u0                    ; UP
        .dw      usbuf0                ; S0
        .dw      urbuf                 ; R0
        .dw      TX1+PFLASH
        .dw      TX1Q+PFLASH
        .dw      RX1+PFLASH
        .dw      RX1Q+PFLASH
        .dw      u0                    ; ULINK
        .dw      0x0010                ; BASE
        .dw      utibbuf               ; TIB
        .dw      OPERATOR_AREA+PFLASH  ; TASK
;;; *************************************************

; *******************************************************************
; Coded for max 256 byte pagesize !
;if (ibaselo != (iaddrlo&(~(PAGESIZEB-1))))(ibasehi != iaddrhi)
;   if (idirty)
;       writebuffer_to_imem
;   endif
;   fillbuffer_from_imem
;   ibaselo = iaddrlo&(~(PAGESIZEB-1))
;   ibasehi = iaddrhi
;endif
IUPDATEBUF:
        mov     t0, iaddr
        andi    t0, ~(PAGESIZEB-1)
        cpse    t0, ibasel
        rjmp    IFILL_BUFFER
        cpse    iaddrh, ibaseh
        rjmp    IFILL_BUFFER
        ret

IFILL_BUFFER:
        rcall   IFLUSH
        mov     t0, iaddrl
        andi    t0, ~(PAGESIZEB-1)
        mov     ibasel, t0
        mov     ibaseh, iaddrh
IFILL_BUFFER_1:
        ldi     t0, PAGESIZEB&(PAGESIZEB-1)
        movw    zl, ibase
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
IFILL_BUFFER_2:
        lpm     t1, z+
        st      x+, t1
        dec     t0
        brne    IFILL_BUFFER_2
        ret

IWRITE_BUFFER:

        lds     t3, (1<<PGERS) | (1<<SPMEN) ; Page erase
        rcall   DO_SPM
        ldi     t3, (1<<RWWSRE) | (1<<SPMEN); re-enable the RWW section
        rcall   DO_SPM

        ; transfer data from RAM to Flash page buffer
        ldi     t0, low(PAGESIZEB);init loop variable
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
IWRITE_BUFFER1:
        ld      r0, x+
        ld      r1, x+
        ldi     t3, (1<<SPMEN)
        rcall   DO_SPM
        adiw    ZH:ZL, 2
        subi    t0, 2
        brne    IWRITE_BUFFER1

        ; execute page write
        subi    ZL, low(PAGESIZEB) ;restore pointer
        ldi     t3, (1<<PGWRT) | (1<<SPMEN)
        rcall   DO_SPM
        ; re-enable the RWW section
        ldi     t3, (1<<RWWSRE) | (1<<SPMEN)
        rcall   DO_SPM
#if 0
        ; read back and check, optional
        ldi     t0, low(PAGESIZEB);init loop variable
        subi    xl, low(PAGESIZEB) ;restore pointer
        sbci    xh, high(PAGESIZEB)
IWRITE_BUFFER2:
        lpm     r0, z+
        ld      r1, x+
        cpse    r0, r1
        jmp     VERIFY_ERROR     ; What to do here ?? reset ?
        subi    t0, 1
        brne    IWRITE_BUFFER2
#endif
        ; return to RWW section
        ; verify that RWW section is safe to read
IWRITE_BUFFER3:
        lds     t0, SPMCSR
        sbrs    t0, RWWSB ; If RWWSB is set, the RWW section is not ready yet
        ret
        ; re-enable the RWW section
        ldi     t3, (1<<RWWSRE) | (1<<SPMEN)
        rcall   DO_SPM
        rjmp    IWRITE_BUFFER3

DO_SPM:
        lds     t2, SPMCSR
        sbrc    t2, SPMEN
        rjmp    DO_SPM       ; Wait for previous write to complete
        in      t2, SREG
        cli
        sts     SPMCSR, t3
        spm
        out     SREG, t2
        ret
;*****************************************************************
IFLUSH:
        sbrc    flags1, idirty
        rjmp    IWRITE_BUFFER
        ret

ISTORE:
        movw    iaddr, tos
        rcall   IUPDATEBUF
        poptos
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
        mov     t0, iaddrl
        andi    t0, ~(PAGESIZEB-1)
        add     xh, t0
        st      x+, tosl
        st      x+, tosh
        poptos
        ret

        .dw     WARM_L+PFLASH
STORE_L:
        .db     NFA|1, "!"
STORE:
        cpi     tosh, 0x01
        brmi    STORE_RAM
        cpi     tosh, 0xe0
        brmi    ISTORE
        rjmp    ESTORE
STORE_RAM:
        movw    zl, tosl
        poptos
        std     Z+1, tosh
        std     Z+0, tosl
        poptos
        ret

ESTORE:
        sbic    eecr, eewe
        rjmp    ESTORE
        out     eearl, tosl
        out     eearh, tosh
        poptos
        out     eedr, tosl
        sbi     eecr, eemwe
        sbi     eecr, eewe

ESTORE1:
        sbic    eecr, eewe
        rjmp    ESTORE1

        in      tosl, eearl
        inc     tosl
        out     eearl, tosl

        out     eedr, tosl
        sbi     eecr, eemwe
        sbi     eecr, eewe

        poptos
        ret

;***********************************************************
IFETCH:
        movw    z, tos
        cpse    zh, ibaseh
        rjmp    IIFETCH
        mov     t0, zh
        andi    t0, ~(PAGESIZEB-1)
        breq    IIFETCH
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
        andi    zh, (PAGESIZEB-1)
        add     xl, zh
        ld      tosl, x+
        ld      tosh, x+
IIFETCH:
        lpm     tosl, z+     ; Fetch from Flash directly
        lpm     tosh, z+
        ret
                
        .dw     STORE_L+PFLASH
FETCH_L:
        .db     NFA|1, "@"
FETCH:
        cpi     tosh, 0x01
        brmi    FETCH_RAM
        cpi     tosh, 0xe0
        brmi    IFETCH
        rjmp    EFETCH
FETCH_RAM:
        movw    zl, tosl
        ld      tosl, z+
        ld      tosh, z+
        ret

EFETCH:
        sbic    eecr, eewe
        rjmp    EFETCH
        out     eearl, tosl
        out     eearh, tosh
        sbi     eecr, eere
        in      tosl, eedr
        in      tosh, eearl
        inc     tosh
        out     eearl, tosh
        sbi     eecr, eere
        in      tosh, eedr
        ret

ICFETCH:
        movw    z, tos
        cpse    zh, ibaseh
        rjmp    IICFETCH
        mov     t0, zh
        andi    t0, ~(PAGESIZEB-1)
        breq    IICFETCH
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
        andi    zh, (PAGESIZEB-1)
        add     xl, zh
        ld      tosl, x+
        clr     tosh
        ret
IICFETCH:
        lpm     tosl, z+     ; Fetch from Flash directly
        clr     tosh
        ret
        .dw     FETCH_L+PFLASH
CFETCH_L:
        .db     NFA|2, "c@",0
CFETCH:
        cpi     tosh, 0x01
        brmi    CFETCH_RAM
        cpi     tosh, 0xe0
        brmi    ICFETCH
        rjmp    ECFETCH
CFETCH_RAM:
        movw    zl, tosl
        ld      tosl, z+
        clr     tosh
        ret
ECFETCH:
        sbic    eecr, eewe
        rjmp    ECFETCH
        out     eearl, tosl
        out     eearh, tosh
        sbi     eecr, eere
        in      tosl, eedr
        clr     tosh
        ret

ICSTORE:
        movw    iaddr, tos
        rcall   IUPDATEBUF
        poptos
        ldi     xl, low(ibuf)
        ldi     xh, high(ibuf)
        mov     t0, iaddrl
        andi    t0, ~(PAGESIZEB-1)
        add     xh, t0
        st      x+, tosl
        poptos
        ret

        .dw     CFETCH_L+PFLASH
CSTORE_L:
        .db     NFA|2, "c!",0
CSTORE:
        cpi     tosh, 0x01
        brmi    CSTORE_RAM
        cpi     tosh, 0xe0
        brmi    ICSTORE
        rjmp    ECSTORE
CSTORE_RAM:
        movw zl, tosl
        poptos
        std Z+0, tosl
        poptos
        ret

ECSTORE:
        sbic    eecr, eewe
        rjmp    ECSTORE
        out     eearl, tosl
        out     eearh, tosh
        poptos
        out     eedr, tosl
        sbi     eecr, eemwe
        sbi     eecr, eewe
        poptos
        ret

TX1:
TX1Q:
RX1:
RX1Q:

OPERATOR_L:
        .db     NFA|8,"operator",0
OPERATOR:
        rcall   DOCREATE
        .dw     OPERATOR_AREA+PFLASH
OPERATOR_AREA:
        .dw     ustart-us0
        .dw     uaddsize
        .dw     ursize
        .dw     ussize
        .dw     utibsize

ICOMMA_L:
        .db     NFA|2, "i,",0
ICOMMA:
        rcall   IHERE
        rcall   STORE
        rcall   CELL
        rcall   IALLOT
        ret
IHERE:
IALLOT:
CELL:
CALL_:
RCALL_:
ABS_:
ZEROSENSE:
COMMAXT_L:
        .db     NFA|3, "cf,"
COMMAXT:
        rcall   DUP
        rcall   IHERE
        rcall   MINUS
        rcall   ABS_ 
        rcall   DOLIT
        .dw     0xffe
        rcall   GREATER
        rcall   ZEROSENSE
        breq    STORECF1
STORECFF1: 
        rcall   CALL_
        rjmp    STORECF2 
STORECF1:
        rcall   IHERE
        rcall   MINUS
        call    TWOMINUS
        rcall   RCALL_
STORECF2:
        ret

; LITERAL  x --           compile literal x as native code
        .dw     OPERATOR_L+PFLASH
LITERAL_L:
        .db     NFA|IMMED|7,"literal"
LITERAL:
        pushtos
        ldi     tosl, 0x9a      ; savettos
        ldi     tosh, 0x93      ; savettos
        rcall   ICOMMA
        ldi     tosl, 0x8a      ; savettos
        ldi     tosh, 0x93      ; savettos
        rcall   ICOMMA
        poptos
        rcall   DUP
        mov     tosh, tosl
        swap    tosh
        andi    tosh, 0xf
        andi    tosl, 0xf
        ori     tosh, 0xe0
        ori     tosl, 0x80
        rcall   ICOMMA
        poptos
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

DOLIT_L:
        .db     NFA|3, "lit"
DOLIT:
        pop     zl
        pop     zh
        pushtos
        lpm     tosl, z+
        lpm     tosh, z+
        ijmp    ; (z)

EXECUTE_L:
        .db     NFA|7,"execute"
EXECUTE:
        movw    zl, tosl
        poptos
        ijmp

FEXECUTE_L:
        .db     NFA|3,"@ex"
FEXECUTE:
        rcall   FETCH
        jmp     EXECUTE

VARIABLE_L:
        .db     NFA|8,"variable",0
VARIABLE:
        rcall   CREATE
        rcall   CELL
        jmp     ALLOT

CONSTANT_L:
        .db     NFA|8,"constant",0
CONSTANT_:
        rcall   CREATE
        rcall   CELL
        rcall   NEGATE
        rcall   IALLOT
        jmp     ICOMMA

CON_L:
        .db     NFA|3,"con"
CON:
        rcall   COLON
        rcall   LITERAL
        jmp     SEMICOLON

DOCREATE_L:
        .db     NFA|3, "(c)"
DOCREATE:
        pop     zl
        pop     zh
        pushtos
        lpm     tosl, z+
        lpm     tosh, z+
        ret
      
DODOES_L:
        .db     NFA|3, "(d)"
DODOES:
        pop     xl
        pop     xh
        pop     zl
        pop     zh
        pushtos
        lpm     tosl, z+
        lpm     tosh, z+
        movw    z, x
        ijmp    ; (z)

SPFETCH_L:
        .db     NFA|3,"sp@"
SPFETCH:
        movw    z, y
        pushtos
        movw    tosl, z
        ret

        .db     NFA|3,"sp!"
SPSTORE:
        movw    y, tosl
        ret

        .db     NFA|3,"rp0"
RPEMPTY:
        ret;FIXME



; !COLON   --       change code field to docolon
;   -6 IALLOT ; 
;       dw      link
;link   set     $
        .db     NFA|6,"!colon",0
STORCOLON:
        rcall   DOLIT
        .dw     0xfffa         ;  -6
        jmp     IALLOT


; 2@    a-addr -- x1 x2            fetch 2 cells
;   DUP @ SWAP CELL+ @ ;
;   the lower address will appear on top of stack
        .dw     COMMAXT_L
TWOFETCH_L:
        .db     NFA|2,"2@",0
TWOFETCH:
        rcall   DUP
        rcall   FETCH
        rcall   SWOP
        rcall   CELLPLUS
        jmp     FETCH

; 2!    x1 x2 a-addr --            store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
        .dw     TWOFETCH_L
TWOSTORE_L:
        .db     NFA|2,"2!",0
TWOSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   CELLPLUS
        rcall   STORE
        jmp     STORE

; 2DROP  x1 x2 --                   drop 2 cells
;   DROP DROP ;
        .dw     TWOSTORE_L
TWODROP_L:
        .db     NFA|5,"2drop"
TWODROP:
        rcall   DROP
        jmp     DROP

; 2DUP   x1 x2 -- x1 x2 x1 x2    dup top 2 cells
;   OVER OVER ;
        .dw     TWODROP_L
TWODUP_L:
        .db     NFA|4,"2dup",0
TWODUP:
        rcall   OVER
        jmp     OVER

; INPUT/OUTPUT ==================================

; SPACE   --                      output a space
;   BL EMIT ;
        .dw     TWODUP_L
SPACE_L:
        .db     NFA|5,"space"
SPACE_:  
        call    BL_
        jmp     EMIT

; SPACES   n --                  output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
        .dw     SPACE_L
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
        .dw     SPACES_L
UMIN_L:
        .db     NFA|4,"umin",0
UMIN:
        rcall   TWODUP
        rcall   UGREATER
        rcall   ZEROSENSE
        breq    UMIN1
        rcall   SWOP
UMIN1:  jmp     DROP


; umax    u1 u2 -- u            unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
        .dw     UMIN_L
UMAX_L:
        .db     NFA|4,"umax",0
UMAX:
        rcall   TWODUP
        rcall   ULESS
        rcall   ZEROSENSE
        breq    UMAX1
        rcall   SWOP
UMAX1:  jmp     DROP

        .dw     UMAX_L
ONE_L:
        .db     NFA|INLINE|1,"1"
ONE:


; ACCEPT  c-addr +n -- +n'  get line from terminal
        .dw     ONE_L
ACCEPT_L:
        .db     NFA|6,"accept",0
ACCEPT:
#if 0
        rcall   OVER
        rcall   PLUS
        rcall   OVER
ACC1:
        rcall   KEY

        movf    Sminus, W, A
        movlw   CR_
        subwf   Splus, W, A
        bnz     ACC_LF
        bsf     FLAGS2, fCR, A
        rcall   DROP
        bra     ACC6
ACC_LF:
        movf    Sminus, W, A
        movlw   LF_
        subwf   Splus, W, A
        bnz     ACC2
        rcall   DROP
        btfss   FLAGS2, fCR, A
        bra     ACC6
        bra     ACC1
ACC2:
        bcf     FLAGS2, fCR, A
        rcall   DUP
        rcall   EMIT
        rcall   DUP
        rcall   LIT
        dw      BS_
        rcall   EQUAL
        rcall   ZEROSENSE
        bz      ACC3
        rcall   DROP
        rcall   ONEMINUS
        rcall   TOR
        rcall   OVER
        rcall   RFROM
        rcall   UMAX
        bra     ACC1
ACC3:
        rcall   OVER
        rcall   CSTORE
        rcall   ONEPLUS
        rcall   OVER
        rcall   UMIN
        rcall   TWODUP
        rcall   NOTEQUAL
        rcall   ZEROSENSE
        bnz     ACC1
ACC6:
        rcall   NIP
        rcall   SWOP
        goto    MINUS
#endif
; TYPE    c-addr u --   type line to terminal u < $100
; : type for c@+ emit next drop ;

        .dw      ACCEPT_L
TYPE_L:
        .db     NFA|4,"type",0
TYPE:
        push    tosh
        push    tosl
        poptos
        rjmp    TYPE2           ; XFOR
TYPE1:  
        rcall   CFETCHPP
        rcall   EMIT
TYPE2:
        call    XNEXT_DEC
        brpl    TYPE1
        pop     t0
        pop     t0          ; UNNEXT
        jmp     DROP


; (S"    -- c-addr u      run-time code for S"
;       dw      link
;link    set     $
        .db      NFA|3,"(s",0x22
XSQUOTE:
        rcall   RFROM
        rcall   CFETCHPP
        rcall   TWODUP
        rcall   PLUS
        rcall   ALIGNED
        rcall   TOR       ; do NOT goto TOR!
        ret

SQUOTE_L:
        .db      NFA|IMMED|COMPILE|2,"s",0x22,0
SQUOTE:
        rcall   LIT
        .dw      XSQUOTE
        rcall   COMMAXT
        rcall   ROM
        rcall   CQUOTE
        jmp     FRAM


CQUOTE_L:
        .db     NFA|2,",",0x22,0
CQUOTE: 
        rcall   LIT
        .dw     0x22
        rcall   PARSE
        rcall   HERE
        rcall   OVER
        rcall   ONEPLUS
        rcall   ALIGNED
        rcall   ALLOT
        jmp     PLACE


DOTQUOTE_L:
        .db      NFA|IMMED|COMPILE|2,".",0x22,0
DOTQUOTE:
        rcall   SQUOTE
        rcall   DOLIT
        .dw     TYPE
        jmp     COMMAXT

ALLOT_L:
        .db     NFA|5,"allot"
ALLOT:
        rcall   DP
        jmp     PLUSSTORE

DROP_L:
        .db     NFA|4,"drop",0
DROP:
        poptos
        ret

SWOP_L:
        .db     NFA|4,"swap",0
SWOP:
        ld      t0, y+
        ld      t1, y+
        pushtos
        movw    tosl, t0
        ret

OVER_L:
        .db     NFA|4,"over",0
OVER:
        ldd     t0, y+1
        ldd     t1, y+2
        pushtos
        movw    tosl, t0
        ret

ROT_L:
        .db     NFA|3, "rot"
ROT:
        rcall   TOR
        rcall   SWOP
        rcall   RFROM
        rjmp    SWOP

TOR_L:
        .db     NFA|COMPILE|2,">r",0
TOR:
        pop     zl
        pop     zh
        push    tosh
        push    tosl
        poptos
        ijmp

RFROM_L:
        .db     NFA|COMPILE|2,"r>",0
RFROM:
        pop     zl
        pop     zh
        pushtos
        pop     tosl
        pop     tosh
        ijmp

RFETCH_L:
        .db     NFA|COMPILE|2,"r@",0
RFETCH:
        pop     zl
        pop     zh
        pushtos
        pop     tosl
        pop     tosh
        push    tosh
        push    tosl
        ijmp


        .dw     CSTORE_L+PFLASH
DUP_L:
        .db     NFA|3, "dup"
DUP:
        pushtos
        ret

ABS_L:
        .dw     ABS_L+PFLASH
PLUS_L:
        .db     NFA|1, "+"

PLUS:
        ld      t0, Y+        
        ld      t1, Y+
        add     tosl, t0
        adc     tosh, t1
        ret

; m+  ( d n -- d1 )
        .dw     PLUS_L+PFLASH
MPLUS_L:
        .db     NFA|2, "m+",0
MPLUS:
        ld      t2, Y+
        ld      t3, Y+
        ld      t0, Y+
        ld      t1, Y+
        add     t0, tosl
        adc     t1, tosh
        clr     tosl
        adc     tosl, t2
        clr     tosh
        adc     tosh, t3
        st      -Y, t1
        st      -Y, t0
        ret

        .dw     MPLUS_L+PFLASH
MINUS_L:
        .db     NFA|1, "-"
MINUS:
        ld      t0, Y+
        ld      t1, Y+
        sub     t0, tosl
        sbc     t1, tosh
        movw    tosl, t0
        ret


        .dw     ONEMINUS_L+PFLASH
AND_L:
        .db     NFA|3, "and"
AND_:
        ld      t0, Y+
        ld      t1, Y+
        and     tosl, t0
        and     tosh, t1
        ret

        .dw     AND_L+PFLASH
OR_L:
        .db     NFA|2, "or",0
OR_:
        ld      t0, Y+
        ld      t1, Y+
        or      tosl, t0
        or      tosh, t1
        ret

        .dw     OR_L+PFLASH
XOR_L:
        .db     NFA|3, "xor"
XOR_:
        ld      t0, Y+
        ld      t1, Y+
        eor     tosl, t0
        eor     tosh, t1
        ret

        .dw     XOR_L+PFLASH
INVERT_L:
        .db     NFA|6, "invert",0
INVERT:
        com     tosl
        com     tosh
        ret

        .dw     INVERT_L+PFLASH
NEGATE_L:
        .db     NFA|6, "negate",0
NEGATE:
        rcall   INVERT
        jmp     ONEPLUS

        .dw     NEGATE_L+PFLASH
ONEPLUS_L:
        .db     NFA|INLINE|2, "1+",0
ONEPLUS:
        adiw    tosl, 1
        ret

        .dw     ONEPLUS_L+PFLASH
ONEMINUS_L:
        .db     NFA|INLINE|2, "1-",0
ONEMINUS:
        sbiw    tosl, 1
        ret

        .dw     ONEMINUS_L+PFLASH
TWOPLUS_L:
        .db     NFA|INLINE|2, "2+",0
TWOPLUS:
        adiw    tosl, 2
        ret

        .dw     TWOPLUS_L+PFLASH
TWOMINUS_L:
        .db     NFA|INLINE|2, "2-",0
TWOMINUS:
        sbiw    tosl, 2
        ret

        .dw     TWOMINUS_L+PFLASH
TOBODY_L:
        .db     NFA|INLINE|5, ">body"
TOBODY:
        adiw    tosl, 4
        ret

        .dw     TOBODY_L+PFLASH
TWOSTAR_L:
        .db     NFA|INLINE|2, "2*",0
TWOSTAR:
        lsl     tosl
        rol     tosh
        ret

        .dw     TWOSTAR_L+PFLASH
TWOSLASH_L:
        .db     NFA|INLINE|2, "2/",0
TWOSLASH:
        asr     tosh
        ror     tosl
        ret

        .dw     TWOSLASH_L+PFLASH
ZEROEQUAL_L:
        .db     NFA|COMPILE|2, "0=",0
ZEROEQUAL:      
        or      tosh, tosl
        brne    ZEROEQUAL_1
TRUE_F:
        ser     tosh
        ser     tosl
ZEROEQUAL_1:
        ret

        .dw     ZEROEQUAL_L+PFLASH
ZEROLESS_L:
        .db     NFA|COMPILE|2, "0<",0
ZEROLESS:
        tst     tosh
        brmi    TRUE_F
FALSE_F:
        clr     tosh
        clr     tosl
        ret
        
EQUAL_L:
        .db     NFA|1, "="
EQUAL:
        rcall   MINUS
        jmp     ZEROEQUAL

LESS_L:
        .db     NFA|1,"<"
LESS:
        rcall   MINUS
        jmp     ZEROLESS

GREATER_L:
        .db     NFA|1,">"
GREATER:
        rcall   SWOP
        jmp     LESS

ULESS_L:
        .db     NFA|2,"u<",0
ULESS:
        rcall   MINUS
        brsh    TRUE_F        ; Carry test  
        jmp     FALSE_F

UGREATER_L:
        .db     NFA|2, "u>",0
UGREATER:
        rcall   SWOP
        jmp     ULESS

PLUSSTORE_L:
        .db     NFA|2,"+!",0
PLUSSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   FETCH
        rcall   PLUS
        rcall   SWOP
        jmp     STORE

WITHIN_L:
        .db     NFA|6,"within",0
WITHIN:
        rcall   OVER
        rcall   MINUS
        rcall   TOR
        rcall   MINUS
        rcall   RFROM
        jmp     ULESS

STORE_P_L:
        .db     NFA|2,"!p",0
STORE_P:
        movw    p_lo, tosl
        poptos
        ret

STORE_P_TO_R_L:
        .db     NFA|COMPILE|4,"!p>r",0
STORE_P_TO_R:
        pop     zl
        pop     zh
        push    p_hi
        push    p_lo
        movw    p_lo, tosl
        poptos
        ijmp

R_TO_P_L:
        .db     NFA|COMPILE|3,"r>p"
R_TO_P:
        pop     zl
        pop     zh
        pop     p_lo
        pop     p_hi
        ijmp

PFETCH_L:
        .db     NFA|2,"p@",0
PFETCH:
        pushtos
        movw    tosl, p_lo
        jmp     FETCH

PSTORE_L:
        .db     NFA|2,"p!",0
PSTORE:
        pushtos
        movw    tosl, p_lo
        jmp     STORE

PCSTORE_L:
        .db     NFA|3,"pc!"
PCSTORE:
        pushtos
        movw    tosl, p_lo
        jmp     CSTORE

PCFETCH_L:
        .db     NFA|3,"pc@"
PCFETCH:
        pushtos
        movw    tosl, p_lo
        jmp     CFETCH

PPLUS_L:
        .db     NFA|2,"p+",0
PPLUS:
        inc     p_lo
        brne    PPLUS1
        inc     p_hi
PPLUS1:
        ret

PNPLUS_L:
        .db     NFA|3,"p++"
PNPLUS:
        add     p_lo, tosl
        adc     p_hi, tosh
        poptos
        ret

UEMIT_L:
        .db     NFA|5,"uemit"
UEMIT_:
        rcall   DOUSER
        .dw     uemit

UKEY_L:
        .db     NFA|4,"ukey",0      
UKEY_:
        rcall   DOUSER
        .dw     ukey

UKEYQ_L:
        .db     NFA|5,"ukeyq"
UKEYQ_:
        rcall   DOUSER
        .dw     ukeyq

USER_L:
        .db     NFA|4,"user",0
USER:
        rcall   CONSTANT_
        rcall   XDOES
DOUSER:
        pushtos
        pop     zl
        pop     zh
        lpm     tosl, z+
        lpm     tosh, z+
        add     tosl, upl
        adc     tosh, uph
        ret

CREATE:


COLON:

SEMICOLON:

XDOES:
DP:

XNEXT_DEC:
        ldd     t0, y+3
        ldd     t1, y+4
        subi    t0, 1        ; XNEXT
        sbci    t1, 0
        std     y+4, t1
        std     y+3, t0 
        ret


DOTSTATUS:
lastword:
KERNEL_END:
