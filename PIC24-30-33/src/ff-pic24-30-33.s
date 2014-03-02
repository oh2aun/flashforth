;**********************************************************************
;                                                                     *
;    Filename:      ff-pic24-30-33.s                                  *
;    Date:          01.03.2014                                        *
;    File Version:  5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2014  Mikael Nordman
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
.include "ff30.inc"

; Macro for inline literals 
.macro mlit lval
        mov     #\lval, w0
        mov     w0, [++w14]
.endm
;..............................................................................
;Global Declarations:
;..............................................................................
.global __reset
.global __T1Interrupt
.global __U1TXInterrupt
.global __U1RXInterrupt
.global __U2TXInterrupt
.global __U2RXInterrupt
.global __OscillatorFail
.global __AddressError
.global __StackError
.global __MathError
.ifdecl INTTREG
.global __DefaultInterrupt
.endif
;..............................................................................
;Program Specific Constants (literals used in code)
;..............................................................................
.equ NFA, 0x80      ; Name field mask
.equ IMMED, 0x40    ; Immediate mask
.equ INLINE, 0x20   ; Inline mask
.equ COMPILE, 0x10  ; Compile only mask
.equ NFL, 0x0f      ; Name field length mask

; flags
.equ BUSYIDLE_MASK, 0xc000
.equ fBUSY,   15  ; 0=IDLE, 1=BUSY
.equ fIDLE,   14  ; 0=IDLE, 1=BUSY
.equ edirty,  12  ; eeprom status dirty
.equ fFC2,    11  ; Flow control for UART2
.equ ixoff2,  10  ; XON/XOFF flag for UART2
.equ fLOCK,   9   ; Disable writes to flash and eeprom
.equ tailcall,8   ; Disable tailcall optimisation
.equ noclear, 6   ; dont clear optimisation flags 
.equ idup,    5   ; Use dupzeroequal instead of zeroequal
.equ izeroeq, 4   ; Use bnz instead of bz if zeroequal
.equ istream, 3
.equ fFC1,    2   ; 0=FC, 1 = noFC for UART1
.equ ixoff1,  1   ; XON/XOFF flag for UART1
.equ idirty,  0


;;; For Flow Control
.equ XON,   0x11
.equ XOFF,  0x13


.equ IVECSIZE, 64

;;; USER AREA sizes for the OPERATOR task

;;; User variables and area
.equ us0,          - 32         ; Start of parameter stack
.equ ur0,          - 30         ; Start of return stack
.equ uemit,        - 28         ; User EMIT vector
.equ ukey,         - 26         ; User KEY vector
.equ ukeyq,        - 24         ; User KEY? vector
.equ ulink,        - 22         ; Task link
.equ ubase,        - 20         ; Number Base
.equ utib,         - 18         ; TIB address
.equ utask,        - 16         ; Task area pointer
.equ uflg,         - 14         ; ACCEPT. true = CR has been received
.equ ustatus,      - 13         ; IDLE / BUSY
.equ ursave,       - 12         ; Saved return stack pointer
.equ ussave,       - 10         ; Saved parameter stack pointer
.equ upsave,       - 8          ; Saved P pointer
.equ usource,      - 6          ; Two cells
.equ utoin,        - 2          ; Input stream
.equ uhp,            0          ; Hold pointer
.equ urbuf,        ustart-us0 + UADDSIZE + 2        ; return stack
.equ usbuf,        urbuf + RETURN_STACK_SIZE        ; Parameter stack
.equ usbuf0,       usbuf - 2
.equ utibbuf,      usbuf + PARAMETER_STACK_SIZE ; Terminal Input buffer

;;;  Initial USER area pointer (operator)
.equ u0,           ustart-us0
.equ uareasize,    -us0+RETURN_STACK_SIZE+PARAMETER_STACK_SIZE+TIB_SIZE+HOLD_SIZE+UADDSIZE+2

;;; Start of free ram
.equ dpdata,       ustart+uareasize

;;; Variables in EEPROM
.ifdef PEEPROM
.equ eeprom,       PEEPROM
.equ dp_start,     eeprom + 0x0000 ; TURNKEY
.equ dp_ram,       eeprom + 0x0002 ; FLASH dictionary pointer
.equ dp_eeprom,    eeprom + 0x0004 ; EEPROM dictionary pointer
.equ dp_flash,     eeprom + 0x0006 ; RAM dictionary pointer
.equ latest,       eeprom + 0x0008 ; Pointer to latest dictionary word
.equ dpeeprom,     eeprom + 0x000a
.else

.endif
;****************************************************
.bss
ibufl:      .space IBUFSIZEL
ibufh:      .space IBUFSIZEH

intcon1dbg:  .space 2

txqueue1:
tbuf_len1:   .space 2
tbuf_wr1:    .space 2
tbuf_rd1:    .space 2
tbuf_lv1:    .space 2
tbuf1:       .space TX1_BUF_SIZE

rxqueue1:
rbuf_len1:   .space 2
rbuf_wr1:    .space 2
rbuf_rd1:    .space 2
rbuf_lv1:    .space 2
rbuf1:       .space RX1_BUF_SIZE

.ifdecl BAUDRATE2
.ifdecl _U2RXREG
txqueue2:
tbuf_len2:   .space 2
tbuf_wr2:    .space 2
tbuf_rd2:    .space 2
tbuf_lv2:    .space 2
tbuf2:       .space TX2_BUF_SIZE

rxqueue2:
rbuf_len2:   .space 2
rbuf_wr2:    .space 2
rbuf_rd2:    .space 2
rbuf_lv2:    .space 2
rbuf2:       .space RX2_BUF_SIZE
.endif
.endif

temp:       .space 2
ibase:      .space 2
iaddr:      .space 2
iflags:     .space 2
status:     .space 2        ; 0 = allow CPU idle 
load:       .space 2
load_acc:   .space 4

ms_count:   .space 2

dpSTART:    .space 2
.ifdef PEEPROM
dpRAM:      .space 2
dpEEPROM:   .space 2
dpFLASH:    .space 2 ; DP's and LATEST in RAM
dpLATEST:   .space 2
dpSAVE:     .space 10
.equ MARKER_LENGTH, 5
.else
dpRAM:      .space 2
dpLATEST:   .space 2
dpFLASH:    .space 2 ; DP's and LATEST in RAM
dpSAVE:     .space 8
.equ MARKER_LENGTH, 4
.endif

.ifdecl INTTREG
IVECTAB:    .space IVECSIZE*2 ; space for 64 interrupt vectors
.endif

cse:        .space 2 ; Current data section 0=flash, 1=eeprom, 2=ram
prompt:     .space 2
state:      .space 2 ; Compilation state
upcurr:     .space 2 ; Current USER area pointer
ustart:     .space uareasize ; The operator user area

; Start of code !
.text
;;; *************************************
;;; COLD dictionary data
COLDLIT:
STARTV: .word      0
.ifdef PEEPROM
DPD:    .word      dpdata
DPE:    .word      dpeeprom
DPC:    .word      handle(KERNEL_END)+PFLASH
LW:     .word      handle(lastword)+PFLASH
.equ coldlitsize, 5
.else
DPD:    .word      dpdata
LW:     .word      handle(lastword)+PFLASH
DPC:    .word      handle(KERNEL_END)+PFLASH
.equ coldlitsize, 4
.endif
;;; *************************************************
;;; WARM user area data
.equ warmlitsize, 14
WARMLIT:
        .word      0x0000                ; CSE RAM
        .word      handle(DOTSTATUS)+PFLASH
        .word      0x0000                ; STATE
        .word      u0                    ; UP
        .word      usbuf0                ; S0, First user variable
        .word      urbuf                 ; R0
        .word      handle(TX1)+PFLASH
        .word      handle(RX1)+PFLASH
        .word      handle(RX1Q)+PFLASH
        .word      u0                    ; ULINK
        .word      BASE_DEFAULT          ; BASE
        .word      utibbuf               ; TIB
        .word      handle(OPERATOR_AREA)+PFLASH ; TASK
        .word      0x0000
;;; *************************************************
__OscillatorFail:
;        clr     intcon1dbg
;        reset
__AddressError:
__StackError:
__MathError:
        mov     INTCON1, W0
        mov     W0, intcon1dbg
        reset

__T1Interrupt:
; No nested interrupts, T1 interrupt must the first interrupt to be enabled
        bset    INTCON1, #NSTDIS
        bclr    IFS0, #T1IF
        inc     ms_count

.if IDLE_MODE == 1
.if CPU_LOAD == 1        
        push.s
        mov     TMR3, W0
        clr     TMR3
        
        add     load_acc
        clr     W0
        addc    load_acc+2
        
        cp0.b   ms_count
        bra     nz, RETFIE_T1_0
        
        mov     #FCY/3126, W2
        mov     load_acc+2, W1
        mov     load_acc, W0
        clr     load_acc
        clr     load_acc+2
        repeat  #17
        div.ud  W0, W2
        mov     W0, load
RETFIE_T1_0:
        pop.s
.endif
.endif
        retfie

__U1RXInterrupt:
        push.s
.ifdef PEEPROM
        push    TBLPAG
.endif
        inc2    W14, W14
__U1RXInterrupt0:
        bclr    IFS0, #U1RXIF
        bset    iflags, #istream      ; Indicate UART activity.
        mov     rbuf_len1, WREG
        sub     rbuf_lv1, WREG        ; level - len
        bra     nn, U1RX_ERR1         ; Queue full ?
        bclr    U1STA, #OERR
        mov     U1RXREG, W0
.if (CTRL_O_WARM_RESET == 1)
        cp      W0, #15
        bra     z, RESET_FF_1
.endif

.if FC1_TYPE == 1
        btsc    iflags, #fFC1
        bra     U1_SKIP_FC_1
        cp      W0, #XOFF
        bra     z, __U1RXInterrupt3
.endif
U1_SKIP_FC_1:
        mov     W0, [++W14]
        mlit    handle(U1RXQUEUE_DATA)+PFLASH
        rcall   CQUEUE_TO
        
        btsc    iflags, #fFC1
        bra     U1_SKIP_FC_2
        mov     #RX1_OFF_FILL, W0
        cp      rbuf_lv1            ; rbuf_lvl - #RX1_OFF_FILL
        bra     n, __U1RXInterrupt3
.if FC1_TYPE == 1
__U1RXInterrupt2:
        mov     #XOFF, W0
        mov     W0, U1TXREG
        bset    iflags, #ixoff1
.else
.if  FC1_TYPE == 2
        bset    U1RTSPORT, #U1RTSPIN
.endif
.endif
U1_SKIP_FC_2:

__U1RXInterrupt3:
        btsc    U1STA, #URXDA
        bra     __U1RXInterrupt0
__U1RXTXIRQ_END:
        sub     W14, #2, W14
.ifdef PEEPROM
        pop     TBLPAG
.endif
ALT_INT_EXIT:
        pop.s
        retfie

U1RX_ERR1:
        btss    U1STA, #TRMT
        bra     U1RX_ERR1
        mov     U1RXREG, W0
        mov     #'|', W0
        mov     W0, U1TXREG
        bra     __U1RXInterrupt3

__U1TXInterrupt:
        push.s
.ifdef PEEPROM
        push    TBLPAG
.endif
        add     W14, #2, W14
        bclr    IFS0, #U1TXIF
__U1TXInterrupt0:
        btsc    U1STA, #UTXBF
        bra     __U1RXTXIRQ_END
        cp0     tbuf_lv1
        bra     z, __U1RXTXIRQ_END
        mlit    handle(U1TXQUEUE_DATA)+PFLASH
        rcall   CQUEUE_FROM
        mov     [W14--], W0
        mov     W0, U1TXREG
        bra     __U1RXTXIRQ_END

.ifdecl BAUDRATE2
.ifdecl _U2RXREG
__U2RXInterrupt:
        push.s
.ifdef PEEPROM
        push    TBLPAG
.endif
        inc2    W14, W14
__U2RXInterrupt0:
        bclr    IFS1, #U2RXIF
        bset    iflags, #istream      ; Indicate UART activity.
        mov     rbuf_len2, WREG
        sub     rbuf_lv2, WREG        ; level - len
        bra     nn, U2RX_ERR1         ; Queue full ?
        bclr    U2STA, #OERR
        mov     U2RXREG, W0

.if (CTRL_O_WARM_RESET == 1)
        cp      W0, #15
        bra     z, RESET_FF_1
.endif

.if FC2_TYPE == 1
        btsc    iflags, #fFC2
        bra     U2_SKIP_FC_1
        cp      W0, #XOFF
        bra     z, __U2RXInterrupt3
.endif
U2_SKIP_FC_1:
        mov     W0, [++W14]
        mlit    handle(U2RXQUEUE_DATA)+PFLASH
        rcall   CQUEUE_TO
        
        btsc    iflags, #fFC2
        bra     U2_SKIP_FC_2
        mov     #RX2_OFF_FILL, W0
        cp      rbuf_lv2
        bra     n, __U2RXInterrupt3
.if FC2_TYPE == 1
__U2RXInterrupt2:
        mov     #XOFF, W0
        mov     W0, U2TXREG
        bset    iflags, #ixoff2
.else
.if  FC2_TYPE == 2
        bset    U2RTSPORT, #U2RTSPIN
.endif
.endif
U2_SKIP_FC_2:
__U2RXInterrupt3:
        btsc    U2STA, #URXDA
        bra     __U2RXInterrupt0
__U2RXTXIRQ_END:
        bra     __U1RXTXIRQ_END

U2RX_ERR1:
        btss    U2STA, #TRMT
        bra     U2RX_ERR1
        mov     U2RXREG, W0
        mov     #'|', W0
        mov     W0, U2TXREG
        bra     __U2RXInterrupt3

__U2TXInterrupt:
        push.s
.ifdef PEEPROM
        push    TBLPAG
.endif
        add     W14, #2, W14
        bclr    IFS1, #U2TXIF
__U2TXInterrupt0:
        btsc    U2STA, #UTXBF
        bra     __U1RXTXIRQ_END
        cp0     tbuf_lv2
        bra     z, __U1RXTXIRQ_END
        mlit    handle(U2TXQUEUE_DATA)+PFLASH
        rcall   CQUEUE_FROM
        mov     [W14--], W0
        mov     W0, U2TXREG
        bra     __U1RXTXIRQ_END
.endif
.endif
;*******************************************************************
.ifdecl INTTREG
__DefaultInterrupt:
        btss    INTCON2, #ALTIVT
        bra     __AddressError
        push.s                   ; Handle interrupt via lookup table
        mov     INTTREG, W0
        sl      W0, W1
        and     #0xff, W1
        mov     #IVECTAB, W0
        add     W0, W1, W2
        mov     [W2], W0
        goto    W0
.else
__DefaultInterrupt:
        bra     __AddressError
.endif


; *******************************************************************
; ibufmask = 0xffc0 or 0xfc00
; ibuflen  = 0x0040 or 0x0400 
iupdatebuf:
;if (ibase != (iaddr&0xffc0)) // ibufmask = 0xffc0 or fc00 
;   if (idirty)
;       writebuffer_to_imem
;   endif
;   fillbuffer_from_imem
;   ibase = iaddr&0xffc0
;endif
        mov     iaddr, W0
        mov     #IBUFMASK, W1
        and     W0, W1, W0
        cp      ibase
        bra     nz, iupdatebuf0
        return

iupdatebuf0:
        rcall   IFLUSH
        mov     iaddr, W0
        mov     #IBUFMASK, W1
        and     W0, W1, W0
        mov     W0, ibase
fill_buffer_from_imem:
        clr     W0
        rcall   wbti_init
fill_buffer_from_imem1:
        mov.w   #IBUFLEN1, W3
fill_buffer_from_imem2:
        tblrdh.b [W2], [W1++]
        tblrdl   [W2++], [W0++]
        dec      W3, W3
        bra      nz, fill_buffer_from_imem2
        dec      W4, W4
        bra      nz, fill_buffer_from_imem1
        return

wait_silence:
        rcall   LOCKED
.if FC1_TYPE == 1
        btsc    U1STA, #UTXBF
        bra     wait_silence
        mov     #XOFF, W2
        mov     W2, U1TXREG
        bset    iflags, #ixoff1
.else
.if FC1_TYPE == 2
        bset    U1RTSPORT, #U1RTSPIN
.endif
.endif
.if FC1_TYPE == 1
wbtil:
        bclr    iflags, #istream
        ; The delay here should be 10 character times long
        ; times = Fcy/baud*40
        mov     #(FCY/BAUDRATE1*10), W2     ;  This loop takes about 5 milliseconds @ 27 Mips
wbtil2: 
        btsc    iflags, #istream    ; Check for UART  activity.
        bra     wbtil               ; 5 cycles per round
        dec     W2, W2              ;
        bra     nz, wbtil2          ;
.endif
        return
;***********************************************************
wbti_init:
        mov.w   W0, NVMCON
        mov.w   #ibufl, W0 ; Low word flash buffer in ram
        mov.w   #ibufh, W1 ; High byte buffer
        mov.w   ibase, W2
        mov.w   #IBUFLEN2, W4
.ifdef PEEPROM
        clr     TBLPAG
.endif
        tblwtl  W2, [W2]          ; Set page address
        return

write_buffer_to_imem:
;; Loop here until there are no more characters has been received for a while
;; from the UART.
;; The assumption is that the serial line is silent then.
        rcall   wait_silence
.if DEBUG_FLASH == 1
        mov     #'F', W2
        mov     W2, U1TXREG
.endif
        mov.w   #FLASH_ERASE, W0  ; #30F 0x4041,  24F 0x4058
        rcall   wbti_init
        rcall   EWENABLE0         ; Now the flash row has been erased.

        mov.w   #FLASH_WRITE, W0  ;  30F 0x4001,  24F 0x4004
        rcall   wbti_init
wbtil3:
        mov.w   #IBUFLEN1, W3
wbtil4:
        tblwth.b  [W1++], [W2]
        tblwtl.w  [W0++], [W2++]
        dec     W3, W3
        bra     nz, wbtil4
        rcall   EWENABLE0   ; Now the flash row has been written.
        dec     W4, W4
        bra     nz, wbtil3  ; write more rows for big flashblocks

        clr     W0
        rcall   wbti_init
wbtil5:
        mov.w   #IBUFLEN1, W3
wbtil6:
        tblrdh.b  [W2], W5
        cp.b      W5, [W1++]
        bra       nz, verify_imem_2
        tblrdl    [W2++], W5
        cp        W5, [W0++]
        bra       nz, verify_imem_2
        dec       W3, W3
        bra       nz, wbtil6
        dec       W4, W4
        bra       nz, wbtil5
        bclr      iflags, #idirty
        setm      ibase       ; Now the flash row has been verified
        return

verify_imem_2:
        reset

; LITERAL  x --           append numeric literal as inline code
        .pword   0
LITERAL_L:
        .byte   NFA|IMMED|7
        .ascii "literal"
        .align 2
LITERAL:
        mov     [W14--], W0
        mov     W0, W1
        sl      W0, #4, W0
        lsr     W1, #12, W1
        ior     #0x20, W1             ; mov  #literal , W0
        mov     W0, [++W14]           ; Lower 16 bit of the instruction
        mov     W1, [++W14]           ; High 8 bit of the instruction
        rcall   AS_COMMA              ; Special instruction to append
                                      ; assembly code
; wBhh hddd dggg ssss 0010 1111 0000 0000
        mov     #0x2f00, W0
; 0111 1www     0111 1000
        mov     #0x0078, W1       ; mov W0, [++W14]
        mov     W0, [++W14]
        mov     W1, [++W14]
        rcall   AS_COMMA
        return

        .pword   paddr(LITERAL_L)+PFLASH
TO_A_L:
        .byte   NFA|INLINE|2
        .ascii  ">a"
        .align 2
        mov     [W14--], W11
        return

        .pword   paddr(TO_A_L)+PFLASH
A_FROM_L:
        .byte   NFA|INLINE|2
        .ascii  "a>"
        .align 2
        mov     W11, [++W14]
        return

        .pword   paddr(A_FROM_L)+PFLASH
IDLE_L:
        .byte   NFA|4
        .ascii  "idle"
        .align 2
IDLE_:
        bclr     iflags, #fIDLE
        return
        
        .pword   paddr(IDLE_L)+PFLASH
BUSY_L:
        .byte   NFA|4
        .ascii  "busy"
        .align 2
BUSY_:
        bset     iflags, #fIDLE
        return
        
        .pword   paddr(BUSY_L)+PFLASH
LOAD_L:
        .byte   NFA|4
        .ascii  "load"
        .align 2
LOAD_:
        mov     load, WREG
        mov     W0, [++W14]
        return
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        .pword   paddr(LOAD_L)+PFLASH
COLD_L:
        .byte   NFA|5
        .ascii  "empty"
        .align 2
COLD:
.ifdef PEEPROM
        mlit    handle(COLDLIT)+PFLASH
        mlit    dp_start
        mlit    coldlitsize
        rcall   WMOVE
        clr     intcon1dbg
.else
        rcall   DP_COLD
        rcall   DP_TO_RAM
.endif
        return
;        bra     RESET_FF

        .pword   paddr(COLD_L)+PFLASH
WARM_L:
        .byte   NFA|4
        .ascii  "warm"
        .align 2
WARM_:
        rcall   IFLUSH
        rcall   DP_TO_EEPROM
RESET_FF:
        mlit    10
        rcall   MS
RESET_FF_1:
        reset

__reset:
WARM:
.ifdecl CLKDIV
        clr     CLKDIV   ; Use full FRC frequency
                         ; PLL PRE/POST scalers are 2.
.endif
        MOV     #urbuf, W15    ;Initalize RP
        setm    SPLIM
        
        CLR     W0
        MOV     W0, W14
        REPEAT  #12
        MOV     W0, [++W14]

        clr     W0              ; Fill operator return and parameter stacks with 0x00
        mov     #txqueue1, W14  ; Dont overwrite INTCONDBG
        mov     #PFLASH, W1
FILL_RAM:
        mov.w   W0, [W14++]
        cp      W14, W1
        bra     nz, FILL_RAM
        mov     #usbuf0, W14
        setm    ibase
        clr     iflags
.ifdecl INTTREG
        mov     #IVECTAB, W0
        mov     #IVECSIZE, W1
        mov     #handle(__DefaultInterrupt), W2
WARM_FILL_IVEC:
        mov     W2, [W0++]
        dec     W1, W1
        bra     nz, WARM_FILL_IVEC
.endif

        setm    PMD1
        setm    PMD2
.ifdecl PMD3
        setm    PMD3
.endif

WARM_0:

; Init the serial TX buffer
        rcall   U1TXQUEUE
        rcall   CQUEUEZ
; Init the serial RX buffer
        rcall   U1RXQUEUE
        rcall   CQUEUEZ
        
.ifdecl BAUDRATE2
.ifdecl _U2RXREG
; Init the serial TX buffer
        rcall   U2TXQUEUE
        rcall   CQUEUEZ
; Init the serial RX buffer
        rcall   U2RXQUEUE
        rcall   CQUEUEZ
.endif
.endif

.ifndecl __dsPIC30F
        mov     OSCCON, W0
        asr     W0, #8, W1
        and     #7, W1
        sub     W1, #1, W0
        bra     z, PLL_IN_USE
        sub     W1, #3, W0
        bra     z, PLL_IN_USE
        bra     PLL_NOT_IN_USE

PLL_IN_USE:
.ifdecl PLLFBD
        mov     #PLL_FBD, W0
        mov     W0, PLLFBD
.endif
WAITFORLOCK:
        btss    OSCCON, #LOCK
        bra     WAITFORLOCK
PLL_NOT_IN_USE:
.endif

; Configure MS timer1
        bclr    PMD1, #T1MD
        mov     #MS_PR_VAL, W0
        mov     W0, PR1
        mov     #0x8000, W0
        mov     W0, T1CON

.if IDLE_MODE == 1
.if CPU_LOAD == 1
; Configure CPU load counter timer3
        bclr    PMD1, #T3MD
        mov     #0xA010, W0    ; Stop timer3 in idle mode, prescaler = 8
        mov     W0, T3CON
.endif
.endif

; Enable T1 interrupt
        bset    IEC0, #T1IE

;;;; Initialise the UART 1
.if FC1_TYPE == 2
        bclr    U1RTSTRIS, #U1RTSPIN
        bclr    U1RTSPORT, #U1RTSPIN
.endif
.ifdecl RPINR18
        setm    AD1PCFGL
        mov     #OSCCONL, W0
        mov.b   #0x46, W1
        mov.b   #0x57, W2
        mov.b   W1, [W0]
        mov.b   W2, [W0]
        bclr.b  OSCCONL, #IOLOCK
        
        mov     #RPINR18VAL, W0
        mov     W0, RPINR18
        mov.b   #0x0003, W0         ; U1TX
        mov.b   WREG, RPOR0+U1TXPIN
.endif

        bclr    PMD1, #U1MD

.ifdecl USE_ALTERNATE_UART_PINS
.if  (USE_ALTERNATE_UART_PINS == 1)
        bset    U1MODE, #ALTIO
.endif
.endif
.ifdecl UTXISEL1
        bset    U1STA, #UTXISEL0
.else
        bset    U1STA, #UTXISEL
.endif

        bset    U1MODE, #UARTEN
.ifdecl BRGH
        bset    U1MODE, #BRGH
.endif
        mov     #BAUD_DIV1, W0
        mov     W0, U1BRG
        bset    U1STA, #UTXEN

.ifdecl AUTOBAUD1
.if (AUTOBAUD1 == 1)
        bset    U1MODE, #ABAUD
WARM_ABAUD1:
        btsc    U1MODE, #ABAUD
        bra     WARM_ABAUD1
        bclr    IFS0, #U1RXIF
.endif
.endif
        bset    IEC0, #U1RXIE
        bset    IEC0, #U1TXIE

;;; Initialise UART2
.ifdecl BAUDRATE2
.ifdecl _U2RXREG
.if FC2_TYPE == 2
        bclr    U2RTSTRIS, #U2RTSPIN
        bclr    U2RTSPORT, #U2RTSPIN
.endif
.ifdecl RPINR19
        mov     #RPINR19VAL, W0
        mov     W0, RPINR19
        mov.b   #0x0005, W0         ; U2TX
        mov.b   WREG, RPOR0+U2TXPIN
.endif
       bclr    PMD1, #U2MD
.ifdecl UTXISEL0
        bset    U2STA, #UTXISEL0
.else
        bset    U2STA, #UTXISEL
.endif
        bset    U2MODE, #UARTEN

.ifdecl BRGH
        bset    U2MODE, #BRGH
.endif
        mov     #BAUD_DIV2, W0
        mov     W0, U2BRG
        bset    U2STA, #UTXEN

.ifdecl AUTOBAUD2
.if (AUTOBAUD2 == 1)
        bset    U2MODE, #ABAUD
WARM_ABAUD2:
        btsc    U2MODE, #ABAUD
        bra     WARM_ABAUD2
        bclr    IFS1, #U2RXIF
.endif
.endif
        bset    IEC1, #U2RXIE
        bset    IEC1, #U2TXIE
.endif
.endif

; Init the warm literals
        mlit    handle(WARMLIT)+PFLASH
        mlit    cse
        mlit    warmlitsize
        rcall   WMOVE

; Check if cold start is needed
.ifdef PEEPROM
        mlit    dp_start
.else
        rcall   FTURNKEY_A
.endif
        rcall   FETCH
        mov     [W14--], W0
        mov     #0xffff, W1
        cp      W0, W1
        bra     z, COLD
        rcall   DP_TO_RAM

				; Wait 10 ms for UARTs to reset 
        mlit    10
        rcall   MS

; Display INTCON1 and RCON restart reason
        btsc    intcon1dbg, #STKERR
        rcall   DOEMIT
        .word   'O'          ; NOP when executed
RQ_DIV0:
        btsc    intcon1dbg, #DIV0ERR
        rcall   DOEMIT
        .word   'D'
RQ_ADDR:
        btsc    intcon1dbg, #ADDRERR
        rcall   DOEMIT
        .word   'A'
RQ_BOR:
        btsc    RCON, #BOR
        rcall   DOEMIT
        .word   'B'
RQ_POR:
        btsc    RCON, #POR
        rcall   DOEMIT
        .word   'P'
RQ_TO:
        btsc    RCON, #WDTO
        rcall   DOEMIT
        .word   'W'
RQ_RI:
        btsc    RCON, #SWR
        rcall   DOEMIT
        .word   'S'
RQ_EXTR:
        btsc    RCON, #EXTR
        rcall   DOEMIT
        .word   'E'
RQ_END:

        clr     intcon1dbg
        clr     RCON

WARM1:
        rcall   XSQUOTE
        .byte   23
        .ascii  " FlashForth PIC24 5.0\r\n"
        .align 2
        rcall   TYPE
        mlit    XON
        rcall   EMIT
; TURNKEY
        rcall   TURNKEY
        cp0     [W14--]
        bra     z, STARTQ2

        rcall   XSQUOTE
        .byte   3
        .ascii  "ESC"
        .align 2
        rcall   TYPE
        mlit    0x0800
        rcall   MS
        rcall   KEYQ
        cp0     [W14--]
        bra     z, STARTQ1
        rcall   KEY
        mlit    0x1b
        rcall   NOTEQUAL
        cp0     [W14]
        bra     z, STARTQ2
STARTQ1:
        rcall   TURNKEY
        rcall   EXECUTE
STARTQ2:
        bra     ABORT


        .pword   paddr(WARM_L)+PFLASH
TURNKEY_L:
        .byte   NFA|7
        .ascii  "turnkey"
        .align 2
TURNKEY:
        rcall   VALUE_DOES
        .word   dpSTART

; PAUSE  20 cycles, 5us@16MHz dsPIC30F 2.5 us for 33F and 24F
        .pword   paddr(TURNKEY_L)+PFLASH
PAUSE_L:
        .byte   NFA|5
        .ascii  "pause"
        .align 2
PAUSE:
        clrwdt
.if IDLE_MODE == 1
        mov     #BUSYIDLE_MASK, W0
        and     iflags, WREG
        bra     nz, PAUSE_BUSY
.if 0
        btsc    iflags, #fIDLE
        bra     PAUSE_BUSY
        btsc    iflags, #fBUSY
        bra     PAUSE_BUSY
.endif
        mov     #u0, W0        ; IDLE only in operator task.
        cp      upcurr
        bra     nz, PAUSE_BUSY
.if CPU_LOAD_LED == 1
        bclr    CPU_LOAD_TRIS, #CPU_LOAD_BIT
.if CPU_LOAD_LED_POLARITY == 0
        bset    CPU_LOAD_PORT, #CPU_LOAD_BIT
.else
        bclr    CPU_LOAD_PORT, #CPU_LOAD_BIT
.endif
.endif
        pwrsav  #1             ; Go to IDLE mode to save power.
PAUSE_BUSY:
.if CPU_LOAD_LED == 1
.if CPU_LOAD_LED_POLARITY == 0
        bclr    CPU_LOAD_PORT, #CPU_LOAD_BIT
.else
        bset    CPU_LOAD_PORT, #CPU_LOAD_BIT
.endif
.endif
.endif
PAUSE2:
        mov     upcurr, W0
        mov     W14, [W0+ussave]    ; Save SP W14
        mov     W15, [W0+ursave]    ; Save RP W15
        mov     W13, [W0+upsave]    ; Save P pointer
        mov     [W0+ulink], W0      ; Set UP

        setm    SPLIM               ; Disable Return Stack overflow protection
        disi    #4
        mov     W0, upcurr
        mov     [W0+upsave], W13    ; Restore P pointer
        mov     [W0+ursave], W15    ; Restore RP
        mov     [W0+ussave], W14    ; Restore SP W14
        
        mov     [W0+us0], W1        ; Set SPLIM
        mov     W1, SPLIM
        return

        .pword   paddr(PAUSE_L)+PFLASH
CWD_L:
        .byte   NFA|INLINE|3
        .ascii  "cwd"
        .align 2
CWD:
        clrwdt
        return

; INT!  ( xt intnumber -- ) intnumber 8..
; Store interrupt vector in alternate interrupt vector table
; Stored directly in Flash on the 30F series     intnumber 0..61 
; Stored in ram revector table in 24,33 series   intnumber 0..83

        .pword   paddr(CWD_L)+PFLASH
INTERRUPT_STORE_L:
        .byte   NFA|4
        .ascii  "int!"
        .align 2
INTERRUPT_STORE:
.ifdecl INTTREG
        mov     #IVECTAB, W0
        mov     [W14--], W1
        sl      W1, W1
        add     W0, W1, W0
        mov     #PFLASH, W1
        mov     [W14--], W2
        sub     W2, W1, W1
        mov     W1, [W0]
        return
.else
        mov     #0x3f, W0
        mov     [W14--], W1
        and     W1, W0, W1
        sl      W1, W1
        mov     #PAIVT+PFLASH+4, W0
        add     W1, W0, W0
        mov     #PFLASH, W1
        mov     [W14], W2
        sub     W2, W1, [W14]
        clr     W3             ; hibyte
        rcall   ISTORE_RAW
        rcall   IFLUSH
        return
.endif

; IVT  ( -- )  Use the normal interrupt vector table
        .pword   paddr(INTERRUPT_STORE_L)+PFLASH
IVT_L:
        .byte   NFA|INLINE|3
        .ascii  "ivt"
        .align  2
IVT:
        bclr    INTCON2, #ALTIVT
        return

; AIVT ( -- ) Use the alternate interrupt vector table
        .pword   paddr(IVT_L)+PFLASH
AIVT_L:
        .byte   NFA|INLINE|4
        .ascii  "aivt"
        .align  2
AIVT:
        bset    INTCON2, #ALTIVT
        return

; [i ( -- ) enter the interrupt context
        .pword   paddr(AIVT_L)+PFLASH
BRACKETI_L:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "[i"
        .align  2
BRACKETI:
.ifndecl INTTREG
        push.s          ; W0...W3
.endif
.ifdef PEEPROM
        push    TBLPAG  ; Used by eeprom access
.endif
        push    W13     ; Preg        
        push    RCOUNT  ; used by repeat
        add     W14, #2, W14
        return

; i] ( -- ) exit the interrupt context
        .pword   paddr(BRACKETI_L)+PFLASH
IBRACKET_L:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "i]"
        .align  2
IBRACKET:
        sub     W14, #2, W14
        pop     RCOUNT  ; used by repeat
        pop     W13     ; Preg
.ifdef PEEPROM
        pop     TBLPAG  ; Used by eeprom access
.endif
.ifndecl INTTREG
        pop.s           ; W0...W3
.endif
        return


; di ( -- ) disable interrupts
        .pword   paddr(IBRACKET_L)+PFLASH
DI_L:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "di"
        .align  2
DI:
        push    SR
        mov.b   #0xe0, W0
        ior.b   SRL
        return

; ei ( -- ) enable interrupts
        .pword   paddr(DI_L)+PFLASH
EI_L:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "ei"
        .align  2
EI:
        pop     SR
        return

        .pword   paddr(EI_L)+PFLASH
OPERATOR_L:
        .byte   NFA|8
        .ascii  "operator"
        .align  2
OPERATOR:
        call    DOCREATE        ; Use call to align with PIC18 and AVR
        .pword  paddr(OPERATOR_AREA)+PFLASH
OPERATOR_AREA: 
        .word   ustart-us0      ; user pointer
        .byte   UADDSIZE
        .byte   RETURN_STACK_SIZE
        .byte   PARAMETER_STACK_SIZE
        .byte   TIB_SIZE+HOLD_SIZE


;  rcall, ( rel-addr -- )
        .pword   paddr(OPERATOR_L)+PFLASH
RCALL_L:
        .byte   NFA|6
        .ascii  "rcall,"
        .align  2
RCALL_:
        asr     [W14], [W14]        ; 2/
        mlit    #7
        rcall   AS_COMMA
        return

;  return, ( -- )
        .pword   paddr(RCALL_L)+PFLASH
RETURN_L:
        .byte   NFA|7
        .ascii  "return,"
        .align  2
RETURN_:
        rcall   FALSE_
        mlit    #6
        rcall   AS_COMMA
        return

;  retfie, ( -- )
        .pword   paddr(RETURN_L)+PFLASH
RETFIE_L:
        .byte   NFA|7
        .ascii  "retfie,"
        .align  2
RETFIE_:
        mlit    #0x4000
        mlit    #6
        rcall   AS_COMMA
        return

;  bra, ( cc rel-addr -- )
        .pword   paddr(RETFIE_L)+PFLASH
BRA_L:
        .byte   NFA|4
        .ascii  "bra,"
        .align  2
BRA_:
        asr     [W14], [W14]        ; 2/
        rcall   SWOP
        mlit    #0x30
        rcall   OR
        rcall   AS_COMMA
        return

;  as0 ( bit ram-addr -- u ) 
; : bset, swap dup >r 8 u/ + r> $7 and #12 lshift or ;
        .pword   paddr(BRA_L)+PFLASH
AS0_L:
        .byte   NFA|3
        .ascii  "as0"
        .align  2
AS0:
        rcall   SWOP
        mov     [W14], [W15++]  ; dup >r
        mlit    8
        rcall   USLASH
        rcall   PLUS
        mov     [--W15], [++W14]
        mlit    0x7
        rcall   AND
        mlit    13
        rcall   LSHIFT
        rcall   OR
        return

; bclr, ( bit ram-addr -- ) clr bit in ram
        .pword   paddr(AS0_L)+PFLASH
BCLR_L:
        .byte   NFA|5
        .ascii  "bclr,"
        .align  2
BCLR_:
        rcall   AS0
        mlit    0xa9
        rcall   AS_COMMA
        return
                    
; bset, ( bit ram-addr -- ) set bit in ram
        .pword   paddr(BCLR_L)+PFLASH
BSET_L:
        .byte   NFA|5
        .ascii  "bset,"
        .align  2
BSET_:
        rcall   AS0
        mlit    0xa8
        rcall   AS_COMMA
        return
; btst, ( bit ram-addr -- ) test bit in ram -> STATUS, Z bit
        .pword   paddr(BSET_L)+PFLASH
BTST_L:
        .byte   NFA|5
        .ascii  "btst,"
        .align  2
BTST_:
        rcall   AS0
        mlit    0xab
        rcall   AS_COMMA
        return

; btsc, ( bit ram-addr -- ) bit test f, skip if clear
        .pword   paddr(BTST_L)+PFLASH
BTSC_L:
        .byte   NFA|5
        .ascii  "btsc,"
        .align  2
BTSC_:
        rcall   AS0
        mlit    0xaf
        rcall   AS_COMMA
        return

; btss, ( bit ram-addr -- ) bit test f, skip if set
        .pword   paddr(BTSC_L)+PFLASH
BTSS_L:
        .byte   NFA|5
        .ascii  "btss,"
        .align  2
BTSS_:
        rcall   AS0
        mlit    0xae
        rcall   AS_COMMA
        return

; cf! ( datal datah addr -- )
        .pword   paddr(BTSS_L)+PFLASH
CF_STORE_L:
        .byte   NFA|3
        .ascii  "cf!"
        .align  2
CF_STORE:
        rcall   SWOP
        mov     [W14--], W3
        rcall   CFISTORE
        return

; cf@ ( addr -- datal datah)
        .pword   paddr(CF_STORE_L)+PFLASH
CF_FETCH_L:
        .byte   NFA|3
        .ascii  "cf@"
        .align  2
CF_FETCH:
        rcall   FETCH
        mov     W3, [++W14]
        return

; as, ( datal datah -- )
        .pword   paddr(CF_FETCH_L)+PFLASH
AS_COMMA_L:
        .byte   NFA|3
        .ascii  "as,"
        .align  2
AS_COMMA:
        mov     [W14--], W0
.if 1        
        cp      W0, W10
        bra     nz, AS_COMMA1
        sub     #0x78, W10
        bra     nz, AS_COMMA1  ; hibytecheck
        
        mov     [W14], W1
        mov     #0x002e, W2
        cp      W1, W2
        bra     nz, AS_COMMA1  ; mov [W14--], W0  ???
        mov     #0x2f00, W2
        cp      W2, W12
        bra     nz, AS_COMMA1  ; mov W0, [++W14]  ???
        sub     W14, #2, W14
        rcall   IDPMINUS
        mov     #0, W12
        mov     #0, W10
        bra     AS_COMMA2
.endif        
AS_COMMA1:
        mov     W0, W3          ; hibyte
        mov     W0, W10
        mov     [W14], W12
        rcall   IHERE
        rcall   CFISTORE
        rcall   CELL
        rcall   IALLOT
AS_COMMA2:
        return

; i, ( data  -- )  upper byte is in 'hibyte'
;        .pword   paddr(AS_COMMA_L)+PFLASH
ICOMMA_L:
        .byte   NFA|2
        .ascii  "i,"
        .align  2
ICOMMA:
        rcall   IHERE
        rcall   STORE
        rcall   CELL
        rcall   IALLOT
        return

; cf,    xt --  append codefield
        .pword  paddr(AS_COMMA_L)+PFLASH
COMMAXT_L:
        .byte   NFA|3
        .ascii  "cf,"
        .align  2
COMMAXT:
        rcall   IHERE
        rcall   MINUS
        dec2    [W14], [W14]
        rcall   RCALL_
        return

        .pword   paddr(COMMAXT_L)+PFLASH
IFLUSH_L:
        .byte   NFA|6
        .ascii  "iflush"
        .align  2
IFLUSH:
        btsc    iflags, #idirty
        bra     write_buffer_to_imem
        return
        
; data addr IC! Address is in W0
ICSTORE:
        rcall   ISTORE_ADDRCHK
        clr     W3               ; hibyte
        rcall   ISTORE_SUB
        mov.b   W1, [W0]
        return

; data addr I!  Address is in W0
CFISTORE:
        mov     [W14--], W0
        bra     ISTORECF
ISTORE:
        clr     W3
ISTORECF:
        rcall   ISTORE_ADDRCHK
ISTORE_RAW:
        rcall   ISTORE_SUB
        mov     W1, [W0]
ISTORE1:
        return

ISTORE_SUB:
        mov     #PFLASH, W1
        sub     W0, W1, W0
        mov     W0, iaddr       ; W0 = addr, iaddr = addr
        push    W3
        rcall   iupdatebuf      ; uses w3
        pop     W3
        mov     iaddr, W0
        
        mov     #IBUFSIZEL-1, W1
        and     W0, W1, W2
        lsr     W2,#1,W0
        mov     #ibufh, W1
        add     W1, W0, W0

        mov.b   W3, [W0]        ; hibyte

        mov     #ibufl, W1
        add     W2, W1, W0
        mov     [W14--], W1
        bset    iflags, #idirty
        return              ; !!!!!!!!!!!!!!!!!!
ISTORE_ADDRCHK:
        mov     #handle(KERNEL_END)+PFLASH, W1
        cp      W0, W1
        bra     LTU, ISTORE_ADDRERR
        return
ISTORE_ADDRERR:
        bset    INTCON1, #ADDRERR

PCFETCH1:
        tblrdl.b [W0], [W14]
        return

ICFETCH:
        rcall   IFETCH_INIT
        cp      W1, W2
        bra     NZ, PCFETCH1
        mov     #IBUFSIZEL-1, W1
        and     W1, W0, W0
        mov     #ibufl, W1
        add     W1, W0, W0
        mov.b   [W0],[W14]
        return

PFETCH1:
        clr     W3
        tblrdh.b [W0], W3
        tblrdl  [W0], [W14]
        return

IFETCH:
        rcall    IFETCH_INIT
        cp       W1, W2
        bra      NZ, PFETCH1
        mov      #IBUFSIZEL-1, W1
        and      W1, W0, W0
        mov      W0, W2
        mov      #ibufl, W1
        add      W1, W0, W1
        mov      [W1],[W14]

        lsr      W2, #1, W0
        mov      #ibufh, W1
        add      W1, W0, W0
        clr      W3
        mov.b    [W0], W3       ; hibyte
        return
        
IFETCH_INIT:
.ifdef PEEPROM
        clr      TBLPAG
.endif
        mov      #PFLASH, W1
        sub      W0, W1, W0
        mov      ibase, W1
        mov      #IBUFMASK, W2
        and      W2, W0, W2
        return

EWENABLE:
        mov     W1, NVMCON
EWENABLE0:
        disi    #5
        mov     #0x55, W3       ; W3 selected to avoid clash in flash write routine.
        mov     W3, NVMKEY
        mov     #0xaa, W3
        mov     W3, NVMKEY
        bset    NVMCON, #WR
        nop
        nop
EWENABLE1:
        btsc    NVMCON, #WR
        bra     EWENABLE1       ; Now the cell has been stored
        return

.ifdef PEEPROM
ESTORE:
        rcall   wait_silence
        mov.w   #0xf000, W1 
        ior.w   W0, W1, W0
        mov.w   #0x7f, W1
        mov     W1, TBLPAG
.ifndef __24F16KA102
        TBLWTL  [W14], [W0]
        mov.w   #EEPROM_ERASE, W1
        rcall   EWENABLE
.endif
        TBLWTL  [W14--], [W0]
        mov     #EEPROM_WRITE, W1
        rcall   EWENABLE
        clr     TBLPAG
        return


ECSTORE:
        mov.w   #0xf000, W1
        ior.w   W0, W1, W0
        mov.w   #0x7f, W1
        mov.w   W1, TBLPAG
ECSTORE1:
        mov.w   W0, W1
        mov.w   #0xfffe, W2
        and.w   W2, W0, W0
        tblrdl  [W0], W2
        btsc    W1, #0
        bra     ECSTORE2
        mov.w   #0xff00, W1
        and.w   W1, W2, W2
        mov.w   [W14], W1
        ior.w   W1, W2, [W14]
        bra     ESTORE
ECSTORE2:
        and.w   #0xff, W2
        mov.w   [W14], W1
        swap.w  W1
        ior.w   W1, W2, [W14]
        bra     ESTORE

EFETCH:
        mov.w   #0xf000, W1
        ior.w   W0, W1, W0
        mov.w   #0x7f, W1
        mov.w   W1, TBLPAG
        tblrdl  [W0], W0
        mov     W0, [W14]
        clr     TBLPAG
        return
ECFETCH:
        mov     W0, W2
        bclr    W0, #0
        rcall   EFETCH
        btsc    W2, #0
        swap.w  W0
        and.w   #0xff, W0
        mov.w   W0, [W14]
        return
.else
;;; Only for TURNKEY, DP_FLASH, DP_RAM, LATEST !
;;;
;;; Read the last non-FFhibyte word from one 1 Kbyte bank
;;; No size check, just finds the last non-FF entry.
; ( start-of-1k-blockaddr -- data )
;        dw      link
;link    set     $
;        db      NFA|3,"ee@"
EEREAD:
        mov     [W14], W0
        mov     #PFLASH, W1
        sub     W0, W1, W0
        mov     #0x400, W1
        add     W0, W1, W0      ; W0 = endof flash page.
.ifdef PEEPROM
        clr     TBLPAG
.endif
EEREAD1:
        tblrdl  [--W0], W1
        inc     W1, W1
        bra     z, EEREAD1
        dec     W1, W1
        mov     W1, [W14]
        return

EEINIT:
        rcall   LOCKED
        rcall   EEERASE
        bra     EEWRITE

;;; Write of word to first free (hibyte=FF) location in a 1 Kbyte bank
;;; ( data start-of-1K-blockaddr -- )
;        dw      link
;link    set     $
;        db      NFA|3,"ee!"
EEWRITE:
        rcall   wait_silence
.ifdef PEEPROM
        clr     TBLPAG
.endif
        mov     [W14], W0
        mov     #PFLASH, W1
        sub     W0, W1, W0
        mov     #0x200, W2
EEWRITE1:
        tblrdl  [W0++], W1
        inc     W1, W1
        bra     nz, EEWRITE2
        dec2    W0, W0
        dec2    W14, W14
        bra     EEWRITE3
EEWRITE2:
        dec     W2, W2
        bra     nz, EEWRITE1
        rcall   EEERASE        ; ( blockstart -- blockstart)
        mov     [W14--], W0
EEWRITE3:
        mov     #FLASH_WRITE_SINGLE, W1
        mov     W1, NVMCON
        tblwtl  [W14--], [W0]
        rcall   EWENABLE0
        return

; block-addr -- block-addr
EEERASE:
.if DEBUG_FLASH == 1
        mov     #'R', W2
        mov     W2, U1TXREG
.endif
        mov     #FLASH_ERASE, W0
        mov     W0, NVMCON
        mov     [W14], W0
        mov     #PFLASH, W1
        sub     W0, W1, W0
        tblwtl  W0, [W0]
        bra     EWENABLE0
.endif

        .pword   paddr(IFLUSH_L)+PFLASH
FLOCK_L:
        .byte   NFA|3
        .ascii  "fl+"
        .align  2
        bclr    iflags, #fLOCK
        return

        .pword   paddr(FLOCK_L)+PFLASH
FUNLOCK_L:
        .byte   NFA|3
        .ascii  "fl-"
        .align  2
        bset    iflags, #fLOCK
        return
        
LOCKED:
        btss    iflags, #fLOCK
        return
        bset    INTCON1, #ADDRERR

        .pword   paddr(FUNLOCK_L)+PFLASH
FCON_L:
        .byte   NFA|3
        .ascii  "u1+"
        .align  2
        bclr    iflags, #fFC1
        return

        .pword   paddr(FCON_L)+PFLASH
FCOFF_L:
        .byte   NFA|3
        .ascii  "u1-"
        .align  2
        bset    iflags, #fFC1
        return

        .pword   paddr(FCOFF_L)+PFLASH
.ifdecl BAUDRATE2
.ifdecl _U2RXREG
FCON2_L:
        .byte   NFA|3
        .ascii  "u2+"
        .align  2
        bclr    iflags, #fFC2
        return

        .pword   paddr(FCON2_L)+PFLASH
FCOFF2_L:
        .byte   NFA|3
        .ascii  "u2-"
        .align  2
        bset    iflags, #fFC2
        return

        .pword   paddr(FCOFF2_L)+PFLASH
.endif
.endif
STORE_L:
        .byte   NFA|1
        .ascii  "!"
        .align  2
STORE:
        mov.w   [W14--], W0
        mov.w   #PFLASH, W1
        cp      W0, W1
        bra     GEU, STORE1
        mov.w   [W14--], [W0]
        return
STORE1:
.ifdef PEEPROM
        mov.w   #PEEPROM, W1
        cp      W0, W1
        bra     GEU, ESTORE
.endif
        bra     ISTORE

        .pword   paddr(STORE_L)+PFLASH
CSTORE_L:
        .byte   NFA|2
        .ascii  "c!"
        .align  2
CSTORE:
        mov.w   [W14--], W0
        mov.w   #PFLASH, W1
        cp      W0, W1
        bra     GEU, CSTORE1
        mov.b   [W14], [W0]
        mov.w   [W14--], W0
        return
CSTORE1:
.ifdef PEEPROM
        mov.w   #PEEPROM, W1
        cp      W0, W1
        bra     GEU, ECSTORE
.endif
        bra     ICSTORE

        .pword   paddr(CSTORE_L)+PFLASH
FETCH_L:
        .byte   NFA|1
        .ascii  "@"
        .align  2
FETCH:
        mov.w   [W14], W0
        mov.w   #PFLASH, W1
        cp      W0, W1
        bra     GEU, FETCH1
        mov.w   [W0], [W14]
        return
FETCH1:
.ifdef PEEPROM
        mov.w   #PEEPROM, W1
        cp      W0, W1
        bra     GEU, EFETCH
.endif
        bra     IFETCH

       .pword   paddr(FETCH_L)+PFLASH
CFETCH_L:
        .byte   NFA|2
        .ascii  "c@"
        .align  2
CFETCH:
        mov.w   [W14], W0
        clr     [W14]
        mov.w   #PFLASH, W1
        cp      W0, W1
        bra     GEU, CFETCH1
        mov.b   [W0], [W14]
        return
CFETCH1:
.ifdef PEEPROM
        mov.w   #PEEPROM, W1
        cp      W0, W1
        bra     GEU, ECFETCH
.endif
        bra     ICFETCH

        .pword  paddr(CFETCH_L)+PFLASH
MSET_L:
        .byte   NFA|4
        .ascii  "mset"
        .align  2
MSET:
        mov     [W14--], W0
        mov.w   [W14--], W1
        ior.w   W1, [W0],[W0] 
        return

        .pword  paddr(MSET_L)+PFLASH
MCLR_L:
        .byte   NFA|4
        .ascii  "mclr"
        .align  2
MCLR:
        mov     [W14--], W0
        com.w   [W14--], W1
        and.w   W1, [W0],[W0] 
        return

        .pword  paddr(MCLR_L)+PFLASH
MTST_L:
        .byte   NFA|4
        .ascii  "mtst"
        .align  2
MTST:
        mov     [W14--], W0
        mov     [W14--], W1
        and.w   W1, [W0], W0
        mov     W0, [++W14] 
        return

; bset ( base-addr bit-index -- )
        .pword  paddr(MTST_L)+PFLASH
BSET__L:
        .byte   NFA|4
        .ascii  "bset"
        .align  2
BSET__:
        mov.w   [W14--], W0
        mov.w   [W14--], W1
        lsr     W0, #3, W2
        add     W2, W1, W1
        bclr    W1, #0
        bset    SR, #C
        bsw.c   [W1], W0
        return


; bclr ( base-addr bit-index -- )
        .pword  paddr(BSET__L)+PFLASH
BCLR__L:
        .byte   NFA|4
        .ascii  "bclr"
        .align  2
BCLR__:
        mov.w   [W14--], W0
        mov.w   [W14--], W1
        lsr     W0, #3, W2
        add     W2, W1, W1
        bclr    W1, #0
        bclr    SR, #C
        bsw.c   [W1], W0
        return

; btst ( base-addr bit-index -- f )
        .pword  paddr(BCLR__L)+PFLASH
BTST__L:
        .byte   NFA|4
        .ascii  "btst"
        .align  2
BTST__:
        mov.w   [W14--], W0
        mov.w   [W14--], W1
        lsr     W0, #3, W2
        add     W2, W1, W1
        bclr    W1, #0
        btst.c  [W1], W0
        rrc     W1, W1
        asr     W1, #15, W0
        mov     W0, [++W14]
        return

        .pword  paddr(BTST__L)+PFLASH
LSHIFT_L:
        .byte   NFA|6
        .ascii  "lshift"
        .align  2
LSHIFT:
        mov.w   [W14--], W0
        mov     [W14--], W1
        sl      W1, W0, W0
        mov     W0, [++W14]
        return

        .pword  paddr(LSHIFT_L)+PFLASH
RSHIFT_L:
        .byte   NFA|6
        .ascii  "rshift"
        .align  2
RSHIFT:
        mov.w   [W14--], W0
        mov     [W14--], W1
        lsr     W1, W0, W0
        mov     W0, [++W14]
        return

        .pword  paddr(RSHIFT_L)+PFLASH
NEQUAL_L:
        .byte   NFA|2
        .ascii  "n="
        .align  2
NEQUAL:
        mov.w   [W14--], W3     ; count
        mov.w   [W14--], W4     ; caddr2 in flash
        mov.w   [W14], W5       ; caddr1 in ram
        mov.w   W4, [W14]
        rcall   CFETCH          ; nfu
        mov.w   [W14], W0
        and.w   #NFL, W0
        cp.b    W0, W3
        bra     nz, NEQUAL_TRUE
NEQUAL1:
        inc     W4, W4
        inc     W5, W5
        mov.w   W4, [W14]
        rcall   CFETCH
        mov.w   W5, [++W14]
        rcall   CFETCH
        mov.w   [W14--], W0
        cp.b    W0, [W14]
        bra     nz, NEQUAL_TRUE
        dec     W3, W3
        bra     nz, NEQUAL1
        clr.w   [W14]
        return
NEQUAL_TRUE:
        setm.w  [W14]
        return

; SKIP   c-addr u c -- c-addr' u'
;                          skip matching chars
        .pword  paddr(NEQUAL_L)+PFLASH
SKIP_L:
        .byte   NFA|4
        .ascii  "skip"
        .align  2
SKIP:
        mov     [W14--], W3     ; char
        mov     [W14--], W4     ; count 
        mov     [W14--], W5     ; c-addr
        cp0     W4
        bra     z, SKIP2
SKIP0:
        mov     W5, [++W14]
        rcall   CFETCH
        mov     [w14--], w0
        cp.b    w0, #9
        bra     z, SKIP1
        cp.b    w3, w0
        bra     z, SKIP1
        bra     SKIP2
SKIP1:
        inc     W5, w5
        dec     w4, w4
        bra     nz, SKIP0
SKIP2:
        mov     w5, [++w14]
        mov     w4, [++w14]
        return

        .pword  paddr(SKIP_L)+PFLASH
SCAN_L:
        .byte   NFA|4
        .ascii  "scan"
        .align  2
SCAN:
        mov     [W14--], W3     ; c
        mov     [W14--], W4     ; count
        mov     [W14--], W5     ; c-addr
        cp0     W4
        bra     z, SCAN3
SCAN0:
        mov     W5, [++W14]
        rcall   CFETCH
        mov     [w14--], w0
        cp.b    w0, #9
        bra     z, SCAN3
SCAN2:
        cp.b    w0, W3
        bra     z, SCAN3
        inc     w5, w5
        dec     W4, w4
        bra     nz, SCAN0
SCAN3:
        mov     W5, [++W14]
        mov     W4, [++W14]
        return

        .pword  paddr(SCAN_L)+PFLASH
EMIT_L:
        .byte   NFA|4
        .ascii  "emit"
        .align  2
EMIT:
        rcall   UEMIT
        goto    FEXECUTE

        .pword  paddr(EMIT_L)+PFLASH
KEY_L:
        .byte   NFA|3
        .ascii  "key"
        .align  2
KEY:
        rcall   UKEY
        goto    FEXECUTE

        .pword  paddr(KEY_L)+PFLASH
KEYQ_L:
        .byte   NFA|4
        .ascii  "key?"
        .align  2
KEYQ:
        rcall   UKEYQ
        goto    FEXECUTE

        .pword  paddr(KEYQ_L)+PFLASH
UEMIT_L:
        .byte   NFA|5
        .ascii  "'emit"
        .align  2
UEMIT:
        rcall   DOUSER
        .word   uemit

        .pword  paddr(UEMIT_L)+PFLASH
UKEY_L:
        .byte   NFA|4
        .ascii  "'key"
        .align  2
UKEY:
        rcall   DOUSER
        .word   ukey

        .pword  paddr(UKEY_L)+PFLASH
UKEYQ_L:
        .byte   NFA|5
        .ascii  "'key?"
        .align  2
UKEYQ:
        rcall   DOUSER
        .word   ukeyq

; >CQ ( c addr -- ) Put to character queue
        .pword  paddr(UKEYQ_L)+PFLASH
CQUEUE_TO_L:
        .byte   NFA|3
        .ascii  ">cq"
        .align  2
CQUEUE_TO:
        rcall   FETCH
        mov     [W14--], W0 ; W0 is the base pointer (length)
        disi    #11
        mov     [W0+2], W1      ; W1 is the write pointer
        mov.b   [W14], [W1++]
        sub     W14, #2, W14
        add     W0, [W0], W2
        add     W2, #8, W2      ; W2 is the end of buffer pointer
        cp      W2, W1          ; end of buf - write pointer
        bra     nn, CQUEUE_TO_1
        add     W0, #8, W1
CQUEUE_TO_1:        
        mov     W1, [W0+2]
        add     W0, #6, W0
        inc     [W0], [W0]
        return

; >CQ? ( addr -- flag ) Space available ? false = queue is full
        .pword  paddr(CQUEUE_TO_L)+PFLASH
CQUEUE_TOQ_L:
        .byte   NFA|4
        .ascii  ">cq?"
        .align  2
CQUEUE_TOQ:
        rcall   FETCH
        mov     [W14], W0   ; W0 is the base pointer (length)
        disi    #2
        mov     [W0+6], W1      ; W1 = FillLevel
        sub     W1, [W0], W1    ; W1 = Fill - maxLength
        asr     W1, #15, W1     ; Extend the sign bit to a flag
        mov     W1, [W14]
        return

; CQ> ( addr -- c) Get from character queue
        .pword  paddr(CQUEUE_TOQ_L)+PFLASH
CQUEUE_FROM_L:
        .byte   NFA|3
        .ascii  "cq>"
        .align  2
CQUEUE_FROM:
        rcall   FETCH
        mov     [W14], W0       ; W0 is the base pointer (length)
        clr     [W14]           ; Zero the upper byte of the result
        add     W0, [W0], W2    ; W2 = 
        add     W2, #8, W2      ; W2 is the end of buffer pointer
        disi    #9
        mov     [W0+4], W1      ; W1 is the read pointer
        mov.b   [W1++], [W14]   ; remember the unqueued character
        cp      W2, W1          ; end of buf - read pointer
        bra     nn, CQUEUE_FROM_1
        add     W0, #8, W1      ; New read pointer
CQUEUE_FROM_1:      
        mov     W1, [W0+4]      ; Store the new read pointer
        add     W0, #6, W0
        dec     [W0], [W0]
        return

; CQ>? ( addr -- flag ) Character available ? false = no
        .pword  paddr(CQUEUE_FROM_L)+PFLASH
CQUEUE_FROMQ_L:
        .byte   NFA|4
        .ascii  "cq>?"
        .align  2
CQUEUE_FROMQ:
        rcall   FETCH
        mov     [W14], W0
        mov     [W0+6], W1      ; Atomic, disi not needed.
        mov     W1, [W14]
        return

; CQ0 ( addr -- ) Reset a character queue 
        .pword  paddr(CQUEUE_FROMQ_L)+PFLASH
CQUEUEZ_L:
        .byte   NFA|3
        .ascii  "cq0"
        .align  2
CQUEUEZ:
        rcall   FETCHPP         ; addr baseaddr
        rcall   SWOP
        rcall   FETCH           ; baseaddr size
        mov     [W14--], W2     ; size
        dec2    W2, W2          ; Leave two empty pos in the queue
        mov     [W14--], W1     ; W1 points to the base
        disi    #5
        mov     W2, [W1]        ; Store the size
        add     W1, #8, W0
        mov     W0, [++W1]      ; Write pointer
        mov     W0, [++W1]      ; Read pointer
        clr     [++W1]          ; Fill level
        return
    
; CQ: ( size "name" -- ) Create a character queue
        .pword  paddr(CQUEUEZ_L)+PFLASH
CQUEUE_L:
        .byte   NFA|3
        .ascii  "cq:"
        .align  2
CQUEUE:
        rcall   FLASH
        rcall   CREATE
        rcall   RHERE
        rcall   COMMA
        mov     [W14++], [W14]      ; dup
        dec     [W14], [W14]
        rcall   COMMA
        mlit    8
        rcall   PLUS
        rcall   RAM
        rcall   ALLOT
        rcall   XDOES
CQUEUE_DOES:
        rcall   DODOES
        return

        .pword  paddr(CQUEUE_L)+PFLASH
U1TXQUEUE_L:
        .byte   NFA|INLINE|5
        .ascii  "u1txq"
        .align  2
U1TXQUEUE:
        mlit    handle(U1TXQUEUE_DATA)+PFLASH
        return
U1TXQUEUE_DATA:
        .word   txqueue1
        .word   TX1_BUF_SIZE-1

        .pword  paddr(U1TXQUEUE_L)+PFLASH
U1RXQUEUE_L:
        .byte   NFA|INLINE|5
        .ascii  "u1rxq"
        .align  2
U1RXQUEUE:
        mlit    handle(U1RXQUEUE_DATA)+PFLASH
        return
U1RXQUEUE_DATA:
        .word   rxqueue1
        .word   RX1_BUF_SIZE-1

        .pword  paddr(U1RXQUEUE_L)+PFLASH
TX1_L:
        .byte   NFA|3
        .ascii  "tx1"
        .align  2
TX1:
.ifdef UTXISEL
        bset    iflags, #fBUSY
.endif
        rcall   PAUSE
        rcall   TX1Q
        cp0     [W14--]
        bra     z, TX1
TX1_1:
.ifdef UTXISEL
        bclr    iflags, #fBUSY
.endif
        rcall   U1TXQUEUE
        rcall   CQUEUE_TO
        bset    IFS0, #U1TXIF       ; check if UART TX has space
        return

        .pword  paddr(TX1_L)+PFLASH
TX1Q_L:
        .byte   NFA|4
        .ascii  "tx1?"
        .align  2
TX1Q:
        mlit    handle(U1TXQUEUE_DATA)+PFLASH
        goto    CQUEUE_TOQ

        .pword  paddr(TX1Q_L)+PFLASH
RX1_L:
        .byte   NFA|3
        .ascii  "rx1"
        .align  2
RX1:
        rcall   PAUSE
        rcall   RX1Q
        cp0     [W14--]
        bra     z, RX1
        mlit    handle(U1RXQUEUE_DATA)+PFLASH
        rcall   CQUEUE_FROM
        return

        .pword  paddr(RX1_L)+PFLASH
RX1Q_L:
        .byte   NFA|4
        .ascii  "rx1?"
        .align  2
RX1Q:
        mlit    handle(U1RXQUEUE_DATA)+PFLASH
        rcall   CQUEUE_FROMQ
        cp0     [W14]
        bra     nz, RX1Q1
        btsc    iflags, #fFC1
        bra     RX1Q1      
.if FC1_TYPE == 1
        btst    iflags, #ixoff1
        bra     z, RX1Q1
        bclr    iflags, #ixoff1
;        mlit    '>'
;        rcall   TX1
        mlit    XON
        rcall   TX1
.else
.if FC1_TYPE == 2
        cp0     rbuf_lv1
        bra     nz, RX1Q1
        bclr    U1RTSPORT, #U1RTSPIN
.endif
.endif
RX1Q1:
        return

        .pword  paddr(RX1Q_L)+PFLASH
.ifdecl BAUDRATE2
.ifdecl _U2RXREG
U2TXQUEUE_L:
        .byte   NFA|INLINE|5
        .ascii  "u2txq"
        .align  2
U2TXQUEUE:
        mlit    handle(U2TXQUEUE_DATA)+PFLASH
        return
U2TXQUEUE_DATA:
        .word   txqueue2
        .word   TX2_BUF_SIZE-1

        .pword  paddr(U2TXQUEUE_L)+PFLASH
U2RXQUEUE_L:
        .byte   NFA|INLINE|5
        .ascii  "u2rxq"
        .align  2
U2RXQUEUE:
        mlit    handle(U2RXQUEUE_DATA)+PFLASH
        return
U2RXQUEUE_DATA:
        .word   rxqueue2
        .word   RX2_BUF_SIZE-1

        .pword  paddr(U2RXQUEUE_L)+PFLASH
TX2_L:
        .byte   NFA|3
        .ascii  "tx2"
        .align  2
TX2:    
.ifdef UTXISEL
        bset    iflags, #fBUSY
.endif
        rcall   PAUSE
        rcall   TX2Q
        cp0     [W14--]
        bra     z, TX2
.ifdef UTXISEL
        bclr    iflags, #fBUSY
.endif
        rcall   U2TXQUEUE
        rcall   CQUEUE_TO
        bset    IFS1, #U2TXIF       ; check if UART TX has space
TX2_2:
        return

        .pword  paddr(TX2_L)+PFLASH
TX2Q_L:
        .byte   NFA|4
        .ascii  "tx2?"
        .align  2
TX2Q:
        mlit    handle(U2TXQUEUE_DATA)+PFLASH
        goto    CQUEUE_TOQ

        .pword  paddr(TX2Q_L)+PFLASH
RX2_L:
        .byte   NFA|3
        .ascii  "rx2"
        .align  2
RX2:
        rcall   PAUSE
        rcall   RX2Q
        cp0     [W14--]
        bra     z, RX2
        mlit    handle(U2RXQUEUE_DATA)+PFLASH
        rcall   CQUEUE_FROM
        return

        .pword  paddr(RX2_L)+PFLASH
RX2Q_L:
        .byte   NFA|4
        .ascii  "rx2?"
        .align  2
RX2Q:
        mlit    handle(U2RXQUEUE_DATA)+PFLASH
        rcall   CQUEUE_FROMQ
        cp0     [W14]
        bra     nz, RX2Q1
        btss    iflags, #fFC2
        bra     RX2Q1      
.if FC2_TYPE == 1
        btst    iflags, #ixoff2
        bra     z, RX2Q1
        bclr    iflags, #ixoff2
        mlit    XON
        rcall   TX2
.else
.if FC1_TYPE == 2
        cp0     rbuf_lv2
        bra     nz, RX2Q1
        bclr    U2RTSPORT, #U2RTSPIN
.endif
.endif
RX2Q1:
        return

        .pword  paddr(RX2Q_L)+PFLASH
.endif
.endif
DUP_L:
        .byte   NFA|INLINE|3
        .ascii  "dup"
        .align  2
DUP:
        mov.w   [W14++], [W14]
        return

        .pword  paddr(DUP_L)+PFLASH
EXECUTE_L:
        .byte   NFA|7
        .ascii  "execute"
        .align  2
EXECUTE:
        mov.w   [W14--], W0
        mov     #PFLASH, W1
        sub     W0, W1, W0
        goto    W0

        .pword  paddr(EXECUTE_L)+PFLASH
FEXECUTE_L:
        .byte   NFA|3
        .ascii  "@ex"
        .align  2
FEXECUTE:
        rcall   FETCH
        mov.w   [W14--], W0
        mov     #PFLASH, W1
        sub     W0, W1, W0
        goto    W0

        .pword  paddr(FEXECUTE_L)+PFLASH
EXIT_L:
        .byte   NFA|COMPILE|4
        .ascii  "exit"
        .align  2
EXIT:
        sub     #4, W15
        return

        .byte   NFA|COMPILE|3
        .ascii  "(,)"
        .align  2
DOCOMMAXT:
         pop    W1
         pop    W0
         tblrdl [W0++], [++W14]
         push   W0
         push   W1
         goto   COMMAXT

        .pword  paddr(EXIT_L)+PFLASH
DOCREATE_L:
        .byte   NFA|COMPILE|3
        .ascii  "(c)"
        .align  2
DOCREATE:
        mov.w   [W15-4], W0
.ifdef PEEPROM
        clr     TBLPAG
.endif
        tblrdl  [W0], [++W14]
        sub     #4, W15
        return
DOEMIT:
        mov.w   [W15-4], W0
.ifdef PEEPROM
        clr     TBLPAG
.endif
        tblrdl  [W0++], [++W14]
        mov.w   W0, [W15-4]
        goto    EMIT

        .pword  paddr(DOCREATE_L)+PFLASH
DODOES_L:
        .byte   NFA|COMPILE|3
        .ascii  "(d)"
        .align  2
DODOES:
        pop     W0
        pop     W0
        pop     W1
        pop     W1
.ifdef PEEPROM
        clr     TBLPAG
.endif
        tblrdl  [W1], [++W14]
        goto    W0

        .pword  paddr(DODOES_L)+PFLASH
SPSTORE_L:
        .byte   NFA|3
        .ascii  "sp!"
        .align  2
SPSTORE:
        mov.w   [W14], W14
        return

        .pword  paddr(SPSTORE_L)+PFLASH
SPFETCH_L:
        .byte   NFA|3
        .ascii  "sp@"
        .align  2
SPFETCH:
        mov     W14, [++W14]
        return

; RP points to the first empty stack cell.
        .pword  paddr(SPFETCH_L)+PFLASH
RPFETCH_L:
        .byte   NFA|3
        .ascii  "rp@"
        .align  2
RPFETCH:
        dec2    W15, W0
        dec2    W0, [++W14]
        return

        .pword  paddr(RPFETCH_L)+PFLASH
RPEMPTY_L:
        .byte   NFA|COMPILE|3
        .ascii  "rp0"
        .align  2
RPEMPTY:
        pop     W0
        pop     [++W14]
        rcall   R0
        rcall   FETCH
        mov     [W14--], W15
        mov     [W14--], W0
        goto    W0

        .pword  paddr(RPEMPTY_L)+PFLASH
DROP_L:
        .byte   NFA|INLINE|4
        .ascii  "drop"
        .align  2
DROP:
        sub     W14, #2, W14
        return

        .pword  paddr(DROP_L)+PFLASH
SWAP_L:
        .byte   NFA|INLINE|4
        .ascii  "swap"
        .align  2
SWOP:
        mov     [W14--], W0
        mov     [W14++], [W14]
        mov     W0, [W14-2]
        return

        .pword  paddr(SWAP_L)+PFLASH
OVER_L:
        .byte   NFA|INLINE|4
        .ascii  "over"
        .align  2
OVER:
        mov     [W14-0x2], W0
        mov     W0, [++W14]
        return

        .pword  paddr(OVER_L)+PFLASH
ROT_L:
        .byte   NFA|3
        .ascii  "rot"
        .align  2
ROT:
        mov     [W14-0x4], W0
        mov     [W14-0x2], W1
        mov     [W14], W2 
        mov     W1, [W14-0x4]
        mov     W2, [W14-0x2]
        mov     W0, [W14]
        return
        
        .pword  paddr(ROT_L)+PFLASH
TOR_L:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  ">r"
        .align  2
TOR:
        push    [W14--]
        return

        .pword  paddr(TOR_L)+PFLASH
        .align  2
RFROM_L:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "r>"
        .align  2
RFROM:
        pop     [++W14]
        return
        
        .pword  paddr(RFROM_L)+PFLASH
        .align  2
RFETCH_L:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "r@"
        .align  2
RFETCH:
        mov     [W15-2], W0
        mov     W0, [++W14]
        return

;   ABS     n   --- n1      absolute value of n
        .pword  paddr(RFETCH_L)+PFLASH
ABS_L:
        .byte   NFA|3
        .ascii  "abs"
        .align  2
ABS:
        mov     [W14++], [W14]      ; dup
        rcall   QNEGATE
        return

;   ABS     n   --- n1      absolute value of n
        .pword  paddr(ABS_L)+PFLASH
DABS_L:
        .byte   NFA|4
        .ascii  "dabs"
        .align  2
DABS:
        mov     [W14++], [W14]      ; dup
        goto    QDNEGATE

        .pword  paddr(DABS_L)+PFLASH
PLUS_L:
        .byte   NFA|INLINE|1
        .ascii  "+"
        .align  2
PLUS:
        mov     [W14--], W0
        add     W0, [W14], [W14]
        return

        .pword  paddr(PLUS_L)+PFLASH
MPLUS_L:
        .byte   NFA|2
        .ascii  "m+"
        .align  2
MPLUS:
        mov     [W14--], W0
        setm    W1
        btss    W0, #15
        clr     W1
        add     W0, [--W14], [W14++]
        addc    W1, [W14], [W14]
        return

        .pword  paddr(MPLUS_L)+PFLASH
DPLUS_L:
        .byte   NFA|2
        .ascii  "d+"
        .align  2
DPLUS:
        mov     [W14--], W0
        mov     [W14--], W1
        mov     [W14--], W2
        add     W1, [W14], [W14++]
        addc    W0, W2, [W14]
        return

        .pword  paddr(DPLUS_L)+PFLASH
MINUS_L:
        .byte   NFA|INLINE|1
        .ascii  "-"
        .align  2
MINUS:
        mov     [W14--], W0
        mov     [W14], W1
        sub     W1, W0, [W14]
        return

        .pword  paddr(MINUS_L)+PFLASH
DMINUS_L:
        .byte   NFA|2
        .ascii  "d-"
        .align  2
DMINUS:
        mov     [W14--], W0
        mov     [W14--], W1
        mov     [W14--], W2
        mov     [W14], W3
        sub     W3, W1, [W14++]
        subb    W2, W0, [W14]
        return

        .pword  paddr(DMINUS_L)+PFLASH
AND_L:
        .byte   NFA|INLINE|3
        .ascii  "and"
        .align  2
AND:
        mov     [W14--], W0
        and     W0, [W14], [W14]
        return

        .pword  paddr(AND_L)+PFLASH
OR_L:
        .byte   NFA|INLINE|2
        .ascii  "or"
        .align  2
OR:
        mov     [W14--], W0
        ior     W0, [W14], [W14]
        return

        .pword  paddr(OR_L)+PFLASH
XOR_L:
        .byte   NFA|INLINE|3
        .ascii  "xor"
        .align  2
XOR:
        mov     [W14--], W0
        xor     W0, [W14], [W14]
        return

        .pword  paddr(XOR_L)+PFLASH
INVERT_L:
        .byte   NFA|INLINE|6
        .ascii  "invert"
        .align  2
INVERT:
        com     [W14], [W14]
        return

        .pword  paddr(INVERT_L)+PFLASH
DINVERT_L:
        .byte   NFA|INLINE|7
        .ascii  "dinvert"
        .align  2
DINVERT:
        com     [W14], [W14--]
        com     [W14], [W14++]
        return

        .pword  paddr(DINVERT_L)+PFLASH
NEGATE_L:
        .byte   NFA|INLINE|6
        .ascii  "negate"
        .align  2
NEGATE:
        neg     [W14], [W14]
        return

        .pword  paddr(NEGATE_L)+PFLASH
DNEGATE_L:
        .byte   NFA|7
        .ascii  "dnegate"
        .align  2
DNEGATE:
        rcall   SWOP        
        com     [W14], [W14]
        rcall   SWOP
        com     [W14], [W14]
        rcall   ONE
        goto    MPLUS
        
        .pword  paddr(DNEGATE_L)+PFLASH
QDNEGATE_L:
        .byte   NFA|8
        .ascii  "?dnegate"
        .align  2
QDNEGATE:
        rcall   ZEROLESS
        cp0     [W14--]
        bra     z, QDNEGATE1
        rcall   DNEGATE
QDNEGATE1:
        return        
        
        .pword  paddr(QDNEGATE_L)+PFLASH
ONEPLUS_L:
        .byte   NFA|INLINE|2
        .ascii  "1+"
        .align  2
ONEPLUS:
        inc     [W14], [W14]
        return

        .pword  paddr(ONEPLUS_L)+PFLASH
TWOPLUS_L:
        .byte   NFA|INLINE|2
        .ascii  "2+"
        .align  2
TWOPLUS:
        inc2    [W14], [W14]
        return

        .pword  paddr(TWOPLUS_L)+PFLASH
ONEMINUS_L:
        .byte   NFA|INLINE|2
        .ascii  "1-"
        .align  2
ONEMINUS:
        dec     [W14], [W14]
        return

        .pword  paddr(ONEMINUS_L)+PFLASH
TWOMINUS_L:
        .byte   NFA|INLINE|2
        .ascii  "2-"
        .align  2
TWOMINUS:
        dec2    [W14], [W14]
        return

        .pword  paddr(TWOMINUS_L)+PFLASH
TWOSTAR_L:
        .byte   NFA|INLINE|2
        .ascii  "2*"
        .align  2
TWOSTAR:
        sl      [W14], [W14]        ; 2*
        return

        .pword  paddr(TWOSTAR_L)+PFLASH
DTWOSTAR_L:
        .byte   NFA|INLINE|3
        .ascii  "d2*"
        .align  2
DTWOSTAR:
        sl      [--W14], [W14]        ; 2* msb -> c, 0 -> lsb
        rlc     [++W14], [W14]
        return

        .pword  paddr(DTWOSTAR_L)+PFLASH
TWOSLASH_L:
        .byte   NFA|INLINE|2
        .ascii  "2/"
        .align  2
TWOSLASH:
        asr     [W14], [W14]        ; 2/
        return

        .pword  paddr(TWOSLASH_L)+PFLASH
DTWOSLASH_L:
        .byte   NFA|INLINE|3
        .ascii  "d2/"
        .align  2
DTWOSLASH:
        asr     [W14], [W14--]		; 2/ lsb -> c
        rrc     [W14], [W14++]		; 2/ c -> msb
        return

        .pword  paddr(DTWOSLASH_L)+PFLASH
ZEROEQUAL_L:
        .byte   NFA|2
        .ascii  "0="
        .align  2
ZEROEQUAL:
        cp0     [W14]
        bra     nz, test_false
test_true:
        setm    [W14]
        return

        .pword  paddr(ZEROEQUAL_L)+PFLASH
DZEROEQUAL_L:
        .byte   NFA|3
        .ascii  "d0="
        .align  2
DZEROEQUAL:
        mov     [W14--], W0
        ior     W0, [W14], [W14]
        bra     nz, test_false
        goto	test_true

        .pword  paddr(DZEROEQUAL_L)+PFLASH
ZEROLESS_L:
        .byte   NFA|2
        .ascii  "0<"
        .align  2
ZEROLESS:
        cp0     [W14]
        bra     n, test_true
test_false:
        clr     [W14]
        return

        .pword  paddr(ZEROLESS_L)+PFLASH
DZEROLESS_L:
        .byte   NFA|3
        .ascii  "d0<"
        .align  2
DZEROLESS:
        cp0     [W14--]
        bra     n, test_true
        goto	test_false

        .pword  paddr(DZEROLESS_L)+PFLASH
STORE_P_L:
        .byte   NFA|INLINE|2
        .ascii  "!p"
        .align  2
STORE_P:
        mov     [w14--], W13
        return

        .pword  paddr(STORE_P_L)+PFLASH
STORE_P_TO_R_L:
        .byte   NFA|COMPILE|INLINE|4
        .ascii  "!p>r"
        .align  2
STORE_P_TO_R:
        push    W13
        mov     [W14--], W13
        return

        .pword  paddr(STORE_P_TO_R_L)+PFLASH
R_TO_P_L:
        .byte   NFA|INLINE|COMPILE|3
        .ascii  "r>p"
        .align  2
R_TO_P:
        pop     W13
        return

        .pword  paddr(R_TO_P_L)+PFLASH
FETCH_P_L:
        .byte   NFA|INLINE|2
        .ascii  "@p"
        .align  2
FETCH_P:
        mov     W13, [++W14]
        return

        .pword  paddr(FETCH_P_L)+PFLASH
PFETCH_L:
        .byte   NFA|2
        .ascii  "p@"
        .align  2
PFETCH:
        mov     W13, [++W14]
        goto    FETCH

        .pword  paddr(PFETCH_L)+PFLASH
PCFETCH_L:
        .byte   NFA|3
        .ascii  "pc@"
        .align  2
PCFETCH:    
        mov     W13, [++W14]
        goto    CFETCH      

        .pword  paddr(PCFETCH_L)+PFLASH
PSTORE_L:
        .byte   NFA|2
        .ascii  "p!"
        .align  2
PSTORE:
        mov     W13, [++W14]
        goto    STORE

        .pword  paddr(PSTORE_L)+PFLASH
PCSTORE_L:
        .byte   NFA|3
        .ascii  "pc!"
        .align  2
PCSTORE:
        mov     W13, [++W14]
        goto    CSTORE

        .pword  paddr(PCSTORE_L)+PFLASH
PPLUS_L:
        .byte   NFA|INLINE|2
        .ascii  "p+"
        .align  2
PPLUS:
        inc     W13, W13
        return

        .pword  paddr(PPLUS_L)+PFLASH
PPLUS2_L:
        .byte   NFA|INLINE|3
        .ascii  "p2+"
        .align  2
PPLUS2:
        inc2    W13, W13
        return

        .pword  paddr(PPLUS2_L)+PFLASH
PNPLUS_L:
        .byte   NFA|INLINE|3
        .ascii  "p++"
        .align  2
PNPLUS:
        mov     [w14--], W0
        add     W0, W13, W13
        return

        .pword  paddr(PNPLUS_L)+PFLASH
STOD_L:
        .byte   NFA|3
        .ascii  "s>d"
        .align  2
STOD:
        mov     [W14++], [W14]
        goto    ZEROLESS

        .pword  paddr(STOD_L)+PFLASH
UMSTAR_L:
        .byte   NFA|3
        .ascii  "um*"
        .align  2
UMSTAR:
        mov     [W14--], W0
        mul.uu  W0, [W14], W2
        mov     W2, [W14++]
        mov     W3, [W14]
        return

        .pword  paddr(UMSTAR_L)+PFLASH
MSTAR_L:
        .byte   NFA|2
        .ascii  "m*"
        .align  2
MSTAR:
        mov     [W14--], W0
        mul.ss  W0, [W14], W2
        mov     W2, [W14++]
        mov     W3, [W14]
        return

        .pword  paddr(MSTAR_L)+PFLASH
UMSLASHMOD_L:
        .byte   NFA|6
        .ascii  "um/mod"
        .align  2
UMSLASHMOD:
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14--], W0
        repeat  #17
        div.ud  W0, W2
        mov     W1, [++W14]
        mov     W0, [++W14]
        return

        .pword  paddr(UMSLASHMOD_L)+PFLASH
SMSLASHREM_L:
        .byte   NFA|6
        .ascii  "sm/rem"
        .align  2
SMSLASHREM:
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14--], W0
        repeat  #17
        div.sd  W0, W2
        mov     W1, [++W14]
        mov     W0, [++W14]
        return

        .pword  paddr(SMSLASHREM_L)+PFLASH
USLASHMOD_L:
        .byte   NFA|5
        .ascii  "u/mod"
        .align  2
USLASHMOD:
        mov     [W14--], W2
        mov     [W14--], W0
        repeat  #17
        div.u   W0, W2
        mov     W1, [++W14]
        mov     W0, [++W14]
        return

        .pword  paddr(USLASHMOD_L)+PFLASH
SLASHMOD_L:
        .byte   NFA|4
        .ascii  "/mod"
        .align  2
SLASHMOD:
        mov     [W14--], W2
        mov     [W14--], W0
        repeat  #17
        div.s   W0, W2
        mov     W1, [++W14]
        mov     W0, [++W14]
        return

        .pword  paddr(SLASHMOD_L)+PFLASH
MOD_L:
        .byte   NFA|3
        .ascii  "mod"
        .align  2
MOD:
        mov     [W14--], W2
        mov     [W14--], W0
        repeat  #17
        div.s   W0, W2
        mov     W1, [++W14]
        return

; user ( n "name" -- )
        .pword  paddr(MOD_L)+PFLASH
USER_L:
        .byte   NFA|4
        .ascii  "user"
        .align  2
USER:
        rcall   CONSTANT
        rcall   XDOES
DOUSER:
        pop     W0
        pop     W0
.ifdef PEEPROM
        clr     TBLPAG
.endif
        tblrdl  [W0], W0
        add     upcurr, WREG
        mov     W0, [++W14]
        return

; value
        .pword  paddr(USER_L)+PFLASH
VALUE_L:
        .byte   NFA|5
        .ascii  "value"
        .align  2
VALUE:
        rcall   CREATE
        rcall   COMMA
        rcall   XDOES
VALUE_DOES:
        rcall   DODOES
        goto    FETCH

; defer ( "name" -- )
        .pword  paddr(VALUE_L)+PFLASH
DEFER_L:
        .byte   NFA|5
        .ascii  "defer"
        .align  2
DEFER:
        rcall   CREATE
        mlit    handle(ABORT)+PFLASH
        rcall   COMMA
        rcall   XDOES
DEFER_DOES:
        rcall   DODOES
        goto    FEXECUTE

        .pword  paddr(DEFER_L)+PFLASH
IS_L:
        .byte   NFA|IMMED|2
        .ascii  "is"
        .align  2
IS:
        rcall   TICK
        inc2    [W14], [W14]
        rcall   FETCH
        rcall   STATE
        cp0     [W14--]
        bra     z, IS1
        rcall   LITERAL
        rcall   DOCOMMAXT
        .word   handle(STORE)+PFLASH
        bra     IS2
IS1:
        rcall   STORE
IS2:
        return

        .pword  paddr(IS_L)+PFLASH
TO_L:
        .byte   NFA|IMMED|2
        .ascii  "to"
        .align  2
TO:
        goto    IS


; >body xt -- a-addr transform a created words XT to it's data field address
        .pword  paddr(TO_L)+PFLASH
TOBODY_L:
        .byte   NFA|INLINE|5
        .ascii  ">body"
        .align  2
TOBODY:
        inc2    [W14], [W14]
        return

        .pword  paddr(TOBODY_L)+PFLASH
VARIABLE_L:
        .byte   NFA|8
        .ascii  "variable"
        .align  2
VARIABLE_:
        rcall   HERE
        rcall   CELL
        rcall   ALLOT
        goto    CON_

        .pword  paddr(VARIABLE_L)+PFLASH
TWOVARIABLE_L:
        .byte   NFA|9
        .ascii  "2variable"
        .align  2
TWOVARIABLE:
        rcall   HERE
        mlit	#4
        rcall   ALLOT
        goto    CON_

        .pword  paddr(TWOVARIABLE_L)+PFLASH
CONSTANT_L:
        .byte   NFA|3
        .ascii  "co:"
        .align  2
CONSTANT:
        rcall   CREATE
        rcall   CELL
        neg     [W14], [W14]
        rcall   IALLOT
        goto    ICOMMA

        .pword  paddr(CONSTANT_L)+PFLASH
CON_L:
        .byte   NFA|8
        .ascii  "constant"
        .align  2
CON_:
        rcall   COLON
        rcall   LITERAL
        goto    SEMICOLON

        .pword  paddr(CON_L)+PFLASH
TWOCON_L:
        .byte   NFA|9
        .ascii  "2constant"
        .align  2
TWOCON_:
        rcall   SWOP
        rcall   COLON
        rcall   LITERAL
        rcall   LITERAL
        goto    SEMICOLON

        .pword  paddr(TWOCON_L)+PFLASH
PFLASH_L:
        .byte   NFA|INLINE|3
        .ascii  "pfl"
        .align  2
PFLASH_:
        mlit    PFLASH
        return

        .pword  paddr(PFLASH_L)+PFLASH
TO_XA_L:
        .byte   NFA|INLINE|3
        .ascii  ">xa"
        .align  2
TO_XA:
        mov     #PFLASH, W0
        mov     [W14], W1
        sub     W1, W0, [W14]
        return

        .pword  paddr(TO_XA_L)+PFLASH
XA_FROM_L:
        .byte   NFA|INLINE|3
        .ascii  "xa>"
        .align  2
XA_FROM:
        mov     #PFLASH, W0
        add     W0, [W14],[W14]
        return

        .pword  paddr(XA_FROM_L)+PFLASH
FLASH_L:
        .byte   NFA|5
        .ascii  "flash"
        .align  2
FLASH:
        mov     #4, W0
        mov     W0, cse
        return

        .pword  paddr(FLASH_L)+PFLASH
.ifdef PEEPROM
EEPROM_L:
        .byte   NFA|6
        .ascii  "eeprom"
        .align  2
EEPROM:
        mov     #2, W0
        mov     W0, cse
        return

        .pword  paddr(EEPROM_L)+PFLASH
.endif
RAM_L:
        .byte   NFA|3
        .ascii  "ram"
        .align  2
RAM:
        clr     cse
        return

;        .pword  paddr(RAM_L)+PFLASH
;CSE_L:
        .byte   NFA|3
        .ascii  "cse"
        .align  2
CSE:
        mov     cse, W0
        mov     W0, [++W14]
        return


; IDP    -- a-addr  Dictonary pointer storage        
;        .pword  paddr(CSE_L)+PFLASH
; IDP_L:
        .byte   NFA|3
        .ascii  "idp"
        .align  2
IDP:
        rcall   DOCREATE
        .word   dpFLASH

        .pword  paddr(RAM_L)+PFLASH
DP_L:
        .byte   NFA|2
        .ascii  "dp"
        .align  2
DP:
        mlit    dpRAM
        rcall   CSE
        goto    PLUS

        .pword  paddr(DP_L)+PFLASH
HERE_L:
        .byte   NFA|4
        .ascii  "here"
        .align  2
HERE:
        rcall   DP
        goto    FETCH

        .pword  paddr(HERE_L)+PFLASH
COMMA_L:
        .byte   NFA|1
        .ascii  ","
        .align  2
COMMA:
        rcall   HERE
        rcall   STORE
        rcall   CELL
        goto    ALLOT

        .pword  paddr(COMMA_L)+PFLASH
CCOMMA_L:
        .byte   NFA|2
        .ascii  "c,"
        .align  2
CCOMMA:
        rcall   HERE
        rcall   CSTORE
        rcall   ONE
        goto    ALLOT

        .pword  paddr(CCOMMA_L)+PFLASH
CELL_L:
        .byte   NFA|INLINE|4
        .ascii  "cell"
        .align  2
CELL:
        mov     #2, W0
        mov     W0, [++W14]
        return

        .pword  paddr(CELL_L)+PFLASH
ALIGN_L:
        .byte   NFA|5
        .ascii  "align"
        .align  2
ALIGN:
        rcall   HERE
        rcall   ALIGNED
        rcall   DP
        goto    STORE

        .pword  paddr(ALIGN_L)+PFLASH
ALIGNED_L:
        .byte   NFA|7
        .ascii  "aligned"
        .align  2
ALIGNED:
        inc     [W14], [W14]
        mlit    #0xfffe
        goto    AND

        .pword  paddr(ALIGNED_L)+PFLASH
CELLPLUS_L:
        .byte   NFA|INLINE|5
        .ascii  "cell+"
        .align  2
CELLPLUS:
        inc2    [W14], [W14]
        return

        .pword  paddr(CELLPLUS_L)+PFLASH
CELLS_L:
        .byte   NFA|INLINE|5
        .ascii  "cells"
        .align  2
CELLS:
        sl      [W14], [W14]        ; 2*
        return

        .pword  paddr(CELLS_L)+PFLASH
CHARPLUS_L:
        .byte   NFA|INLINE|5
        .ascii  "char+"
        .align  2
CHARPLUS:
        inc     [W14], [W14]        ; char+
        return

        .pword  paddr(CHARPLUS_L)+PFLASH
CHARS_L:
        .byte   NFA|INLINE|5
        .ascii  "chars"
        .align  2
CHARS:
        return

STORCOLON:
        mlit    #0xfffc; -4
        goto    IALLOT

        .pword  paddr(CHARS_L)+PFLASH
TWOFETCH_L:
        .byte   NFA|2
        .ascii  "2@"
        .align  2
TWOFETCH:
        mov     [W14++], [W14]      ; dup
        rcall   FETCH
        rcall   SWOP
        inc2    [W14], [W14]
        goto    FETCH

        .pword  paddr(TWOFETCH_L)+PFLASH
TWOSTORE_L:
        .byte   NFA|2
        .ascii  "2!"
        .align  2
TWOSTORE:
        rcall   SWOP
        rcall   OVER
        inc2    [W14], [W14]
        rcall   STORE
        goto    STORE

        .pword  paddr(TWOSTORE_L)+PFLASH
TWODROP_L:
        .byte   NFA|INLINE|5
        .ascii  "2drop"
        .align  2
TWODROP:
        sub     W14, #4, W14        ; 2drop
        return

        .pword  paddr(TWODROP_L)+PFLASH
TWODUP_L:
        .byte   NFA|4
        .ascii  "2dup"
        .align  2
TWODUP:
        rcall   OVER
        goto    OVER

        .pword  paddr(TWODUP_L)+PFLASH
TWOSWAP_L:
        .byte   NFA|5
        .ascii  "2swap"
        .align  2
TWOSWAP:
        rcall   ROT
        push    [W14--]
        rcall   ROT
        pop     [++W14]
        return

        .pword  paddr(TWODUP_L)+PFLASH
CR_L:
        .byte   NFA|2
        .ascii  "cr"
        .align  2
CR:
        mlit    #0x0d       ; CR \r
        rcall   EMIT
        mlit    #0x0a       ; LF \n
        goto    EMIT

        .pword  paddr(CR_L)+PFLASH
SPACE_L:
        .byte   NFA|5
        .ascii  "space"
        .align  2
SPACE_:  
        rcall   BL
        goto    EMIT

        .pword  paddr(SPACE_L)+PFLASH
SPACES_L:
        .byte   NFA|6
        .ascii  "spaces"
        .align  2
SPACES:
SPCS1:
        cp0     [W14]
        bra     z, SPCS2
        rcall   SPACE_
        dec     [W14], [W14]
        bra     SPCS1
SPCS2:  
        sub     W14, #2, W14
        return

        .pword  paddr(SPACES_L)+PFLASH
UMIN_L:
        .byte   NFA|4
        .ascii  "umin"
        .align  2
UMIN:
        rcall   TWODUP
        rcall   UGREATER
        cp0     [W14--]
        bra     z, UMIN1
        rcall   SWOP
UMIN1:
        sub     W14, #2, W14
        return

        .pword  paddr(UMIN_L)+PFLASH
UMAX_L:
        .byte   NFA|4
        .ascii  "umax"
        .align  2
UMAX:
        rcall   TWODUP
        rcall   ULESS
        cp0     [W14--]
        bra     z, UMAX1
        rcall   SWOP
UMAX1:
        sub     W14, #2, W14
        return

        .pword  paddr(UMAX_L)+PFLASH
ZERO_L:
        .byte   NFA|INLINE|1
        .ascii  "0"
        .align  2
ZERO:
        clr     [++W14]
        return

        .pword  paddr(ZERO_L)+PFLASH
ONE_L:
        .byte   NFA|INLINE|1
        .ascii  "1"
        .align  2
ONE:
        mlit    #1
        return

        .pword  paddr(ONE_L)+PFLASH
ACCEPT_L:
        .byte   NFA|6
        .ascii  "accept"
        .align  2
ACCEPT:
        rcall   OVER
        rcall   PLUS
        rcall   OVER
ACC1:
        rcall   KEY

        mov     [W14], W0

        cp      W0, #0x0d   ; CR
        bra     nz, ACC_LF
        sub     W14, #2, W14
        
        rcall   TRUE_
        rcall   FCR
        rcall   CSTORE
        bra     ACC6
ACC_LF:
        cp      W0, #0x0a   ; LF
        bra     nz, ACC2
        sub     W14, #2, W14

        rcall   FCR
        rcall   CFETCH
        rcall   ZEROSENSE
        bra     z, ACC6
        rcall   FALSE_
        rcall   FCR
        rcall   CSTORE
        bra     ACC1
ACC2:
        mov     [W14++], [W14]      ; dup
        rcall   EMIT

        rcall   FALSE_
        rcall   FCR
        rcall   CSTORE
        mov     [W14++], [W14]      ; dup
        mlit    #0x08              ; BS
        rcall   EQUAL
        cp0     [W14--]
        bra     z, ACC3
        sub     W14, #2, W14
        dec     [W14], [W14]
        push    [W14--]
        rcall   OVER
        pop     [++W14]
        rcall   UMAX
        bra     ACC1
ACC3:
        rcall   OVER
        rcall   CSTORE
        inc     [W14], [W14]
        rcall   OVER
        rcall   UMIN
        rcall   TWODUP
        rcall   NOTEQUAL
        cp0     [W14--]
        bra     nz, ACC1
ACC6:
        mov     [W14--], [W14]      ; nip
        rcall   SWOP
        goto    MINUS

FCR_L:
        .byte   NFA|3
        .ascii  "fcr"
        .align  2
FCR:
        rcall   DOUSER
        .word   uflg

;  .id ( nfa -- ) 
        .pword  paddr(ACCEPT_L)+PFLASH
DOTID_L:
        .byte   NFA|3
        .ascii  ".id"
        .align  2
DOTID:
        rcall   CFETCHPP
        mlit    NFL              ; nfu
        rcall   AND
        push    [W14--]
        bra     DOTID3
DOTID1:
        rcall   CFETCHPP
        rcall   TO_PR
        rcall   EMIT
DOTID3:
        dec     [--W15], [W15++] ; XNEXT
        bra     c, DOTID1        ; XNEXT
        pop     W0               ; UNNEXT
        sub     W14, #2, W14     ; DROP
        return
        

; >pr   c -- c      Filter a character to printable 7-bit ASCII
        .pword  paddr(DOTID_L)+PFLASH
TO_PR_L:
        .byte   NFA|3
        .ascii  ">pr"
        .align  2
TO_PR:
        mov     [W14++], [W14]
        rcall   BL
        mlit    0x7f
        rcall   WITHIN
        cp0     [W14--]
        bra     nz, TO_PR1
        mov    #'.', W0
        mov     W0, [W14]
TO_PR1:
        return

        .pword  paddr(TO_PR_L)+PFLASH
TYPE_L:
        .byte   NFA|4
        .ascii  "type"
        .align  2
TYPE:
        push    [W14--]         ; XFOR TOR
        bra     TYPE2           ; XFOR
TYPE1:  
        rcall   CFETCHPP
        rcall   EMIT
TYPE2:
        dec     [--W15], [W15++] ; XNEXT
        bra     c, TYPE1        ; XNEXT
        pop     W0              ; UNNEXT
        sub     W14, #2, W14    ; DROP
        return

        .pword  paddr(TYPE_L)+PFLASH
XSQUOTE_L:
        .byte   NFA|4
        .ascii  "(s" 
        .byte 0x22 
        .ascii ")"
        .align  2
XSQUOTE:
        pop     W0
        pop     [++W14]
        mov     #PFLASH, W0
        add     W0, [W14],[W14]
        rcall   CFETCHPP
        rcall   TWODUP
        rcall   PLUS
        rcall   ALIGNED
        mov     [W14--], W0
        mov     #PFLASH, W1
        sub     W0, W1, W0
        goto    W0

; S"      --            compile in-line string to flash
        .pword  paddr(XSQUOTE_L)+PFLASH
SQUOTE_L:
        .byte   NFA|IMMED|COMPILE|2
        .ascii  "s" 
        .byte 0x22
        .align  2
SQUOTE:
        rcall   DOCOMMAXT
        .word   handle(XSQUOTE)+PFLASH
        rcall   FLASH
        rcall   CQUOTE
        goto    RAM

; ,"      --           store a string to current data space
        .pword paddr(SQUOTE_L)+PFLASH
CQUOTE_L:
        .byte   NFA|2
        .ascii  "," 
        .byte 0x22
        .align  2
CQUOTE:
        mlit    0x22
        rcall   PARSE
        rcall   HERE
        rcall   OVER
        rcall   ONEPLUS
        rcall   ALIGNED
        rcall   ALLOT
        goto    PLACE


; ."       --            compile string to print into flash
        .pword  paddr(CQUOTE_L)+PFLASH
DOTQUOTE_L:
        .byte   NFA|IMMED|COMPILE|2
        .ascii  "." 
        .byte 0x22
        .align  2
DOTQUOTE: 
        rcall   SQUOTE
        rcall   DOCOMMAXT
        .word   handle(TYPE)+PFLASH
        return

;   +!      n/u addr --     add cell to data memory
        .pword  paddr(DOTQUOTE_L)+PFLASH
PLUSSTORE_L:
        .byte   NFA|2
        .ascii  "+!" 
        .align  2
PLUSSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   FETCH
        rcall   PLUS
        rcall   SWOP
        goto    STORE

;   WITHIN      ( u ul uh -- t )
;               Return true if u is within the range of ul and uh. ( ul <= u < uh )
        .pword  paddr(PLUSSTORE_L)+PFLASH
WITHIN_L:
        .byte   NFA|6
        .ascii  "within" 
        .align  2
WITHIN:
        rcall   OVER
        rcall   MINUS
        push    [W14--]
        rcall   MINUS
        pop     [++W14]
        goto    ULESS

;   <>      x1 x2 -- flag       return true if not equal 
        .pword  paddr(WITHIN_L)+PFLASH
NOTEQUAL_L:
        .byte   NFA|2
        .ascii  "<>" 
        .align  2
NOTEQUAL:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     NZ, test_true
        bra     test_false

;   =       x1 x2 -- flag       return true if x1 = x2
        .pword  paddr(NOTEQUAL_L)+PFLASH
EQUAL_L:
        .byte   NFA|1
        .ascii  "=" 
        .align  2
EQUAL:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     Z, test_true
        bra     test_false

;   =       d1 d2 -- flag       return true if d1 = d2
        .pword  paddr(EQUAL_L)+PFLASH
DEQUAL_L:
        .byte   NFA|2
        .ascii  "d=" 
        .align  2
DEQUAL:
        rcall   DMINUS
        goto    DZEROEQUAL

;   <       n1 n2 -- flag       return true if n1 < n2
        .pword  paddr(DEQUAL_L)+PFLASH
LESS_L:
        .byte   NFA|1
        .ascii  "<" 
        .align  2
LESS:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     GT, test_true
        bra     test_false

;   <       d1 d2 -- flag       return true if d1 < d2
        .pword  paddr(LESS_L)+PFLASH
DLESS_L:
        .byte   NFA|2
        .ascii  "d<" 
        .align  2
DLESS:
        rcall   DMINUS
        goto    DZEROLESS

;   >       n1 n2 -- flag      return true if n1 > n2
        .pword  paddr(DLESS_L)+PFLASH
GREATER_L:
        .byte   NFA|1
        .ascii  ">" 
        .align  2
GREATER:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     LT, test_true
        bra     test_false

;   >       d1 d2 -- flag      return true if d1 > d2
        .pword  paddr(GREATER_L)+PFLASH
DGREATER_L:
        .byte   NFA|2
        .ascii  "d>" 
        .align  2
DGREATER:
        rcall   TWOSWAP
        goto    DLESS

;   U<      u1 u2 -- flag       test unsigned less
        .pword  paddr(DGREATER_L)+PFLASH
ULESS_L:
        .byte   NFA|2
        .ascii  "u<" 
        .align  2
ULESS:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     GTU, test_true
        bra     test_false

;   U>      u1 u2 -- flag       test unsigned greater than
        .pword  paddr(ULESS_L)+PFLASH
UGREATER_L:
        .byte   NFA|2
        .ascii  "u>" 
        .align  2
UGREATER:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     LTU, test_true
        bra     test_false

; *      n1|u1 n2|u2 -- n3|u3      16*16->16 multiply
; : *  um* drop ;
        .pword  paddr(UGREATER_L)+PFLASH
STAR_L:
        .byte   NFA|1
        .ascii  "*" 
        .align  2
STAR: 
        rcall   UMSTAR
        sub     W14, #2, W14
        return

; U/      u1 u2 -- u3      16/16-> divide
        .pword  paddr(STAR_L)+PFLASH
USLASH_L:
        .byte   NFA|2
        .ascii  "u/" 
        .align  2
USLASH:
        rcall   USLASHMOD
        mov     [W14--], [W14]  ; NIP
        return

; U*/MOD  u1 u2 u3 -- u4 u5    u1*u2/u3, rem&quot
        .pword  paddr(USLASH_L)+PFLASH
USSMOD_L:
        .byte   NFA|6
        .ascii  "u*/mod" 
        .align  2
USSMOD:
        push    [W14--]
        rcall   UMSTAR
        pop     [++W14]
        goto    UMSLASHMOD

; */MOD  n1 n2 n3 -- n4 n5    n1*n2/n3, rem&quot
        .pword  paddr(USSMOD_L)+PFLASH
SSMOD_L:
        .byte   NFA|5
        .ascii  "*/mod" 
        .align  2
SSMOD:
        push    [W14--]
        rcall   MSTAR
        pop     [++W14]
        goto    SMSLASHREM

; */  n1 n2 n3 -- n4    n1*n2/n3, quot
        .pword  paddr(SSMOD_L)+PFLASH
SS_L:
        .byte   NFA|2
        .ascii  "*/" 
        .align  2
SS:
        push    [W14--]
        rcall   MSTAR
        pop     [++W14]
        rcall   SMSLASHREM
        mov     [W14--], [W14]   ; NIP
        return

; / n1 n2 -- n3  signed 16/16->16 divide
        .pword  paddr(SS_L)+PFLASH
SLASH_L:
        .byte   NFA|1
        .ascii  "/" 
        .align  2
SLASH: 
        mov     [W14--], W2
        mov     [W14--], W0
        repeat  #17
        div.s   W0, W2
        mov     W0, [++W14]
        return

; UD*  ud u -- ud
        .pword  paddr(SLASH_L)+PFLASH
L_UDSTAR:
        .byte   NFA|3
        .ascii  "ud*"
        .align  2
UDSTAR:
        rcall   DUP
        push    [W14--]
        rcall   UMSTAR
        rcall   DROP
        rcall   SWOP
        pop     [++W14]
        rcall   UMSTAR
        rcall   ROT
        goto    PLUS
        
; UD/MOD  ud u --u(rem) ud(quot)
        .pword  paddr(L_UDSTAR)+PFLASH
L_UDSLASHMOD:
        .byte   NFA|6
        .ascii  "ud/mod"
        .align  2
UDSLASHMOD:
        push    [W14--]
        rcall   FALSE_
        mov     [W15-2], W0
        mov     W0, [++W14]
        rcall   UMSLASHMOD
        rcall   ROT
        rcall   ROT
        pop     [++W14]
        rcall   UMSLASHMOD
        goto    ROT
        

;   NIP x1 x2 -- x2         NIP
        .pword  paddr(L_UDSLASHMOD)+PFLASH
NIP_L:
        .byte   NFA|INLINE|3
        .ascii  "nip" 
        .align  2
NIP:
        mov     [W14--], [W14]
        return

;   TUCK    x1 x2 -- x2 x1 x2
        .pword  paddr(NIP_L)+PFLASH
TUCK_L:
        .byte   NFA|4
        .ascii  "tuck" 
        .align  2
TUCK:
        rcall   SWOP
        goto    OVER

; ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;
        .pword  paddr(TUCK_L)+PFLASH
QNEGATE_L:
        .byte   NFA|7
        .ascii  "?negate" 
        .align  2
QNEGATE:
        rcall   ZEROLESS
        cp0     [W14--]
        bra     z, QNEGATE1
        neg     [W14], [W14]
QNEGATE1:
        return

; MAX    n1 n2 -- n3              signed maximum
;   2DUP < IF SWAP THEN DROP ;
        .pword  paddr(QNEGATE_L)+PFLASH
MAX_L:
        .byte   NFA|3
        .ascii  "max" 
        .align  2
MAX:    
        rcall   TWODUP
        rcall   LESS
        cp0     [W14--]
        bra     z, max1
        rcall   SWOP
max1:   sub     W14, #2, W14
        return

; MIN    n1 n2 -- n3              signed minimum
;   2DUP > IF SWAP THEN DROP ;
        .pword  paddr(MAX_L)+PFLASH
MIN_L:
        .byte   NFA|3
        .ascii  "min" 
        .align  2
MIN:    
        rcall   TWODUP
        rcall   GREATER
        cp0     [W14--]
        bra     z, min1
        rcall   SWOP
min1:   sub     W14, #2, W14
        return

; UP    -- a-addr       Current User area
        .pword  paddr(MIN_L)+PFLASH
UPTR_L:
        .byte   NFA|2
        .ascii  "up" 
        .align  2
UPTR:
        rcall   DOCREATE
        .word   upcurr

; HOLD  char --        add char to output string
;   -1 HP +!  HP @ C! ;
        .pword  paddr(UPTR_L)+PFLASH
HOLD_L:
        .byte   NFA|4
        .ascii  "hold" 
        .align  2
HOLD:
        rcall   TRUE_
        rcall   HP
        rcall   PLUSSTORE
        rcall   HP
        rcall   FETCH
        goto    CSTORE

; <#    --              begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
        .pword  paddr(HOLD_L)+PFLASH
LESSNUM_L:
        .byte   NFA|2
        .ascii  "<#" 
        .align  2
LESSNUM: 
        rcall   PAD
        rcall   HP
        goto    STORE

; >digit   n -- c            convert to 0..9a..z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
        .pword  paddr(LESSNUM_L)+PFLASH
TODIGIT_L:
        .byte   NFA|6
        .ascii  ">digit" 
        .align  2
TODIGIT: 
        mov     [W14++], [W14]      ; dup
        mlit    #9
        rcall   GREATER
        mlit    #0x27
        rcall   AND
        rcall   PLUS
        mlit    #0x30
        goto    PLUS

; #     ud1 -- ud2     convert 1 digit of output
;   BASE @ Ud/MOD rot >digit HOLD ;
        .pword  paddr(TODIGIT_L)+PFLASH
NUM_L:
        .byte   NFA|1
        .ascii  "#"
        .align  2
NUM:
        rcall   BASE
        rcall   FETCH
        rcall   UDSLASHMOD
        rcall   ROT
        rcall   TODIGIT
        goto    HOLD

; #S    u1 -- u2      convert remaining digits
;   begin # dup 0= until ;
        .pword  paddr(NUM_L)+PFLASH
NUMS_L:
        .byte   NFA|2
        .ascii  "#s"
        .align  2
NUMS:
        rcall   NUM
        rcall   TWODUP
        rcall   OR
        cp0     [W14--]
        bra     nz, NUMS
        return

; #>    u1 -- c-addr u    end conv., get string
;   DROP HP @ HA OVER - ;
        .pword  paddr(NUMS_L)+PFLASH
NUMGREATER_L:
        .byte   NFA|2
        .ascii  "#>"
        .align  2
NUMGREATER:
        sub     W14, #4, W14
        rcall   HP
        rcall   FETCH
        rcall   PAD
        rcall   OVER
        goto    MINUS

; SIGN  n --               add minus sign if n<0
;   0< IF 2D HOLD THEN ; 
        .pword  paddr(NUMGREATER_L)+PFLASH
SIGN_L:
        .byte   NFA|4
        .ascii  "sign"
        .align  2
SIGN:   
        rcall   ZEROLESS
        cp0     [W14--]
        bra     z, SIGN1
        mlit    #0x2D
        rcall   HOLD
SIGN1:
        return

; U.    u --                  display u unsigned
;   <# #S #> TYPE SPACE ;
        .pword  paddr(SIGN_L)+PFLASH
UDOT_L:
        .byte   NFA|2
        .ascii  "u."
        .align  2
UDOT:
        rcall   LESSNUM
        rcall   FALSE_
        rcall   NUMS
        rcall   NUMGREATER
        rcall   TYPE
        goto    SPACE_

; U.R    u +n --      display u unsigned in field of n.
;     <# 1- for # next #s #> type space ;
        .pword  paddr(UDOT_L)+PFLASH
UDOTR_L:
        .byte   NFA|3
        .ascii  "u.r"
        .align  2
UDOTR:
        rcall   LESSNUM
        dec     [W14], [W14]
        push    [W14--]
        rcall   FALSE_
        bra     UDOTR2
UDOTR1: 
        rcall   NUM
UDOTR2: 
        dec     [--W15], [W15++] ; XNEXT
        bra     c, UDOTR1
        pop     W0                ; UNNEXT
        rcall   NUMS
        rcall   NUMGREATER
        rcall   TYPE
        goto    SPACE_

; .     n --                    display n signed
        .pword  paddr(UDOTR_L)+PFLASH
DOT_L:
        .byte   NFA|1
        .ascii  "."
        .align  2
DOT:
        rcall   LESSNUM
        mov     [W14++], [W14]      ; dup
        rcall   ABS
        rcall   FALSE_
        rcall   NUMS
        rcall   ROT
        rcall   SIGN
        rcall   NUMGREATER
        rcall   TYPE
        goto    SPACE_

; UD.    ud --                  display ud unsigned
        .pword  paddr(DOT_L)+PFLASH
UDDOT_L:
        .byte   NFA|3
        .ascii  "ud."
        .align  2
UDDOT:
        rcall   LESSNUM
        rcall   NUMS
        rcall   NUMGREATER
        rcall   TYPE
        goto    SPACE_

; .     d --                    display d signed
;   <# dup >r dabs #s r> sign #> type space ;
        .pword  paddr(UDDOT_L)+PFLASH
DDOT_L:
        .byte   NFA|2
        .ascii  "d."
        .align  2
DDOT:
        rcall   LESSNUM
        push    [W14]
        rcall   DABS
        rcall   NUMS
        pop     [++W14]
        rcall   SIGN
        rcall   NUMGREATER
        rcall   TYPE
        goto    SPACE_

        .pword  paddr(DDOT_L)+PFLASH
MEMHI_L:
        .byte   NFA|2
        .ascii  "hi"
        .align  2
MEMHI:
        mlit    handle(FLASHHI)+PFLASH
        call    CSE
        call    PLUS
        goto    FETCH
FLASHHI:
        .word   PFLASH-1
        .word   0xffff
        .word   FLASH_HI

; DECIMAL  --         set number base to decimal
;   #10 BASE ! ;
        .pword  paddr(MEMHI_L)+PFLASH
DECIMAL_L:
        .byte   NFA|7
        .ascii  "decimal"
        .align  2
DECIMAL: 
        mlit    #10
        rcall   BASE
        goto    STORE

; HEX     --              set number base to hex
        .pword  paddr(DECIMAL_L)+PFLASH
HEX_L:
        .byte   NFA|3
        .ascii  "hex"
        .align  2
HEX:
        mlit    #0x10
        rcall   BASE
        goto    STORE

; BIN     --              set number base to binary
        .pword  paddr(HEX_L)+PFLASH
BIN_L:
        .byte   NFA|3
        .ascii  "bin"
        .align  2
BIN:
        rcall   CELL
        rcall   BASE
        goto    STORE

; ULINK   -- a-addr     link to next task
        .pword  paddr(BIN_L)+PFLASH
ULINK_L:
        .byte   NFA|5
        .ascii  "ulink"
        .align  2
ULINK:
        rcall   DOUSER
        .word   ulink

; RSAVE   -- a-addr     Return stack save area
        .pword  paddr(ULINK_L)+PFLASH
RSAVE_L:
        .byte   NFA|5
        .ascii  "rsave"
        .align  2
RSAVE:
        rcall   DOUSER
        .word   ursave

; SSAVE   -- a-addr     Saved parameter stack pointer
        .pword  paddr(RSAVE_L)+PFLASH
SSAVE_L:
        .byte   NFA|5
        .ascii  "ssave"
        .align  2
SSAVE:
        rcall   DOUSER
        .word   ussave

; TASK   -- a-addr     Pointer to task area
        .pword  paddr(SSAVE_L)+PFLASH
TASK_L:
        .byte   NFA|4
        .ascii  "task"
        .align  2
TASK:
        rcall   DOUSER
        .word   utask

; HP       -- a-addr                HOLD pointer
        .pword  paddr(TASK_L)+PFLASH
HP_L:
        .byte   NFA|2
        .ascii  "hp"
        .align  2
HP:
        rcall   DOUSER
        .word   uhp

; PAD     -- a-addr        User PAD buffer
        .pword  paddr(HP_L)+PFLASH
PAD_L:
        .byte   NFA|3
        .ascii  "pad"
        .align  2
PAD:
        rcall   TIB
        rcall   TIBSIZE
        goto    PLUS

; BASE    -- a-addr       holds conversion radix
        .pword  paddr(PAD_L)+PFLASH
BASE_L:
        .byte   NFA|4
        .ascii  "base"
        .align  2
BASE:
        rcall   DOUSER
        .word   ubase

; SOURCE   -- adr n         current input buffer
;   'SOURCE 2@ ;        length is at higher adrs
        .pword  paddr(PAD_L)+PFLASH
SOURCE_L:
        .byte   NFA|6
        .ascii  "source"
        .align  2
SOURCE:
        rcall   TICKSOURCE
        goto    TWOFETCH

; /STRING  a u n -- a+n u-n          trim string
;   swap over - >r + r>
        .pword  paddr(SOURCE_L)+PFLASH
SLASHSTRING_L:
        .byte   NFA|7
        .ascii  "/string"
        .align  2
SLASHSTRING:
        rcall   SWOP
        rcall   OVER
        rcall   MINUS
        push    [W14--]
        rcall   PLUS
        pop     [++W14]
        return

; \     Skip the rest of the line
        .pword  paddr(SLASHSTRING_L)+PFLASH
BSLASH_L:
        .byte   NFA|IMMED|1
        .byte   0x5c
        .align  2
BSLASH:
        rcall   SOURCE
        rcall   TOIN
        rcall   STORE
        sub     W14, #2, W14
        return


; PARSE  char -- c-addr u
        .pword  paddr(BSLASH_L)+PFLASH
PARSE_L:
        .byte   NFA|5
        .ascii  "parse"
        .align  2
PARSE:
        mov     [W14++], [W14]      ; dup
        rcall   SOURCE
        rcall   TOIN
        rcall   FETCH
        rcall   SLASHSTRING
        mov     [W14++], [W14]      ; dup
        push    [W14--]
        rcall   ROT
        rcall   SKIP
        rcall   OVER
        push    [W14--]
        rcall   ROT
        rcall   SCAN
        cp0     [W14]
        bra     z, PARSE1
        dec     [W14], [W14]
PARSE1:
        pop     [++W14]     ; R>
        pop     [++W14]     ; R>
        rcall   ROT
        rcall   MINUS
        rcall   TOIN
        rcall   PLUSSTORE
        rcall   TUCK
        goto    MINUS

; WORD   char -- c-addr        word delimited by char
; Length byte is written to the input buffer.
        .pword  paddr(PARSE_L)+PFLASH
WORD_L:
        .byte   NFA|4
        .ascii  "word"
        .align  2
WORD:
        rcall   PARSE
        rcall   SWOP
        rcall   ONEMINUS
        rcall   TUCK
        goto    CSTORE

        .pword  paddr(WORD_L)+PFLASH
ERASE_L:
        .byte   NFA|5
        .ascii  "erase"
        .align  2
ERASE_:
        mlit    0
        goto    FILL
       
        .pword  paddr(ERASE_L)+PFLASH
BLANKS_L:
        .byte   NFA|6
        .ascii  "blanks"
        .align  2
BLANKS:
        mlit    ' '
        goto    FILL

; FILL  addr n c --  copy u bytes from src to dst
; fill rot !p>r swap for dup pc! p+ next r>p drop ;
        .pword  paddr(BLANKS_L)+PFLASH
FILL_L:
        .byte   NFA|4
        .ascii  "fill"
        .align  2
FILL:
        push    W13             ; P to return stack
        mov     [W14--], W1
        push    [W14--]         ; Count to return stack
        mov     [W14], W13      ; addr to P
        mov     W1, [W14]       ; Char to parameter stack
        bra     FILL2
FILL1:
        mov     [W14++],[W14]
        rcall   PCSTORE
        inc     W13, W13
FILL2:
        dec     [--W15], [W15++] ; XNEXT
        bra     c, FILL1
        pop     W0               ; UNNEXT
        pop     W13
        sub     W14, #2, W14 
        return

; CMOVE  src dst u --  copy u bytes from src to dst
; cmove swap !p>r for c@+ pc! p+ next r>p drop ;
        .pword  paddr(FILL_L)+PFLASH
CMOVE_L:
        .byte   NFA|5
        .ascii  "cmove"
        .align  2
CMOVE:
        push    W13             ; P to return stack
        push    [W14--]         ; Count to return stack 
        mov     [W14--], W13    ; dst to P
        bra     CMOVE2
CMOVE1:
        rcall   CFETCHPP
        rcall   PCSTORE
        inc     W13, W13
CMOVE2:
        dec     [--W15], [W15++] ; XNEXT
        bra     c, CMOVE1
        pop     W0               ; UNNEXT
        pop     W13
        sub     W14, #2, W14
        return

; WMOVE  src dst u --  copy u words (cells) from src to dst
; wmove swap !p for @+ p! p2+ next drop ;
        .pword  paddr(CMOVE_L)+PFLASH
WMOVE_L:
        .byte   NFA|5
        .ascii  "wmove"
        .align  2
WMOVE:
        push    W13
        push    [W14--]              ; Count
        mov     [W14--], W13         ; dst 
        bra     WMOVE2
WMOVE1:
        rcall   FETCHPP
        rcall   PSTORE
        inc2    W13, W13
WMOVE2:
        dec     [--W15], [W15++] ; XNEXT
        bra     c, WMOVE1
        pop     W0               ; UNNEXT
        pop     W13
        sub     W14, #2, W14
        return

; place  src n dst --     place as counted str
        .pword  paddr(WMOVE_L)+PFLASH
PLACE_L:
        .byte   NFA|5
        .ascii  "place"
        .align  2
PLACE: 
        rcall   TWODUP
        rcall   CSTORE
        inc     [W14], [W14]        ; char+
        rcall   SWOP
        goto    CMOVE

; :     c@+ ( addr -- addr+1 n ) dup 1+ swap c@ ;
        .pword  paddr(PLACE_L)+PFLASH
CFETCHPP_L:
        .byte   NFA|3
        .ascii  "c@+"
        .align  2
CFETCHPP:
        mov     [W14++], [W14]      ; dup
        rcall   CFETCH
        mov     [W14-0x2], W0
        inc     W0, W0
        mov     W0, [W14-0x2]
        return

; :     @+ ( addr -- addr+2 n ) dup 2+ swap @ ;
        .pword  paddr(CFETCHPP_L)+PFLASH
FETCHPP_L:
        .byte   NFA|2
        .ascii  "@+"
        .align  2
FETCHPP:
        mov     [W14++], [W14]      ; dup
        rcall   FETCH
        mov     [W14-0x2], W0
        inc2    W0, W0
        mov     W0, [W14-0x2]      
        return

; NFA>CFA   nfa -- cfa    name adr -> code field
        .pword  paddr(FETCHPP_L)+PFLASH
NFATOCFA_L:
        .byte   NFA|3
        .ascii  "n>c"
        .align  2
NFATOCFA:
        rcall   CFETCHPP
        mlit    #NFL        ; nfu
        rcall   AND
        rcall   PLUS
        inc     [W14], [W14]
        mlit    #0xfffe
        goto    AND

; CFA>NFA   cfa -- nfa    code field addr -> name field addr
        .pword  paddr(NFATOCFA_L)+PFLASH
CFATONFA_L:
        .byte   NFA|3
        .ascii  "c>n"
        .align  2
CFATONFA:
        dec     [W14], [W14]
CFATONFA1:
        dec     [W14], [W14]
        mov     [W14++], [W14]      ; dup
        rcall   CFETCH              ; nfu
        mlit    #0x7F
        rcall   GREATER
        cp0     [W14--]
        bra     z, CFATONFA1
        return

; findi   c-addr nfa -- c-addr 0   if not found
;                          xt  1      if immediate
;                          xt -1      if "normal"
        .pword  paddr(CFATONFA_L)+PFLASH
FINDI_L:
        .byte   NFA|3
        .ascii  "(f)"
        .align  2
findi:
findi1:
FIND_1: 
        rcall   TWODUP
        rcall   OVER
        rcall   CFETCH
        rcall   NEQUAL
        cp0     [W14]
        bra     z, findi2
        sub     W14, #2, W14
        rcall   TWOMINUS
        rcall   FETCH
        bclr    [W14], #0           ; Compensate for bug in ASM30 súite
        mov     [W14++], [W14]      ; dup
findi2:
        cp0     [W14--]
        bra     nz, findi1
        cp0     [W14]
        bra     z, findi3
        mov     [W14--], [W14]      ; nip
        mov     [W14++], [W14]      ; dup
        rcall   NFATOCFA
        rcall   SWOP
        rcall   IMMEDQ
        rcall   ZEROEQUAL
        rcall   ONE
        rcall   OR
findi3: 
        rcall   PAUSE
        return

; IMMED?    nfa -- f        fetch immediate flag
        .pword  paddr(FINDI_L)+PFLASH
IMMEDQ_L:
        .byte   NFA|6
        .ascii  "immed?"
        .align  2
IMMEDQ: 
        rcall   CFETCH
        mlit    handle(IMMED)+PFLASH
        goto    AND

; FIND   c-addr -- c-addr 0   if not found
;                  xt  1      if immediate
;                  xt -1      if "normal"
        .pword  paddr(IMMEDQ_L)+PFLASH
FIND_L:
        .byte   NFA|4
        .ascii  "find"
        .align  2
FIND:   
        mlit    handle(kernellink)+PFLASH
        rcall   findi
        cp0     [W14]
        bra     nz, FIND1
        sub     W14, #2, W14
        rcall   LATEST
        rcall   FETCH
        rcall   findi
FIND1:
        return

; DIGIT?   c -- n -1   if c is a valid digit
        .pword  paddr(FIND_L)+PFLASH
DIGITQ_L:
        .byte   NFA|6
        .ascii  "digit?"
        .align  2
DIGITQ:
                                ; 1 = 31    A = 41
        mov     [W14++], [W14]  ; c c       c c
        mlit    0x39            ; c c 39    c c 39
        rcall   GREATER         ; c 0       c ffff
        cp0     [W14--]
        bra     z, DIGITQ1
        mlit    0x27
        rcall   MINUS
DIGITQ1:
        mlit    0x30            ; c 30
        rcall   MINUS           ; 1
        mov     [W14++], [W14]  ; 1 1
        rcall   BASE            ; 1 1 base
        rcall   FETCH           ; 1 1 10
        rcall   LESS            ; 1 ffff
        rcall   OVER            ; 1 ffff 1
        rcall   ZEROLESS        ; 1 ffff 0
        com     [W14], [W14]
        goto    AND

; SIGN?   adr n -- adr' n' f   get optional sign
        .pword  paddr(DIGITQ_L)+PFLASH
SIGNQ_L:
        .byte   NFA|5
        .ascii  "sign?"
        .align  2
SIGNQ:
        rcall   OVER
        rcall   CFETCH
        mlit    ','
        rcall   MINUS
        mov     [W14++], [W14]      ; dup
        rcall   ABS
        rcall   ONE
        rcall   EQUAL
        rcall   AND
        cp0     [W14]
        bra     z, QSIGN1
        inc     [W14], [W14]
        push    [W14--]
        rcall   ONE
        rcall   SLASHSTRING
        pop     [++W14]
QSIGN1: return
.if 1
; >NUMBER  0 0  adr u -- ud.l ud.h  adr' u'
;                       convert string to number
        .pword  paddr(SIGNQ_L)+PFLASH
TONUMBER_L:
        .byte   NFA|7
        .ascii  ">number"
        .align  2
TONUMBER:
TONUM1:
        cp0     [W14]           ; ud.l ud.h  adr u
        bra     z, TONUM3
        push    [W14--]
        push    [W14]
        rcall   CFETCH
        rcall   DIGITQ
        cp0     [W14--]
        bra     nz, TONUM2
        sub     W14, #2, W14
        pop     [++W14]
        pop     [++W14]
        bra     TONUM3
TONUM2:
        push    [W14--]            ; ud.l ud.h digit 
        rcall   BASE
        rcall   FETCH
        rcall   UDSTAR
        pop     [++W14]
        rcall   MPLUS
        pop     [++W14]
        pop     [++W14]  
        rcall   ONE
        rcall   SLASHSTRING
        bra     TONUM1
TONUM3: 
        return

BASEQV:   
        .word      handle(DECIMAL)+PFLASH
        .word      handle(HEX)+PFLASH
        .word      handle(BIN)+PFLASH
; NUMBER?  c-addr -- n 1      string->number
;                 -- dl du 2   string-> double number
;                 -- c-addr 0  if convert error
        .pword  paddr(TONUMBER_L)+PFLASH
NUMBERQ_L:
        .byte   NFA|7
        .ascii  "number?"
        .align  2
NUMBERQ:
        mov     [W14++], [W14]  ; a a
        rcall   FALSE_          ; a a 0
        rcall   FALSE_          ; a a 0 0 
        rcall   ROT             ; a 0 0 a
        rcall   CFETCHPP        ; a 0 0 a' u
        rcall   SIGNQ           ; a 0 0 a' u f
        push    [W14--]         ; a 0 a' u
        rcall   BASE
        rcall   FETCH
        push    [W14--]        ; a 0 0 a' u
        rcall   OVER
        rcall   CFETCH
        mlit    '#'
        rcall   MINUS
        mov     [W14++], [W14]      ; dup
        mlit    3
        rcall   ULESS
        cp0     [W14--]
        bra     z, BASEQ1
        sl      [W14], [W14]        ; CELLS
        mlit    handle(BASEQV)+PFLASH
        rcall   PLUS
        rcall   FEXECUTE
        rcall   ONE
        rcall   SLASHSTRING
        bra     BASEQ2
BASEQ1:
        sub     W14, #2, W14
BASEQ2: 
        rcall   TONUMBER        ; a ud.l ud.h  a' u
        pop     [++W14]         ; a ud.l ud.h  a' u oldbase
        rcall   BASE            ; a ud.l ud.h  a' u oldbase addr
        rcall   STORE           ; a ud.l ud.h  a' u
        
        mov     [W14++], [W14]
        dec2    [W14], [W14]
        rcall   ZEROLESS
        cp0     [W14--]         ; a ud.l ud.h  a' u
        bra     nz, QNUMD
        
QNUM_ERR:                       ; Not a number
        pop     [++W14]         ; a ud.l ud.h a' u sign
        sub     W14, #6, W14
QNUM_ERR1:      
        sub     W14, #4, W14
        rcall   FALSE_          ; a 0           Not a number
        bra     QNUM3

QNUMD:                          ; Double number
                                ; a ud.l ud.h a' u
        rcall   TWOSWAP         ; a a' u ud.l ud.h 
        pop     [++W14]         ; a a' u ud.l ud.d sign
        cp0     [W14--]
        bra     z, QNUMD1
        rcall   DNEGATE
QNUMD1: 
        rcall   TWOSWAP         ; a d.l d.h a' u
        cp0     [W14--]         ; a d.l d.h a'
        bra     z, QNUM1
        rcall   CFETCH
        mlit    '.'
        rcall   MINUS
        cp0     [W14--]         ; a d.l d.h
        bra     nz, QNUM_ERR1
        rcall   ROT             ; d.l d.h a
        sub     W14, #2, W14    ; d.l d.h
        mlit    2               ; d.l ud.h 2    Double number
        bra     QNUM3
QNUM1:                          ; single precision dumber
                                ; a ud.l ud.h  a'
        rcall   TWODROP         ; a n
        rcall   NIP             ; n
        rcall   ONE             ; n 1           Single number
QNUM3:  
        return

.else

; >NUMBER  n adr u -- n' adr' u'
;                       convert string to number
        .pword  paddr(SIGNQ_L)+PFLASH
TONUMBER_L:
        .byte   NFA|7
        .ascii  ">number"
        .align  2
TONUMBER:
TONUM1:
        cp0     [W14]      ; n adr u
        bra     z, TONUM3
        rcall   OVER               ; n adr u adr 
        rcall   CFETCH             ; n adr u c
        rcall   DIGITQ             ; n adr u m f
        cp0     [W14--]            ; n adr u m
        bra     nz, TONUM2
        sub     W14, #2, W14       ; n adr u
        bra     TONUM3
TONUM2:
        push    [W14--]            ; n adr u 
        rcall   ROT                ; adr u n 
        rcall   BASE
        rcall   FETCH              ; adr u n base  
        rcall   STAR               ; adr u n''  
        pop     [++W14]            ; adr u n'' m
        rcall   PLUS               ; adr u n' 
        rcall   ROT
        rcall   ROT                ; n' adr u  
        rcall   ONE
        rcall   SLASHSTRING
        bra     TONUM1
TONUM3: 
        return

BASEQV:   
        .word      handle(DECIMAL)+PFLASH
        .word      handle(HEX)+PFLASH
        .word      handle(BIN)+PFLASH
; NUMBER?  c-addr -- n -1      string->number
;                 -- c-addr 0  if convert error
        .pword  paddr(TONUMBER_L)+PFLASH
NUMBERQ_L:
        .byte   NFA|7
        .ascii  "number?"
        .align  2
NUMBERQ:
        mov     [W14++], [W14]  ; a a
        rcall   FALSE_          ; a a 0
        rcall   SWOP            ; a 0 a
        rcall   CFETCHPP        ; a 0 a' u
        rcall   SIGNQ           ; a 0 a' u f
        push    [W14--]         ; a 0 a' u
        rcall   BASE
        rcall   FETCH
        push    [W14--]        ; a 0 a' u
        rcall   OVER
        rcall   CFETCH
        mlit    '#'
        rcall   MINUS
        mov     [W14++], [W14]      ; dup
        mlit    3
        rcall   ULESS
        cp0     [W14--]
        bra     z, BASEQ1
        sl      [W14], [W14]        ; CELLS
        mlit    handle(BASEQV)+PFLASH
        rcall   PLUS
        rcall   FEXECUTE
        rcall   ONE
        rcall   SLASHSTRING
        bra     BASEQ2
BASEQ1:
        sub     W14, #2, W14
BASEQ2: 
        rcall   TONUMBER        ; a n a' u
        pop     [++W14]         ; a n a' u oldbase
        rcall   BASE            ; a n a' u oldbase addr
        rcall   STORE           ; a n a' u
        cp0     [W14--]         ; a n a'
        bra     z, QNUM1
        pop     [++W14]         ; a n a' f
        sub     W14, #6, W14        ; 3drop
        rcall   FALSE_
        bra     QNUM3
QNUM1:
        sub     W14, #2, W14    ; a n
        mov     [W14--], [W14]  ; n
        pop     [++W14]         ; n f
        cp0     [W14--]
        bra     z, QNUM2
        neg     [W14], [W14]
QNUM2:  
        rcall   TRUE_
QNUM3:  
        return

.endif
; TIBSIZE  -- n                      size of TIB
; CONSTANT
        .pword  paddr(NUMBERQ_L)+PFLASH
TIBSIZE_L:
        .byte   NFA|3
        .ascii  "ti#"
        .align  2
TIBSIZE:
        rcall   TASK
        rcall   FETCH
        mlit    0x5
        rcall   PLUS
        goto    CFETCH

; TIU     -- a-addr        Terminal Input Buffer Pointer
        .pword  paddr(TIBSIZE_L)+PFLASH
TIB_L:
        .byte   NFA|3
        .ascii  "tib"
        .align  2
TIB:
        rcall   TIU
        goto    FETCH      ; pointer to Terminal input buffer pointer

; TIU     -- a-addr        Terminal Input Buffer Pointer
        .pword  paddr(TIB_L)+PFLASH
TIU_L:
        .byte   NFA|3
        .ascii  "tiu"
        .align  2
TIU:
        rcall   DOUSER
        .word   utib             ; pointer to Terminal input buffer pointer

; >IN     -- a-addr        holds offset into TIB
; In RAM
        .pword  paddr(TIU_L)+PFLASH
TOIN_L:
        .byte   NFA|3
        .ascii  ">in"
        .align  2
TOIN:
        rcall   DOUSER
        .word   utoin

; 'SOURCE  -- a-addr        two cells: len, adrs
        .pword  paddr(TOIN_L)+PFLASH
TICKSOURCE_L:
        .byte   NFA|7
        .ascii  "'source"
        .align  2
TICKSOURCE:
        rcall   DOUSER
        .word   usource           ; two cells !!!!!!


; Check the inline bit.
; IN? ( nfa -- flag )
        .pword  paddr(TICKSOURCE_L)+PFLASH
INQ_L:
        .byte   NFA|3
        .ascii  "in?"
        .align  2
INQ:
        rcall   CFETCH
        mlit    INLINE
        goto    AND

; DEBUG-------------------------------------------------
.if 0
DBG1:
        rcall   CR
        mov     [W15-4], W0
        mov     W0, [++W14]
        dec2    [W14], [W14]
        rcall   UDOT
        mlit    #':'
        rcall   EMIT
        goto    DOTS
.endif
; -------------------------------------------------------
;nop
;  INTERPRET  i*x c-addr u -- j*x   interpret given buffer
        .pword  paddr(INQ_L)+PFLASH
INTERPRET_L:
        .byte   NFA|9
        .ascii  "interpret"
        .align  2
INTERPRET: 
        rcall   TICKSOURCE
        rcall   TWOSTORE
        rcall   FALSE_
        rcall   TOIN
        rcall   STORE
INTER1: 
        rcall   BL
        rcall   WORD
        mov     [W14++], [W14]      ; dup
        rcall   CFETCH
        cp0     [W14--]
        bra     z, INTER6
        rcall   FIND
        cp0     [W14]
        bra     z, INUMBER
        inc     [W14], [W14]
        rcall   STATE
        rcall   ZEROEQUAL
        rcall   OR
        cp0     [W14--]
        bra     z, INTER2
                            ; Compiling immediate or interpret state
        mov     [W14++], [W14]      ; dup
        rcall   CFATONFA
        rcall   CFETCH
        mlit    COMPILE
        rcall   AND 
        cp0     [W14--]
        bra     z, INTER11  ; Interpretable word
        rcall   STATE       ; Compile only word
        rcall   XSQUOTE
        .byte   12
        .ascii  "COMPILE ONLY"
        .align  2
        rcall   QABORT
INTER11:
        mov     [W14++], [W14]      ; dup
        mlit    handle(SEMICOLON)+PFLASH
        rcall   EQUAL
        cp0     [W14--]
        btsc    SRL, #Z
        bclr    iflags, #tailcall  ; allow tailcall optimisation
        bclr    iflags, #noclear ; dont clear flags in case of \
        mov     [W14++], [W14]      ; dup
        mlit    handle(BSLASH)+PFLASH
        rcall   EQUAL
        cp0     [W14--]
        btsc    SRL, #Z
        bset    iflags, #noclear
        rcall   EXECUTE         ; Execute a word
        btss    iflags, #noclear
        bra     INTER1
        bclr    iflags, #izeroeq ; Clear 0= encountered in compilation
        bclr    iflags, #idup    ; Clear DUP encountered in compilation
        bra     INTER1
INTER2:
        bclr    iflags, #tailcall  ; allow tailcall optimisation
        bclr    iflags, #izeroeq ; Clear 0= encountered in compilation
        mov     [W14++], [W14]      ; dup
        mlit    handle(ZEROEQUAL)+PFLASH ; Check for 0=, modifies IF and UNTIL to use bnz
        rcall   EQUAL
        cp0     [W14--]
        bra     z, INTER3
        bset    iflags, #izeroeq ; Mark 0= encountered in compilation
        bra     INTERCALL
INTER3:
        mov     [W14++], [W14]      ; dup
        mlit    handle(DUP)+PFLASH
        rcall   EQUAL
        cp0     [W14--]
        bra     z, INTERDUPCLR
        bset    iflags, #idup    ; Mark DUP encountered during compilation
        bra     INTERXT
INTERDUPCLR:
        bclr    iflags, #idup    ; Clear DUP encountered in compilation
INTERXT:
        mov     [W14++], [W14]      ; dup
        rcall   CFATONFA
        rcall   INQ
        cp0     [W14--]
        bra     z, INTERCALL
        rcall   INLINE_0           ; Inline the word
        bra     INTER1

INTERCALL:
        rcall   COMMAXT           ; Compile a call
        bra     INTER1

INUMBER:                           ; Convert to number
        bclr    iflags, #tailcall  ; allow tailcall optimisation
        bclr    iflags, #izeroeq   ; Clear 0= encountered in compilation
        bclr    iflags, #idup      ; Clear DUP encountered in compilation
        sub     W14, #2, W14
        rcall   NUMBERQ
        cp0     [W14]
        bra     z, IUNKNOWN
        rcall   STATE
        cp0     [W14--]
        bra     z, INUMBER1
        btss    [W14--], #1
        bra     ISINGLE
IDOUBLE:
        rcall   SWOP
        rcall   LITERAL
ISINGLE:
        rcall   LITERAL
        bra     INTER1
INUMBER1:
        sub     W14, #2, W14
        bra     INTER1
IUNKNOWN:                        ; ?????
        dec2    W14, W14
        rcall   CFETCHPP
        rcall   TYPE
        rcall   FALSE_
        rcall   QABORTQ
;        bra     INTER1
INTER6: 
        sub     W14, #2, W14
        return
;nop
        .pword  paddr(INTERPRET_L)+PFLASH
SHB_L:
        .byte   NFA|3
        .ascii  "shb"     ; Set header bit
        .align  2
SHB:
        rcall   LATEST
        rcall   FETCH
        mov     [W14++], [W14]      ; dup
        rcall   CFETCH              ; nfu
        rcall   ROT
        rcall   OR
        rcall   SWOP
        goto    CSTORE

        .pword  paddr(SHB_L)+PFLASH
IMMEDIATE_L:
        .byte   NFA|9
        .ascii  "immediate" ; 
        .align  2
IMMEDIATE:
        mlit    IMMED
        goto    SHB

        .pword  paddr(IMMEDIATE_L)+PFLASH
INLINED_L:
        .byte   NFA|7
        .ascii  "inlined" ; 
        .align  2
INLINED:
        mlit    INLINE
        goto    SHB

;; .st ( -- ) output a string with current data section and current base info
;;; : .st base @ dup decimal <#  [char] , hold #s  [char] < hold #> type 
;;;     <# [char] > hold cse #s #> type base ! ;
        .pword  paddr(INLINED_L)+PFLASH
DOTSTATUS_L:
        .byte   NFA|3
        .ascii  ".st"
        .align  2
DOTSTATUS:
        mlit    0x3c
        rcall   EMIT
        rcall   DOTBASE
        rcall   EMIT
        mlit    0x2C
        rcall   EMIT
        rcall   MEMQ
        rcall   TYPE
        mlit    0x3e
        rcall   EMIT
        rcall   SPACE_
        goto    DOTS

.ifndef PEEPROM
FTURNKEY_A:
        rcall   DOCREATE
        .word   PFLASH + CONFIG_DATA
FRAM_A:
        rcall   DOCREATE
        .word   PFLASH + CONFIG_DATA + IBUFSIZEL
FLATEST_A:
        rcall   DOCREATE
        .word   PFLASH + CONFIG_DATA + IBUFSIZEL*2
FFLASH_A:
        rcall   DOCREATE
        .word   PFLASH + CONFIG_DATA + IBUFSIZEL*3

; dp0 ( -- ) Initialize turnkey, DPs and latest in flash
;        dw      link
;link    set     $
;        db      NFA|3,"dp0"
DP_COLD:
        
        mlit    handle(STARTV)+PFLASH
        rcall   FETCHPP
        rcall   FTURNKEY_A
        rcall   EEINIT
        
        rcall   FETCHPP
        rcall   FRAM_A
        rcall   EEINIT
        
        rcall   FETCHPP
        rcall   FLATEST_A
        rcall   EEINIT
        
        rcall   FETCH
        rcall   FFLASH_A
        bra     EEINIT
.endif

; dp> ( -- ) Copy ini, dps and latest from eeprom to ram
;        dw      link
; link    set     $
        .pword  paddr(DOTSTATUS_L)+PFLASH
DP_TO_RAM_L:
        .byte   NFA|3
        .ascii  "dp>"
        .align  2
DP_TO_RAM:
.ifdef PEEPROM
        mlit    dp_start
        mlit    dpSTART
        mlit    5
        goto    WMOVE
.else
        push    W13             ; P to return stack
        mov     #dpSTART, W13   ; dst to P
        rcall   FTURNKEY_A
        mlit    4
        push    [W14--]
DP_TO_RAM1:
        rcall   DUP
        rcall   EEREAD
        rcall   PSTORE
        rcall   PPLUS2
        rcall   PLUS0x400
        dec     [--W15], [W15++] ; XNEXT
        bra     nz, DP_TO_RAM1
DP_TO_RAM2:
        pop     W0               ; UNNEXT
        pop     W13
        goto    DROP

PLUS0x400:
        mlit    0x400
        bra     PLUS
.endif

; >dp ( -- ) Copy only changed ini, dp's and latest from ram to eeprom
        .pword  paddr(DP_TO_RAM_L)+PFLASH
DP_TO_EEPROM_L:
        .byte   NFA|3
        .ascii  ">dp"
        .align  2
DP_TO_EEPROM:
.ifdef  PEEPROM
        mlit    dp_start
        push    W13
        mov     [W14--], W13
        mlit    dpSTART
        mlit    5
        push    [W14--]
        bra     DP_TO_EEPROM_3
DP_TO_EEPROM_0:
        rcall   FETCHPP
        mov     [W14++], [W14]      ; dup
        rcall   PFETCH
        rcall   NOTEQUAL
        cp0     [W14--]
        bra     z, DP_TO_EEPROM_1
        rcall   PSTORE
.if DEBUG_FLASH == 1
        mov     #'E', W2
        mov     W2, U1TXREG
.endif
        bra     DP_TO_EEPROM_2
DP_TO_EEPROM_1:
        sub     W14, #2, W14
DP_TO_EEPROM_2:
        inc2    W13, W13
DP_TO_EEPROM_3:
        dec     [--W15], [W15++] ; XNEXT
        bra     c, DP_TO_EEPROM_0
        pop     W0
        pop     W13
        sub     W14, #2, W14
        return
.else
        mlit    dpSTART
        push    W13             ; P to return stack
        mov     [W14--], W13    ; src in ram
        rcall   FTURNKEY_A      ; dst in flash
        mlit    4
        push    [W14--]
DP_TO_EEPROM_0:
        rcall   DUP
        rcall   EEREAD
        rcall   PFETCH
        rcall   EQUAL
        cp0     [W14--]
        bra     nz, DP_TO_EEPROM_1
        rcall   PFETCH
        rcall   OVER
        rcall   EEWRITE
.if DEBUG_FLASH == 1
        mov     #'E', W2
        mov     W2, U1TXREG
.endif
DP_TO_EEPROM_1:
        rcall   PLUS0x400
        rcall   PPLUS2
        dec     [--W15], [W15++] ; XNEXT
        bra     nz, DP_TO_EEPROM_0
        pop     W0
        pop     W13
        sub     W14, #2, W14
        return
.endif

;;; Check parameter stack pointer
        .byte   NFA|3
        .ascii  "sp?"
        .align  2
check_sp:
        rcall   SPFETCH
        rcall   S0
        rcall   FETCH
        rcall   TIB
        rcall   WITHIN
        rcall   XSQUOTE
        .byte   3
        .ascii  "SP?"
        .align  2
        rcall   QABORT
        return
; QUIT     --    R: i*x --    interpret from kbd
        .pword  paddr(DP_TO_EEPROM_L)+PFLASH
QUIT_L:
        .byte   NFA|4
        .ascii  "quit"
        .align  2
QUIT:
        rcall   RPEMPTY         ; Empty the return stack
        rcall   LEFTBRACKET
        rcall   RAM
QUIT0:  
        rcall   IFLUSH
        ;; Copy INI and DP's from eeprom to ram
        rcall   DP_TO_RAM 
QUIT1: 
        rcall   check_sp
        rcall   CR
        rcall   TIB
        mov     [W14++], [W14]      ; dup
        rcall   TIBSIZE
        mlit    HOLD_SIZE             ; Reserve  for hold buffer
        rcall   MINUS
        rcall   ACCEPT
        rcall   SPACE_
        rcall   INTERPRET
        rcall   STATE
        cp0     [W14--]
        bra     nz, QUIT1
        rcall   DP_TO_EEPROM   
        rcall   XSQUOTE
        .byte   3
        .ascii  " ok"
        .align  2
        rcall   TYPE
        rcall   PROMPT
        bra     QUIT0
        return

        .pword  paddr(QUIT_L)+PFLASH
PROMPT_L:
        .byte   NFA|6
        .ascii  "prompt"
        .align  2
PROMPT:
        rcall   DEFER_DOES
        .word   prompt

; ABORT    i*x --   R: j*x --   clear stk & QUIT
        .pword  paddr(PROMPT_L)+PFLASH
ABORT_L:
        .byte   NFA|5
        .ascii  "abort"
        .align  2
ABORT:
        rcall   S0
        rcall   FETCH
        rcall   SPSTORE
        goto    QUIT            ; QUIT never returns

; ?ABORT   f --       abort & print ?
        .pword  paddr(ABORT_L)+PFLASH
QABORTQ_L:
        .byte   NFA|7
        .ascii  "?abort?"
        .align  2
QABORTQ:
        rcall   XSQUOTE
        .byte   1
        .ascii  "?"
        .align  2
        bra     QABORT

; ?ABORT   f c-addr u --       abort & print msg
        .pword  paddr(QABORTQ_L)+PFLASH
QABORT_L:
        .byte   NFA|6
        .ascii  "?abort"
        .align  2
QABORT:
        rcall   ROT
        cp0     [W14--]
        bra     nz, QABO1
QABORT1:        
        rcall   SPACE_
        rcall   TYPE
        rcall   ABORT  ; ABORT never returns
QABO1:  sub     W14, #4, W14        ; 2drop
        return

; ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;         i*x x1 --       R: j*x --      x1<>0
        .pword  paddr(QABORT_L)+PFLASH
ABORTQUOTE_L:
        .byte   NFA|IMMED|COMPILE|6
        .ascii  "abort\""
        .align  2
ABORTQUOTE:
        rcall   SQUOTE
        rcall   DOCOMMAXT
        .word   handle(QABORT)+PFLASH
        return

; '    -- xt             find word in dictionary
        .pword  paddr(ABORTQUOTE_L)+PFLASH
TICK_L:
        .byte   NFA|1
        .byte   0x27
        .align  2
TICK:
        rcall   BL
        rcall   WORD
        rcall   FIND
        goto    QABORTQ

; CHAR   -- char           parse ASCII character
        .pword  paddr(TICK_L)+PFLASH
CHAR_L:
        .byte   NFA|4
        .ascii  "char"
        .align  2
CHAR:
        rcall   BL
        rcall   PARSE
        dec2    W14, W14
        goto    CFETCH

; (    --                     skip input until )
        .pword  paddr(CHAR_L)+PFLASH
PAREN_L:
        .byte   NFA|IMMED|1
        .ascii  "("
        .align  2
PAREN:
        mlit    0x29
        rcall   PARSE
        sub     W14, #4, W14
        bclr    iflags, #noclear ; dont clear flags in case of (
        return

; IHERE    -- a-addr    return Code dictionary ptr
;   IDP @ ;
;        .pword  paddr(PAREN_L)+PFLASH
IHERE_L:
        .byte   NFA|5
        .ascii  "ihere"
        .align  2
IHERE:
        mov     dpFLASH, W0
        mov     W0, [++W14]     ; IHERE must not trash W12=hibyte
        return
;        rcall   IDP
;        goto    FETCH

; [CHAR]   --          compile character literal
        .pword  paddr(PAREN_L)+PFLASH
BRACCHAR_L:
        .byte   NFA|IMMED|COMPILE|6
        .ascii  "[char]"
        .align  2
BRACCHAR:
        rcall   CHAR
        goto    LITERAL

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
        .pword  paddr(BRACCHAR_L)+PFLASH
CREATE_L:
        .byte   NFA|6
        .ascii  "create"
        .align  2
CREATE:
        rcall   BL
        rcall   WORD
        mov     [W14++], [W14]      ; Remember word
        rcall   FIND
        rcall   NIP
        rcall   ZEROEQUAL
        rcall   XSQUOTE
        .byte   15
        .ascii  "ALREADY DEFINED"
        .align  2
        rcall   QABORT           ; ABORT if word has already been defined
        mov     [W14++], [W14]      ; Remember word
        rcall   CFETCH
        rcall   ONE
        mlit    #16
        rcall   WITHIN
        rcall   QABORTQ          ; Abort if there is no name for create

        rcall   LATEST
        rcall   FETCH
        rcall   ICOMMA          ; Link field
        rcall   CFETCHPP        ; str len
        rcall   IHERE
        mov     [W14++], [W14]      ; dup             
        rcall   LATEST          ; new 'latest' link
        rcall   STORE           ; str len ihere
        rcall   PLACE           ; 
        rcall   IHERE           ; ihere
        rcall   CFETCH
        mlit    NFA             ; nfa
        rcall   SHB
        inc     [W14], [W14]
        rcall   ALIGNED
        rcall   IALLOT          ; The header has now been created
        rcall   DOCOMMAXT       ; Append an exeution token
        .word   handle(DOCREATE)+PFLASH
                                ; compiles the runtime routine to fetch the next dictionary cell to the parameter stack
        rcall   ALIGN
        rcall   HERE            ; compiles the current dataspace dp into the dictionary
        mov     cse, W0
        sub     #4, W0
        bra     nz, CREATE2
        inc2    [W14], [W14]
CREATE2:
        goto    ICOMMA          ; dp now points to a free cell

;***************************************************************
; POSTPONE
        .pword  paddr(CREATE_L)+PFLASH
POSTPONE_L:
        .byte   NFA|IMMED|COMPILE|8
        .ascii  "postpone"
        .align  2
POSTPONE:
        rcall   BL
        rcall   WORD
        rcall   FIND
        mov     [W14++], [W14]      ; dup
        rcall   QABORTQ
        rcall   ZEROLESS
        cp0     [W14--]
        bra     z, POSTPONE1
        rcall   DOCOMMAXT
        .word   handle(DOCOMMAXT)+PFLASH
        goto    ICOMMA
POSTPONE1:
        goto    COMMAXT

;***************************************************************
; (DOES>)  --      run-time action of DOES>
;        dw     link
;link   set     $
XDOES_L:
        .byte   NFA|7
        .ascii  "(does>)"
        .align  2
XDOES:
        pop     W0
        pop     W0
        mov     #PFLASH, W1
        add     W0, W1, [++W14]     ; a
        rcall   LATEST      ; a a
        rcall   FETCH       ; a nfa
        rcall   NFATOCFA    ; a cfa
        rcall   IHERE       ; a cfa ihere
        push    [W14--]     ; a cfa
        rcall   IDP         ; a cfa idp
        rcall   STORE       ; a
        rcall   COMMAXT
        pop     [++W14]     ; ihere
        rcall   IDP         ; ihere idp
        rcall   STORE       ;
        return

; DOES>    --      change action of latest def'n
        .pword  paddr(POSTPONE_L)+PFLASH
DOES_L:
        .byte   NFA|IMMED|COMPILE|5
        .ascii  "does>"
        .align  2
DOES:
        rcall   DOCOMMAXT
        .word   handle(XDOES)+PFLASH
        rcall   DOCOMMAXT
        .word   handle(DODOES)+PFLASH
        return

;*****************************************************************
; [        --      enter interpretive state
        .pword  paddr(DOES_L)+PFLASH
LEFTBRACKET_L:
        .byte   NFA|IMMED|1
        .ascii  "["
        .align  2
LEFTBRACKET:
        clr     state
        return

; ]        --      enter compiling state
        .pword  paddr(LEFTBRACKET_L)+PFLASH
RIGHTBRACKET_L:
        .byte   NFA|1
        .ascii  "]"
        .align  2
RIGHTBRACKET:
        setm    state
        return

; :        --           begin a colon definition
        .pword  paddr(RIGHTBRACKET_L)+PFLASH
COLON_L:
        .byte   NFA|1
        .ascii  ":"
        .align  2
COLON:
        rcall   CREATE
        rcall   RIGHTBRACKET
        mlit    0xfffc      ; -4
        goto    IALLOT

; :noname        -- a          define headerless forth code
        .pword  paddr(COLON_L)+PFLASH
NONAME_L:
        .byte   NFA|7
        .ascii  ":noname"
        .align  2
NONAME:
        rcall   IHERE
        goto   RIGHTBRACKET

; ;        --             end a colon definition
;   return, [
        .pword  paddr(NONAME_L)+PFLASH
SEMICOLON_L:
        .byte   NFA|IMMED|COMPILE|1
        .ascii  ";"
        .align  2
                              ; Tail call optimisation
SEMICOLON:
        btst    iflags, #tailcall
        bra     nz, SEMICOLON2
        rcall   IHERE
        dec2    [W14], [W14]      ; 2-
        mov     [W14++], [W14]    ; DUP
        rcall   CF_FETCH
        mlit    7
        rcall   EQUAL
        cp0     [W14--]
        bra     z, SEMICOLON1
        sl      [W14], [W14]        ; 2*
        rcall   IHERE
        rcall   PLUS
        mlit    PFLASH
        rcall   MINUS
        mlit    4
        rcall   ROT
        rcall   CF_STORE
        clr     [++W14]
        clr     [++W14]
        rcall   IHERE
        rcall   CF_STORE
        rcall   CELL
        rcall   IALLOT
        bra     SEMICOLON3
SEMICOLON1:
        sub     W14, #4, W14
SEMICOLON2:
        bclr    iflags, #tailcall
        rcall   RETURN_
SEMICOLON3:
        goto    LEFTBRACKET

; ;i        --             end a interrupt colon definition

        .pword  paddr(SEMICOLON_L)+PFLASH
SEMICOLONI_L:
        .byte   NFA|IMMED|COMPILE|2
        .ascii  ";i"
        .align  2
SEMICOLONI:
.ifdecl INTTREG
        mlit    handle(ALT_INT_EXIT)+PFLASH
        rcall   AGAINC
.else
        rcall   RETFIE_
.endif
        goto    LEFTBRACKET

; [']  --         find word & compile as literal
        .pword  paddr(SEMICOLONI_L)+PFLASH
BRACTICK_L:
        .byte   NFA|IMMED|COMPILE|3
        .ascii  "[']"
        .align  2
BRACTICK:
        rcall   TICK       ; get xt of 'xxx'
        goto    LITERAL

; ,?0=    -- addr  Compile ?0= and make make place for a branch instruction
COMMAZEROSENSE_L:
        .byte   NFA|4
        .ascii  ",?0="
        .align  2
COMMAZEROSENSE:
        btst    iflags, #idup
        bra     nz, COMMAZEROSENSE1
        mlit    handle(ZEROSENSE)+PFLASH
        bra     COMMAZEROSENSE2
COMMAZEROSENSE1:
        rcall   IDPMINUS
        mlit    handle(DUPZEROSENSE)+PFLASH
COMMAZEROSENSE2:
        bclr    iflags, #idup
        bra     INLINE_0
ZEROSENSE:
        cp0     [W14--]
        return
DUPZEROSENSE:
        cp0     [W14]
        return

        .pword  paddr(BRACTICK_L)+PFLASH
INLINE_L:
        .byte   NFA|IMMED|COMPILE|6
        .ascii  "inline"
        .align  2
        bclr    iflags, #izeroeq
        bclr    iflags, #idup
        rcall   TICK
        goto    INLINE_0

; in, ( cfa -- ) begin dup cf@ dup $0006 <> while as, 2+ repeat 3drop ;
        .pword  paddr(INLINE_L)+PFLASH
INC_L:
        .byte   NFA|3
        .ascii  "in,"
        .align  2
INLINE_0:
        mov     [W14++], [W14]      ; dup
        rcall   CF_FETCH
        mov     [W14++], [W14]      ; dup
        mlit    #6
        rcall   NOTEQUAL
        cp0     [W14--]
        bra     z, INLINE1
        rcall   AS_COMMA
        inc2    [W14], [W14]
        bra     INLINE_0
INLINE1:
        sub     W14, #6, W14        ; 3drop
        return

; un, ( -- cc)   Unconditional
        .pword  paddr(INC_L)+PFLASH
UNC_L:
        .byte   NFA|3
        .ascii  "un,"
        .align  2
UNC:
        rcall   DOCREATE
        .word   7

; z,  ( -- cc)   Zero
        .pword  paddr(UNC_L)+PFLASH
ZC_L:
        .byte   NFA|2
        .ascii  "z,"
        .align  2
ZC:
        rcall   DOCREATE
        .word   2

; nc, ( -- cc) Carry
        .pword  paddr(ZC_L)+PFLASH
NC_L:
        .byte   NFA|3
        .ascii  "nc,"
        .align  2
NC:
        rcall   DOCREATE
        .word   9

; not, ( cc -- opposite-cc)  Reverse the condition code
        .pword  paddr(NC_L)+PFLASH
NOTC_L:
        .byte   NFA|4
        .ascii  "not,"
        .align  2
NOTC:
        mlit    8
        goto    XOR

; if, ( cc -- here)  Assembler if
        .pword  paddr(NOTC_L)+PFLASH
IFC_L:
        .byte   NFA|3
        .ascii  "if,"
        .align  2
IFC:
        rcall   NOTC
        rcall   IHERE
        rcall   SWOP
        rcall   FALSE_
        goto    BRA_

; then, ( back-addr -- )  Assembler then
        .pword  paddr(IFC_L)+PFLASH
THENC_L:
        .byte   NFA|5
        .ascii  "then,"
        .align  2
THENC:
        bset    iflags, #tailcall  ; Disable tailcall optimisation
        rcall   IHERE
        rcall   OVER
        rcall   MINUS
        dec2    [W14], [W14]
        asr     [W14], [W14]        ; 2/
        rcall   SWOP
        push    [W14]
        rcall   CF_FETCH
        mov     [W14--], [W14]      ; nip
        pop     [++W14]
        goto    CF_STORE

; else, ( back-addr -- here )  Assembler else
        .pword  paddr(THENC_L)+PFLASH
ELSEC_L:
        .byte   NFA|5
        .ascii  "else,"
        .align  2
ELSEC:
        rcall   IHERE
        rcall   UNC
        rcall   FALSE_
        rcall   BRA_
        rcall   SWOP
        goto    THENC

; begin,    -- adrs        target for bwd. branch
        .pword  paddr(ELSEC_L)+PFLASH
BEGINC_L:
        .byte   NFA|6
        .ascii  "begin,"
        .align  2
BEGINC:
        goto    IHERE

; again,    adrs --      uncond'l backward branch
;   unconditional backward branch
        .pword  paddr(BEGINC_L)+PFLASH
AGAINC_L:
        .byte   NFA|6
        .ascii  "again,"
        .align  2
AGAINC:
        rcall   IHERE
        rcall   MINUS
        dec2    [W14], [W14]
        rcall   UNC
        rcall   SWOP
        goto    BRA_

; until,    adrs cc --   Branch bakwards if cc
        .pword  paddr(AGAINC_L)+PFLASH
UNTILC_L:
        .byte   NFA|6
        .ascii  "until,"
        .align  2
UNTILC:
        bset    iflags, #tailcall  ; Disable tailcall optimisation
        rcall   NOTC
        rcall   SWOP
        rcall   IHERE
        rcall   MINUS
        dec2    [W14], [W14]
        goto    BRA_

;;; Forget the latest compiled one cell instruction
        .byte   NFA|1
        .ascii  " "
        .align  2
IDPMINUS:
        mlit    -2
        goto     IALLOT

; IF       -- adrs   conditional forward branch
; Leaves address of branch instruction 
        .pword  paddr(UNTILC_L)+PFLASH
IF_L:
        .byte   NFA|IMMED|COMPILE|2
        .ascii  "if"
        .align  2
IF_:
        btsc    iflags, #izeroeq
        rcall   IDPMINUS
        rcall   COMMAZEROSENSE
        rcall   ZC
        btss    iflags, #izeroeq
        rcall   NOTC
        bclr    iflags, #izeroeq
        goto    IFC

; THEN     adrs  --        resolve forward branch
        .pword  paddr(IF_L)+PFLASH
THEN_L:
        .byte   NFA|IMMED|COMPILE|4
        .ascii  "then"
        .align  2
THEN_:
        goto    THENC

; ELSE     adrs1 -- adrs2    branch for IF..ELSE
; Leave adrs2 of bra instruction and store bz in adrs1
; Leave adress of branch instruction and FALSE flag on stack
        .pword  paddr(THEN_L)+PFLASH
ELSE_L:
        .byte   NFA|IMMED|COMPILE|4
        .ascii  "else"
        .align  2
ELSE_:
        goto    ELSEC

; BEGIN    -- adrs        target for bwd. branch
        .pword  paddr(ELSE_L)+PFLASH
BEGIN_L:
        .byte   NFA|IMMED|COMPILE|5
        .ascii  "begin"
        .align  2
BEGIN:
        goto   IHERE

; UNTIL    adrs --   Branch bakwards if true
        .pword  paddr(BEGIN_L)+PFLASH
UNTIL_L:
        .byte   NFA|IMMED|COMPILE|5
        .ascii  "until"
        .align  2
UNTIL:
        btsc    iflags, #izeroeq
        rcall   IDPMINUS
        rcall   COMMAZEROSENSE
        rcall   ZC
        btss    iflags, #izeroeq
        rcall   NOTC
        bclr    iflags, #izeroeq
        goto    UNTILC

; AGAIN    adrs --      uncond'l backward branch
;   unconditional backward branch
        .pword  paddr(UNTIL_L)+PFLASH
AGAIN_L:
        .byte   NFA|IMMED|COMPILE|5
        .ascii  "again"
        .align  2
AGAIN:
        goto    AGAINC

; WHILE    addr1 -- addr2 addr1         branch for WHILE loop
; addr1 : address of BEGIN
; addr2 : address where to store bz instruction
        .pword  paddr(AGAIN_L)+PFLASH
WHILE_L:
        .byte   NFA|IMMED|COMPILE|5
        .ascii  "while"
        .align  2
WHILE_:
        rcall   IF_
        goto    SWOP

; REPEAT   addr2 addr1 --     resolve WHILE loop
        .pword  paddr(WHILE_L)+PFLASH
REPEAT_L:
        .byte   NFA|IMMED|COMPILE|6
        .ascii  "repeat"
        .align  2
REPEAT_:
        rcall   AGAIN
        goto    THEN_

; FOR   -- bc-addr bra-addr
        .pword  paddr(REPEAT_L)+PFLASH
FOR_L:
        .byte   NFA|IMMED|COMPILE|3
        .ascii  "for"
        .align  2
FOR:
        mlit    handle(TOR)+PFLASH
        rcall   INLINE_0
        rcall   IHERE
        rcall   UNC
        rcall   FALSE_
        rcall   BRA_
        rcall   IHERE
        goto    SWOP

; NEXT bc-addr bra-addr --

        .pword  paddr(FOR_L)+PFLASH
NEXT_L:
        .byte   NFA|IMMED|COMPILE|4
        .ascii  "next"
        .align  2
NEXT:
        rcall   THENC
        mlit    handle(XNEXT)+PFLASH
        rcall   INLINE_0
        rcall   NC
        rcall   UNTILC
        mlit    handle(RDROP)+PFLASH
        goto    INLINE_0  

; (next) decrement top of return stack
; Works only if inlined.
XNEXT:  
        dec     [--W15], [W15++] ; XNEXT
        return

; leave clear top of return stack
        .pword  paddr(NEXT_L)+PFLASH
LEAVE_L:
        .byte   NFA|INLINE|COMPILE|5
        .ascii  "endit"
        .align  2
LEAVE:
        pop     W0  
        clr     [W15++]
        return

; unnext compile a pop
        .pword  paddr(LEAVE_L)+PFLASH
RDROP_L:
        .byte   NFA|INLINE|COMPILE|5
        .ascii  "rdrop"
        .align  2
RDROP:
        pop     W0
        return

; leave clear top of return stack

; BL      -- char                 an ASCII space
        .pword  paddr(RDROP_L)+PFLASH
BL_L:
        .byte   NFA|INLINE|2
        .ascii  "bl"
        .align  2
BL:
        mlit    0x20
        return

; STATE   -- a-addr             holds compiler state
; In RAM
        .pword  paddr(BL_L)+PFLASH
STATE_L:
        .byte   NFA|5
        .ascii  "state"
        .align  2
STATE:
        mov     state, W0
        mov     W0, [++W14]
        return

; RHERE      -- a-addr          For variables in ram
; In RAM
        .pword  paddr(STATE_L)+PFLASH
RHERE_L:
        .byte   NFA|5
        .ascii  "rhere"
        .align  2
RHERE:
        mlit    dpRAM
        goto    FETCH

; LATEST    -- a-addr           For variables in ram
; In EEPROM
        .pword  paddr(RHERE_L)+PFLASH
LATEST_L:
        .byte   NFA|6
        .ascii  "latest"
        .align  2
LATEST:
        rcall   DOCREATE
        .word   dpLATEST

; S0       -- a-addr      start of parameter stack
        .pword  paddr(LATEST_L)+PFLASH
S0_L:
        .byte   NFA|2
        .ascii  "s0"
        .align  2
S0:
        rcall   DOUSER
        .word   us0

; R0       -- a-addr      start of return stack
        .pword  paddr(S0_L)+PFLASH
R0_L:
        .byte   NFA|2
        .ascii  "r0"
        .align  2
R0:
        rcall   DOUSER
        .word   ur0

; ticks  -- u      system ticks (0-ffff) in milliseconds
        .pword  paddr(R0_L)+PFLASH
TICKS_L:
        .byte   NFA|INLINE|5
        .ascii  "ticks"
        .align  2
TICKS:
        mov     ms_count, W0
        mov     W0, [++w14]
        return

; ms  +n --      Pause for n millisconds
; : ms ( +n -- )     
;   ticks -
;   begin
;     pause dup ticks - 0<
;   until drop ;
;
        .pword  paddr(TICKS_L)+PFLASH
MS_L:
        .byte   NFA|2
        .ascii  "ms"
        .align  2
MS:
        rcall   TICKS
        rcall   PLUS
MS1:    
        rcall   PAUSE
        mov     [W14], W1
        mov     ms_count, W0
        sub     W1, W0, W0         ; time - ticks
        bra     nn, MS1
        sub     W14, #2, W14
        return

 ; WORDS    --          list all words in dict.
        .pword  paddr(MS_L)+PFLASH
WORDS_L:
        .byte   NFA|5
        .ascii  "words"
        .align  2
WORDS:
        rcall   FALSE_
        rcall   CR
        mlit    handle(kernellink)+PFLASH
        rcall   WDS1
        rcall   FALSE_
        rcall   CR
        rcall   LATEST
        rcall   FETCH
WDS1:   mov     [W14++], [W14]      ; dup
        rcall   DOTID
        rcall   SWOP
        inc     [W14], [W14]
        mov     [W14++], [W14]      ; dup
        mlit    7
        rcall   AND
        cp0     [W14--]
        bra     z, WDS2
        mlit    9
        rcall   EMIT
        bra     WDS3
WDS2:   
        rcall   CR
WDS3:
        rcall   SWOP

        rcall   TWOMINUS
        rcall   FETCH
        bclr    [W14], #0           ; Compensate for bug in ASM30 súite
        cp0     [W14]
        bra     nz, WDS1
        sub     W14, #4, W14        ; 2drop
        return

; .S      --           print stack contents
; : .s sp@ s0 @ 2+ begin 2dup < 0= while @+ u. repeat 2drop ;
        .pword  paddr(WORDS_L)+PFLASH
DOTS_L:
        .byte   NFA|2
        .ascii  ".s"
        .align  2
DOTS:
        rcall   SPFETCH
        rcall   S0
        rcall   FETCH
        inc2    [W14], [W14]
DOTS1:
        rcall   TWODUP
        rcall   LESS
        cp0     [W14--]
        bra     nz, DOTS2
        rcall   FETCHPP
        rcall   UDOT
        bra     DOTS1
DOTS2:  
        sub     W14, #4, W14        ; 2drop
        return

; IALLOT   n --    allocate n bytes in ROM
;        .pword  paddr(DOTS_L)+PFLASH
;IALLOT_L:
        .byte   NFA|6
        .ascii  "iallot"
        .align  2
IALLOT:
        rcall   IDP
        goto    PLUSSTORE

; ALLOT   n --    allocate n bytes in current data section
        .pword  paddr(DOTS_L)+PFLASH
ALLOT_L:
        .byte   NFA|5
        .ascii  "allot"
        .align  2
ALLOT:
        rcall   DP
        goto    PLUSSTORE

;   DUMP  ADDR U --       DISPLAY MEMORY
        .pword  paddr(ALLOT_L)+PFLASH
DUMP_L:
        .byte   NFA|4
        .ascii  "dump"
        .align  2
DUMP:
        mlit    0x10
        rcall   USLASH
        push    [W14--]
        bra     DUMP7
DUMP1:  
        rcall   CR
        mov     [W14++], [W14]      ; dup
        mlit    4
        rcall   UDOTR
        mlit    0x3a
        rcall   EMIT
        mlit    0x10
        push    [W14--]
DUMP2:
        rcall   CFETCHPP
        rcall   CELL
        rcall   UDOTR
        dec     [--W15], [W15++] ; XNEXT
        bra     nz, DUMP2
        pop     W0

        mlit    0x10
        rcall   MINUS
        mlit    0x10
        push    [W14--]
DUMP4:  
        rcall   CFETCHPP
        rcall   TO_PR
        rcall   EMIT
        dec     [--W15], [W15++] ; XNEXT
        bra     nz, DUMP4
        pop     W0
DUMP7:
        dec     [--W15], [W15++] ; XNEXT
        bra     c, DUMP1
        pop     W0
        sub     W14, #2, W14
        return

;***************************************************************
; Fcy   ( -- ) The CPU clock ( Fcy in PIC datasheet ) 
        .pword   paddr(DUMP_L)+PFLASH
CPU_CLK_L:
        .byte   NFA|INLINE|3
        .ascii  "Fcy"
        .align  2
FCY_:
        mov     #(FCY/1000), W0
        mov     W0, [++W14]
        return

; C4+ ( n1 -- n2) call C  function unsigned  short func(unsigned short n1);
        .pword  paddr(CPU_CLK_L)+PFLASH
.if C_EXAMPLE == 1
CFOURADD_L:
        .byte   NFA|3
        .ascii  "C4+"
        .align  2
CFOURADD_:
        mov     [W14], W0
        .extern C4add
        call    _C4add
        mov     W0, [W14]
        return

        .pword  paddr(CFOURADD_L)+PFLASH
.endif
FALSE_L:
        .byte   NFA|INLINE|5
        .ascii  "false"
        .align  2
FALSE_:                     ; TOS is 0000 (FALSE)
        clr     [++W14]
        return

        .pword  paddr(FALSE_L)+PFLASH
TRUE_L:
kernellink:
        .byte   NFA|INLINE|4
        .ascii  "true"
        .align  2
TRUE_:                      ; TOS is ffff (TRUE)
        setm    [++W14]
        return

;        .pword  paddr(TRUE_L)+PFLASH
DOTBASE_L:
;        .byte   NFA|2
;        .ascii  "b?"
        .align  2
DOTBASE:
        rcall   BASE
        rcall   FETCH
        mov     [W14], W0
        sub     W0, #0x10, W1
        bra     nz, DOTBASE1
        mov     #'$', W0
        bra     DOTBASEEND
DOTBASE1:
        sub     W0, #0xa, W1
        bra     nz, DOTBASE2
        mov     #'#', W0
        bra     DOTBASEEND
DOTBASE2:
        sub     W0, #0x2, W1
        bra     nz, DOTBASE3
        mov     #'%', W0
        bra     DOTBASEEND
DOTBASE3:
        mov     #'?', W0
DOTBASEEND:
        mov     W0, [W14]
        return

MEMQADDR_N:
        .word   handle(RAM_L)+PFLASH
.ifdef  PEEPROM
        .word   handle(EEPROM_L)+PFLASH
.else
        .word   0
.endif
        .word   handle(FLASH_L)+PFLASH

; -- caddr count    current data space string
;        .word   handle(DOTBASE_L)+PFLASH
MEMQ_L:
;        .byte   NFA|2
;        .ascii  "m?"
        .align  2
MEMQ:
        rcall   CSE
        mlit    handle(MEMQADDR_N)+PFLASH
        rcall   PLUS
        rcall   FETCH
        rcall   CFETCHPP
        mlit    NFL
        goto    AND

        .pword  0
MARKER_L:
lastword:
        .byte   NFA|6
        .ascii  "marker"
        .align  2
MARKER:
        mlit    dpSTART
        mlit    dpSAVE
        mlit    MARKER_LENGTH
        rcall   WMOVE
        rcall   FLASH
        rcall   CREATE
        mlit    dpSAVE
        rcall   HERE
        mlit    MARKER_LENGTH
        rcall   WMOVE
        mlit    MARKER_LENGTH*2
        rcall   ALLOT
        rcall   RAM
        rcall   XDOES
MARKER_DOES:
        rcall   DODOES
        mlit    dpSTART
        mlit    MARKER_LENGTH
        goto    WMOVE

.palign IBUFSIZEL
.ifndef PEEPROM
CONFIG_DATA:
;.pspace IBUFSIZEL*4
KERNEL_END1:
.equ KERNEL_END, KERNEL_END1 + IBUFSIZEL*4
.else
KERNEL_END:
.endif
.end
