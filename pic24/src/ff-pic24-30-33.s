;**********************************************************************
;                                                                     *
;    Filename:      ff-pic24-30-33.s                                  *
;    Date:          26.02.2019                                        *
;    File Version:  5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2019  Mikael Nordman
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
.global __OscillatorFail
.global __AddressError
.global __StackError
.global __MathError
.global __T1Interrupt
.global __U1TXInterrupt
.global __U1RXInterrupt
.global __U2TXInterrupt
.global __U2RXInterrupt
.global __AltOscillatorFail
.global __AltAddressError
.global __AltStackError
.global __AltMathError
.global __AltT1Interrupt
.global __AltU1TXInterrupt
.global __AltU1RXInterrupt
.global __AltU2TXInterrupt
.global __AltU2RXInterrupt

.ifdecl INTTREG
.global __DefaultInterrupt
.endif
.ifdef USB_CDC
.if USB_CDC == 1
.equ IDLE_MODE,0
.endif
.else
.equ USB_CDC,0
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
.equ fIDLE,   14  ; 0=IDLE, 1=BUSY
.equ fwritten,13
.equ edirty,  12  ; eeprom status dirty
.equ fFC2,    11  ; Flow control for UART2
.equ ixoff2,  10  ; XON/XOFF flag for UART2
.equ fLOCK,   9   ; Disable writes to flash and eeprom
.equ tailcall,8   ; Disable tailcall optimisation
.equ WTMO,    7   ; Write timeout active
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
.equ usbuf,        urbuf + RETURN_STACK_SIZE                   ; Parameter stack
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
.if USB_CDC == 1
.include "cdc-bss.s"
.endif
temp:        .space 2
intcon1dbg:  .space 2

ibufl:      .space IBUFSIZEL
ibufh:      .space IBUFSIZEH

.if TX1_BUF_SIZE > 0
txqueue1:
tbuf_len1:   .space 2
tbuf_wr1:    .space 2
tbuf_rd1:    .space 2
tbuf_lv1:    .space 2
tbuf1:       .space TX1_BUF_SIZE+1
.endif

rxqueue1:
rbuf_mask1:
rbuf_len1:   .space 2
rbuf_wr1:    .space 2
rbuf_rd1:    .space 2
rbuf_lv1:    .space 2
rbuf1:       .space RX1_BUF_SIZE+1

.ifdecl BAUDRATE2
.ifdecl _U2RXREG
.if TX2_BUF_SIZE > 0
txqueue2:
tbuf_len2:   .space 2
tbuf_wr2:    .space 2
tbuf_rd2:    .space 2
tbuf_lv2:    .space 2
tbuf2:       .space TX2_BUF_SIZE+1
.endif
rxqueue2:
rbuf_len2:   .space 2
rbuf_wr2:    .space 2
rbuf_rd2:    .space 2
rbuf_lv2:    .space 2
rbuf2:       .space RX2_BUF_SIZE+1
.endif
.endif
index:      .space 2
ibasel:     .space 2
iaddrl:     .space 2
.if WANT_X == 1
ibaseh:     .space 2
iaddrh:     .space 2
.endif
iflags:     .space 2
status:     .space 2        ; 0 = allow CPU idle 
load_acc:   .space 4
load_res:   .space 4
tofloat:    .space 2
ms_count:   .space 2

.if WRITE_METHOD == 2
itmo:          .space 2
.endif
        
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

.ifndef PAIVT
.ifdecl INTTREG
IVECTAB:    .space IVECSIZE*2 ; space for interrupt vectors
.endif
.endif

cse:        .space 2 ; Current data section 0=flash, 1=eeprom, 2=ram
prompt:     .space 2
state:      .space 2 ; Compilation state
upcurr:     .space 2 ; Current USER area pointer
ustart:     .space uareasize ; The operator user area

; Start of code !
.text

; Alternative section for modified linker files
;.section ffcode, code
;  ffcode 0x400 :
;  {
;        *(ffcode);
;  } >program


;;; *************************************
;;; COLD dictionary data
.if USB_CDC == 1
.include "cdc-text.s"
.endif
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

.if OPERATOR_UART == 1
        .word      handle(TX1)+PFLASH
        .word      handle(RX1)+PFLASH
        .word      handle(RX1Q)+PFLASH
.endif
.if OPERATOR_UART == 2
        .word      handle(TX2)+PFLASH
        .word      handle(RX2)+PFLASH
        .word      handle(RX2Q)+PFLASH
.endif
.if OPERATOR_UART == 3
        .word      handle(TXU)+PFLASH
        .word      handle(RXU)+PFLASH
        .word      handle(RXUQ)+PFLASH
.endif
        .word      u0                    ; ULINK
        .word      BASE_DEFAULT          ; BASE
        .word      utibbuf               ; TIB
        .word      handle(OPERATOR_AREA)+PFLASH ; TASK
        .word      0x0000
;;; *************************************************
__OscillatorFail:
__AddressError:
__StackError:
__MathError:
__AltOscillatorFail:
__AltAddressError:
__AltStackError:
__AltMathError:
        mov     INTCON1, W0
        mov     W0, intcon1dbg
        reset

__T1Interrupt:
__AltT1Interrupt:
; No nested interrupts, T1 interrupt must the first interrupt to be enabled
        bset    INTCON1, #NSTDIS
        push.s
        bclr    IFS0, #T1IF
        inc     ms_count

.if IDLE_MODE == 1
.if CPU_LOAD == 1
.ifdecl TSIDL
.ifdecl TMR3
        mov     TMR3, W0
        clr     TMR3
        
        add     load_acc
        clr     W0
        addc    load_acc+2
        
        cp0.b   ms_count
        bra     nz, RETFIE_T1_0
        mov     load_acc, W0
        mov     W0, load_res
        mov     load_acc+2, W0
        mov     W0, load_res+2
        clr     load_acc
        clr     load_acc+2
RETFIE_T1_0:
.endif
.endif
.endif
.endif
        pop.s
        retfie

__U1RXInterrupt:
__AltU1RXInterrupt:
        push.s
        push    TBLPAG
        lnk     #6                    ; 3 cell parameter stack
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
        btsc    iflags, #ixoff1
        bra     U1_SKIP_FC_2
        btsc    U1STA, #UTXBF
        bra     __U1RXInterrupt2
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
        ulnk
        pop     TBLPAG
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

.if TX1_BUF_SIZE > 0
__U1TXInterrupt:
__AltU1TXInterrupt:
        push.s
        push    TBLPAG
        lnk     #4                      ; 2 cell parameter stack
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
.endif

.ifdecl BAUDRATE2
.ifdecl _U2RXREG
__U2RXInterrupt:
__AltU2RXInterrupt:
        push.s
        push    TBLPAG
        lnk     #6             ; 3 cell parameter stack
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
        btsc    iflags, #ixoff2
        bra     U2_SKIP_FC_2
        btsc    U2STA, #UTXBF
        bra     __U2RXInterrupt2
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

.if TX2_BUF_SIZE > 0
__U2TXInterrupt:
__AltU2TXInterrupt:
        push.s
        push    TBLPAG
        lnk     #4        ; 2 Cell parameter stack
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
.endif
;*******************************************************************
.ifdef PAIVT
__DefaultInterrupt:
        bra     __AddressError
.else
.ifdecl INTTREG
__DefaultInterrupt:
.ifdef ALTIVT
        btss    INTCON2, #ALTIVT
        bra     __AddressError
.endif
        push.s                   ; Handle interrupt via lookup table
        mov     INTTREG, W0
        and     #0xff, W0
        sl      W0, W1
        mov     #IVECTAB, W0
        add     W0, W1, W2
        mov     [W2], W0
        goto    W0
.endif
.endif
; *******************************************************************
; ibufmask = 0xfffc0 or 0xffc00 or 0xff800
; ibuflen  = 0x00040 or 0x00400 or 0x00800
iupdatebuf:
;if (ibase != (iaddr&ibufmask)) // ibufmask = 0xffffc0 or fffc00 
;   if (idirty)
;       writebuffer_to_imem
;   endif
;   fillbuffer_from_imem
;   ibase = iaddr&ibufmask
;endif
.if WANT_X == 1
        mov     iaddrh, W0
        cp      ibaseh
        bra     nz, iupdatebuf0
.endif
        mov     iaddrl, W0
        mov     #IBUFMASK, W1
        and     W0, W1, W0
        cp      ibasel
        bra     nz, iupdatebuf0
        return

iupdatebuf0:
        rcall   IFLUSH
        mov     iaddrl, W0
        mov     #IBUFMASK, W1
        and     W0, W1, W0
        mov     W0, ibasel
.if WANT_X == 1
        mov     iaddrh, W0
        mov     W0, ibaseh
        mov     W0, TBLPAG
.endif
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
        bclr     iflags, #fwritten
        clr      TBLPAG
        return

wait_silence:
        rcall   LOCKED
.if OPERATOR_UART ==1
.if FC1_TYPE == 1
        btss    U1STA, #TRMT
        bra     wait_silence
        mov     #XOFF, W2
        mov     W2, U1TXREG
        bset    iflags, #ixoff1
.endif
.if FC1_TYPE == 2
        bset    U1RTSPORT, #U1RTSPIN
.endif
.endif
.if OPERATOR_UART == 2
.if FC2_TYPE == 1
        btss    U2STA, #TRMT
        bra     wait_silence
        mov     #XOFF, W2
        mov     W2, U2TXREG
        bset    iflags, #ixoff2
.endif
.if FC2_TYPE == 2
        bset    U2RTSPORT, #U2RTSPIN
.endif
.endif
wbtil:
        bclr    iflags, #istream
        ; The delay here should be 10 character times long
        ; times = Fcy/baud*40
        mov     #(FCY/BAUDRATE1), W2     ;  This loop takes about 5 milliseconds @ 27 Mips
wbtil2:
        repeat  #100
        nop
        btsc    iflags, #istream    ; Check for UART  activity.
        bra     wbtil               ; 5 cycles per round
        dec     W2, W2              ;
        bra     nz, wbtil2          ;
        return
;***********************************************************
wbti_init:
        mov.w   W0, NVMCON
        mov.w   #ibufl, W0 ; Low word flash buffer in ram
        mov.w   #ibufh, W1 ; High byte buffer
.if WANT_X == 1
        mov.w   ibaseh, W2
        mov.w   W2, TBLPAG
.endif
        mov.w   ibasel, W2
        mov.w   #IBUFLEN2, W4
        tblwtl  W2, [W2]          ; Set page address
        return

write_buffer_to_imem:
;; Loop here until there are no more characters has been received for a while
;; from the UART.
;; The assumption is that the serial line is silent then.
        rcall   wait_silence

write_buffer_to_imem_again:
.if DEBUG_FLASH == 1
        btss    U1STA, #TRMT
        bra     $-2
        mov     #'F', W2
        mov     W2, U1TXREG
.endif
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Erase The Flash
        mov.w   #FLASH_ERASE, W0
        rcall   wbti_init
.ifdecl PIC2433E
        mov     W2, NVMADR
        mov     TBLPAG, W2
        mov     W2, NVMADRU
.endif
        rcall   EWENABLE0         ; Now the flash row has been erased.

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Write The Flash
        mov.w   #FLASH_WRITE, W0
        rcall   wbti_init
.ifdecl PIC2433E
        mov     #0xfa, W5
        mov     W5, TBLPAG
        bra     wbtil31
.endif
wbtil3:
.ifdecl PIC2433E
        inc2    NVMADR
        inc2    NVMADR
wbtil31:
        clr     W2
.endif
        mov.w   #IBUFLEN1, W3
wbtil4:
        tblwth.b  [W1++], [W2]
        tblwtl.w  [W0++], [W2++]
        dec     W3, W3
        bra     nz, wbtil4
        rcall   EWENABLE0   ; Now the flash row has been written.
        dec     W4, W4
        bra     nz, wbtil3  ; write more rows for big flashblocks

.ifdecl PIC2433E
        mov     NVMADRU, W0
        mov     W0, TBLPAG
.endif

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;; Verify The Flash
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
        setm      ibasel       ; Now the flash row has been verified
.if WANT_X == 1
        setm      ibaseh       ; Now the flash row has been verified
.endif
        clr       TBLPAG
        return

verify_imem_2:
        reset

; LITERAL  x --           append numeric literal as inline code
        .pword   0
9:
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
.ifdef DPS_ADDR
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "dps"
        .align 2
        mlit    (DPS_ADDR+(IBUFSIZEL*4))&0xffff
        mlit    (DPS_ADDR+(IBUFSIZEL*4))>>16
        return
.endif
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  ">a"
        .align 2
TO_A:
        mov     [W14--], W11
        return

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "a>"
        .align 2
A_FROM:
        mov     W11, [++W14]
        return

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|1
        .ascii  "?"
        .align 2
        rcall   FETCH
        goto    UDOT

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "idle"
        .align 2
IDLE_:
        bset     iflags, #fIDLE
        return
        
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "busy"
        .align 2
BUSY_:
        bclr     iflags, #fIDLE
        return
        
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "load"
        .align 2
LOAD_:
        mov     #FCY/3126, W2
        mov     load_res+2, W1
        mov     load_res, W0
        repeat  #17
        div.ud  W0, W2
        mov     W0, [++W14]
        return
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "empty"
        .align 2
EMPTY:
.ifdef PEEPROM
        mlit    handle(COLDLIT)+PFLASH
        mlit    dp_start
        mlit    coldlitsize
        rcall   WMOVE
        clr     intcon1dbg
.else
        rcall   DP_COLD
.endif
        rcall   DP_TO_RAM
        return

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|6
        .ascii  "forget"
        .align 2
        rcall   BL
        rcall   WORD
        rcall   LATEST
        rcall   FETCH
        rcall   findi
        rcall   QABORTQ
        rcall   CFATONFA
        rcall   TWOMINUS
        rcall   DUP
        rcall   FETCH
        rcall   QABORTQ
        rcall   DUP
        rcall   FLASH
        rcall   DP
        rcall   STORE
        rcall   FETCH
        rcall   LATEST
        rcall   STORE
        goto    RAM


        .pword   paddr(9b)+PFLASH
9:
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
.if USB_CDC == 1
        goto    WARM
.else
        reset
.endif
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
        mov     #intcon1dbg+2, W14  ; Dont overwrite INTCONDBG
        mov     #PFLASH, W1
FILL_RAM:
        mov.w   W0, [W14++]
        cp      W14, W1
        bra     nz, FILL_RAM
        mov     #usbuf0, W14
        setm    ibasel
.if WANT_X == 1
        setm    ibaseh
.endif
        clr     iflags
.ifndef PAIVT
.ifdecl INTTREG
        mov     #IVECTAB, W0
        mov     #IVECSIZE, W1
        mov     #handle(__DefaultInterrupt), W2
WARM_FILL_IVEC:
        mov     W2, [W0++]
        dec     W1, W1
        bra     nz, WARM_FILL_IVEC
.endif
.endif
;        setm    PMD1
;        setm    PMD2
.ifdecl PMD3
;        setm    PMD3
.endif
.ifdecl PMD4
;        clr    PMD4      ; Don't set PMD4, it may disable the eeprom
.endif
.ifdecl PMD5
;        setm    PMD5
.endif
.ifdecl AD1PCFGL
        setm    AD1PCFGL
.endif
.ifdef ANSELA
        clr      ANSELA
.endif
.ifdef ANSELB
        clr      ANSELB
.endif
.ifdef ANSELC
        clr      ANSELC
.endif
.ifdef ANSELD
        clr      ANSELD
.endif
.ifdef ANSELE
        clr      ANSELE
.endif
.ifdef ANSELG
        clr      ANSELG
.endif
.ifdecl ANSA
        clr     ANSA
.endif
.ifdecl ANSB
        clr     ANSB
.endif
.ifdecl ANSC
        clr     ANSC
.endif
WARM_0:
; Init the serial TX buffer
.if TX1_BUF_SIZE > 0
        rcall   U1TXQUEUE
        rcall   CQUEUEZ
.endif
; Init the serial RX buffer
        rcall   U1RXQUEUE
        rcall   CQUEUEZ
        
.ifdecl BAUDRATE2
.ifdecl _U2RXREG
; Init the serial TX buffer
.if TX2_BUF_SIZE > 0
        rcall   U2TXQUEUE
        rcall   CQUEUEZ
.endif
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
.ifdecl TMR3
.ifdecl TSIDL
; Configure CPU load counter timer3
        bclr    PMD1, #T3MD
        mov     #0xA010, W0    ; Stop timer3 in idle mode, prescaler = 8
        mov     W0, T3CON
.endif
.endif
.endif
.endif
.if USB_CDC == 1
        rcall   USB_ON
.endif
; Enable T1 interrupt
        bset    IEC0, #T1IE

;;;; Initialise the UART 1
.if FC1_TYPE == 2
        bclr    U1RTSTRIS, #U1RTSPIN
        bclr    U1RTSPORT, #U1RTSPIN
.endif
.ifdecl RPINR18
        mov     #OSCCONL, W0
        mov.b   #0x46, W1
        mov.b   #0x57, W2
        mov.b   W1, [W0]
        mov.b   W2, [W0]
        bclr.b  OSCCONL, #IOLOCK
        
        mov     #RPINR18VAL, W0
        mov     W0, RPINR18

; PIC2433HJFJ
.ifdecl U1TXPIN
        mov     #0x0003, W0         ; U1TX
        mov.b   WREG, RPOR0+U1TXPIN
.endif
.ifdecl U1_RPO_REGISTER
; PIC2433EP
        mov     #U1_RPO_VALUE, W0         ; U1TX
        mov     W0, U1_RPO_REGISTER
.endif

.endif
        bclr    PMD1, #U1MD

.ifdecl USE_ALTERNATE_UART_PINS
.if  (USE_ALTERNATE_UART_PINS == 1)
        bset    U1MODE, #ALTIO
.endif
.endif
.ifdecl UTXISEL1
        bset    U1STA, #UTXISEL1
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
.if TX1_BUF_SIZE > 0
        bset    IEC0, #U1TXIE
.endif
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
; PIC2433HJFJ
.ifdecl U1TXPIN
        mov     #0x0003, W0         ; U1TX
        mov.b   WREG, RPOR0+U1TXPIN
.endif
.ifdecl U1_RPO_REGISTER
; PIC2433EP
        mov     #U2_RPO_VALUE, W0         ; U1TX
        mov     W0, U2_RPO_REGISTER
.endif
.endif
       bclr    PMD1, #U2MD
.ifdecl UTXISEL1
        bset    U2STA, #UTXISEL1
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
.if TX2_BUF_SIZE > 0
        bset    IEC1, #U2TXIE
.endif
.endif
.endif

; Init the warm literals
        mlit    handle(WARMLIT)+PFLASH
        mlit    cse
        mlit    warmlitsize
        rcall   WMOVE

                ; Wait 10 ms for UARTs to reset
        mlit    10
        rcall   MS

; Check if EEPROM INIT is needed
.ifdef PEEPROM
        mlit    dp_start
        rcall   FETCH
.else
        rcall   EECHECK
.endif
        inc     [W14--], W0
        bra     nz, WARM_WARM
        rcall   EMPTY
WARM_WARM:
        rcall   DP_TO_RAM

.if WRITE_METHOD == 2
        rcall   DP_PUSH
.endif

; Display INTCON1 and RCON restart reason
        btsc    intcon1dbg, #STKERR
        rcall   DOEMIT
        .word   'O'          ; NOP when executed
RQ_DIV0:
        btsc    intcon1dbg, #MATHERR
        rcall   DOEMIT
        .word   'M'
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
        .byte   32
;                1234567890123456789012345678901234567890
        .ascii  " FlashForth 5 PIC24 26.02.2019\r\n"
        .align 2
        rcall   TYPE
.if OPERATOR_UART == 1
.if FC1_TYPE == 1
        mlit    XON
        rcall   EMIT
.endif
.endif
.if OPERATOR_UART == 2
.if FC2_TYPE == 1
        mlit    XON
        rcall   EMIT
.endif
.endif; TURNKEY
        rcall   TURNKEY
        cp0     [W14--]
        bra     z, STARTQ2

        rcall   XSQUOTE
        .byte   3
        .ascii  "ESC"
        .align 2
        rcall   TYPE
        mlit    TURNKEY_DELAY
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


        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|7
        .ascii  "turnkey"
        .align 2
TURNKEY:
        rcall   VALUE_DOES
        .word   dpSTART

; PAUSE  20 cycles, 5us@16MHz dsPIC30F 2.5 us for 33F and 24F
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "pause"
        .align 2
PAUSE:
        clrwdt
.if USB_CDC == 1
        rcall   USBDriverService
        cp0     ep2icount
        bra     z, PAUSE_USB_CDC_END
        mov     ep2itmo, WREG
        subr    ms_count, WREG
        bra     nn, PAUSE_USB_CDC_END
        rcall   TXU_SEND
PAUSE_USB_CDC_END:
.endif
.if WRITE_METHOD == 2
        btss    iflags, #WTMO
        bra     PAUSE2
        mov     ms_count, W0 ; itmo - ms_count
        sub     itmo, WREG   ; itmo - w0 -> W0
        bra     nn, PAUSE2
        bclr    iflags, #WTMO
        rcall   IFLUSH
        btsc    iflags, #edirty
        rcall   DP_TO_EEPROM
PAUSE2:
.endif
.if IDLE_MODE == 1
        btss    iflags, #fIDLE
        bra     PAUSE_BUSY
        mov     #u0, W0        ; IDLE only in operator task.
        cp      upcurr
        bra     nz, PAUSE_BUSY
.if CPU_LOAD_LED == 1
.if CPU_LOAD_LED_POLARITY == 0
        bset    CPU_LOAD_PORT, #CPU_LOAD_BIT
.else
        bclr    CPU_LOAD_PORT, #CPU_LOAD_BIT
.endif
.endif
        pwrsav  #1             ; Go to IDLE mode to save power.
PAUSE_BUSY:
.if CPU_LOAD_LED == 1
        bclr    CPU_LOAD_TRIS, #CPU_LOAD_BIT
.if CPU_LOAD_LED_POLARITY == 0
        bclr    CPU_LOAD_PORT, #CPU_LOAD_BIT
.else
        bset    CPU_LOAD_PORT, #CPU_LOAD_BIT
.endif
.endif
.endif
        mov     upcurr, W0
        mov     W14, [W0+ussave]    ; Save SP W14
        mov     W15, [W0+ursave]    ; Save RP W15
        mov     W13, [W0+upsave]    ; Save P pointer
        mov     [W0+ulink], W0      ; Set UP

        disi    #5
        setm    SPLIM               ; Disable Return Stack overflow protection
        mov     W0, upcurr
        mov     [W0+upsave], W13    ; Restore P pointer
        mov     [W0+ursave], W15    ; Restore RP
        mov     [W0+us0], W1        ; Set SPLIM
        mov     W1, SPLIM           ;
        mov     [W0+ussave], W14    ; Restore SP W14
        return

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "cwd"
        .align 2
CWD:
        clrwdt
        return
.ifdef PAIVT
; INT/  ( intnumber -- )
; Reset the AIVT interrupt vector to the defaul IVT value
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "int/"
        .align 2
        mov     [W14], W2
        sl      W2, W0
        add     #4, W0
        and     #(PAIVT-1), W0
        tblrdl  [W0], [W14]
        rcall   XA_FROM
        mov     W2, [++W14]
        goto    INTERRUPT_STORE
.endif
; INT!  ( xt intnumber -- ) intnumber 8..
; Store interrupt vector in alternate interrupt vector table
; Stored directly in Flash on the 30F series     intnumber 0..61
; Stored directly in Flash on the 24FK series     intnumber 0..80
; Stored in ram revector table in 24,33 series   intnumber 0..83

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "int!"
        .align 2
INTERRUPT_STORE:
.ifdef IVT
        rcall   IVT
.endif
.ifdef PAIVT
        mov     [W14--], W3
        rcall   TO_XA
        sl      W3, W0
        add     #4, W0
        and     #(PAIVT-1), W0
        mov     #PAIVT+PFLASH, W1
        add     W1, W0, W0
        clr     W3             ; hibyte
        rcall   ISTORE_RAW     ; IntNo address in W0, xt on stack
        rcall   IFLUSH
.else
.ifdecl INTTREG
        mov     [W14--], W3
        rcall   TO_XA
        sl      W3, W1
        mov     #IVECTAB, W0
        add     W0, W1, W0
        mov     [W14--], [W0]
.endif
.endif
        return

; IVT  ( -- )  Use the normal interrupt vector table
        .pword   paddr(9b)+PFLASH
.ifdef ALTIVT
9:
        .byte   NFA|INLINE|3
        .ascii  "ivt"
        .align  2
IVT:
        bclr    INTCON2, #ALTIVT
        return

; AIVT ( -- ) Use the alternate interrupt vector table
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|4
        .ascii  "aivt"
        .align  2
AIVT:
        bset    INTCON2, #ALTIVT
        return

; [i ( -- ) enter the interrupt context
        .pword   paddr(9b)+PFLASH
.endif
9:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "[i"
        .align  2
BRACKETI:
.ifdef  PAIVT
        push.s          ; W0...W3
.endif
        push.d  W4
        push.d  W6
        push    TBLPAG  ; Used by eeprom access
        push    W13     ; Preg        
        push    RCOUNT  ; used by repeat
        lnk     #INTERRUPT_STACK_FRAME
        return

; i] ( -- ) exit the interrupt context
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "i]"
        .align  2
IBRACKET:
        ulnk
        pop     RCOUNT  ; used by repeat
        pop     W13     ; Preg
        pop     TBLPAG  ; Used by eeprom access
        pop.d   W6
        pop.d   W4
.ifdef PAIVT
        pop.s           ; W0...W3
.endif
        return

; di ( -- ) disable interrupts
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "di"
        .align  2
DI:
.ifdecl GIE
        bclr    INTCON2, #GIE
.else
        push    SR
        mov.b   #0xe0, W0
        ior.b   SRL
.endif
        return

; ei ( -- ) enable interrupts
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "ei"
        .align  2
EI:
.ifdecl GIE
        bset    INTCON2, #GIE
.else
        pop     SR
.endif
        return

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|8
        .ascii  "operator"
        .align  2
OPERATOR:
        rcall   DOCREATE
        .pword  paddr(OPERATOR_AREA)+PFLASH
OPERATOR_AREA: 
        .word   ustart-us0      ; user pointer
        .word   UADDSIZE
        .word   RETURN_STACK_SIZE
        .word   PARAMETER_STACK_SIZE
        .word   TIB_SIZE+HOLD_SIZE


;  rcall, ( rel-addr -- )
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|6
        .ascii  "rcall,"
        .align  2
RCALL_:
        asr     [W14], [W14]        ; 2/
        mlit    #7
        rcall   AS_COMMA
        return

;  return, ( -- )
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|7
        .ascii  "return,"
        .align  2
RETURN_:
        rcall   FALSE_
        mlit    #6
        rcall   AS_COMMA
        return

;  retfie, ( -- )
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|7
        .ascii  "retfie,"
        .align  2
RETFIE_:
        mlit    #0x4000
        mlit    #6
        rcall   AS_COMMA
        return

;  bra, ( cc rel-addr -- )
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "bra,"
        .align  2
BRA_:
        asr     [W14], [W14]        ; 2/
        rcall   SWOP
        mov     #0x30, W0
        ior     W0, [W14],[W14]
        rcall   AS_COMMA
        return

;  as0 ( bit ram-addr -- u ) 
; : bset, swap dup >r 8 u/ + r> $7 and #12 lshift or ;
        .pword   paddr(9b)+PFLASH
9:
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
        mov     #0x7, W0
        and     W0, [W14], [W14]
        mlit    13
        rcall   LSHIFT
        rcall   OR
        return

; bclr, ( bit ram-addr -- ) clr bit in ram
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "bclr,"
        .align  2
BCLR_:
        rcall   AS0
        mlit    0xa9
        rcall   AS_COMMA
        return
                    
; bset, ( bit ram-addr -- ) set bit in ram
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "bset,"
        .align  2
BSET_:
        rcall   AS0
        mlit    0xa8
        rcall   AS_COMMA
        return
; btst, ( bit ram-addr -- ) test bit in ram -> STATUS, Z bit
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "btst,"
        .align  2
BTST_:
        rcall   AS0
        mlit    0xab
        rcall   AS_COMMA
        return

; btsc, ( bit ram-addr -- ) bit test f, skip if clear
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "btsc,"
        .align  2
BTSC_:
        rcall   AS0
        mlit    0xaf
        rcall   AS_COMMA
        return

; btss, ( bit ram-addr -- ) bit test f, skip if set
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "btss,"
        .align  2
BTSS_:
        rcall   AS0
        mlit    0xae
        rcall   AS_COMMA
        return

; cf! ( datal datah addr -- )
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "cf!"
        .align  2
CF_STORE:
        rcall   SWOP
        mov     [W14--], W3
        rcall   CFISTORE
        return

; cf@ ( addr -- datal datah)
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "cf@"
        .align  2
CF_FETCH:
        rcall   FETCH
        mov     W3, [++W14]
        return
        
; as, ( datal datah -- )
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "as,"
        .align  2
AS_COMMA:
        mov     [W14--], W0
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
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "cf,"
        .align  2
COMMAXT:
        rcall   IHERE
        rcall   MINUS
        dec2    [W14], [W14]
        rcall   RCALL_
        return

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|6
        .ascii  "iflush"
        .align  2
IFLUSH:
        btsc    iflags, #idirty
        bra     write_buffer_to_imem
        return
.if WRITE_METHOD == 2
BFLUSH:
        mov     [W14], W2
        mov     #PFLASH, W0
        sub     W2, W0, W2
        mov     #IBUFMASK, W1
        and     W2, W1, W0
        cp      ibasel   ; ibasel - address
        bra     z, IFLUSH  ; FLUSH if execute on the current flash page
        mov     #IBUFSIZEL, W1
        add     W0, W1, W0
        cp      ibasel   ; ibasel - address
        btss    iflags, #fwritten
        bra     z, IFLUSH  ; FLUSH if execute on the previous flash page
        return
.endif
        
; data addr IC! Address is in W0
ICSTORE:
        rcall   ISTORE_ADDRCHK
        setm    W3               ; hibyte
        mov     #PFLASH, W1
        sub     W0, W1, W0
        mov     W0, iaddrl       ; W0 = addr, iaddrl = addr
.if WANT_X == 1
        clr     iaddrh
.endif
        rcall   ISTORE_SUB
        mov.b   W1, [W0]
        return

; data addr I!  Address is in W0
CFISTORE:
        mov     [W14--], W0
        bra     ISTORECF
ISTORE:
        setm    W3
ISTORECF:
        rcall   ISTORE_ADDRCHK
ISTORE_RAW:
        mov     #PFLASH, W1
        sub     W0, W1, W0
        mov     W0, iaddrl       ; W0 = addr, iaddrl = addr
.if WANT_X == 1
        clr     iaddrh
.endif
        rcall   ISTORE_SUB
        mov     W1, [W0]
ISTORE1:
        return

ISTORE_SUB:
        push    W3
        rcall   iupdatebuf      ; uses w3
        pop     W3
        mov     iaddrl, W0
        
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
.if WRITE_METHOD == 2
SET_FLASH_W_TMO:
        btsc    iflags, #idirty
        bra     SET_FLASH_W_TMO2
        btsc    iflags, #edirty
        bra     SET_FLASH_W_TMO2
        return
SET_FLASH_W_TMO2:
        bset    iflags, #WTMO
        mov     ms_count, W3
        add     #WRITE_TIMEOUT, W3
        mov     W3, itmo
.endif
        return

.if WRITE_METHOD == 2
SET_EEPROM_W_TMO:
        bset    iflags, #edirty
        bra     SET_FLASH_W_TMO
.endif
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
;        clr     TBLPAG
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
;        clr      TBLPAG
        rcall    IFETCH_INIT
        cp       W1, W2
        bra      NZ, PFETCH1
        mov      #IBUFSIZEL-1, W1
        and      W1, W0, W0
        lsr      W0, #1, W2
        mov      #ibufl, W1
        add      W1, W0, W1
        mov      [W1],[W14]

        mov      #ibufh, W1
        add      W1, W2, W2
        clr      W3
        mov.b    [W2], W3       ; hibyte
        return
        
IFETCH_INIT:
        mov      #PFLASH, W1
        sub      W0, W1, W0
        mov      ibasel, W1
        mov      #IBUFMASK, W2
        and      W2, W0, W2
        return

EWENABLE:
        mov     W1, NVMCON
EWENABLE0:
.ifdecl PIC2433E
        bclr    INTCON2, #GIE
.else
        disi    #7
.endif
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
.ifdecl PIC2433E
        bset    INTCON2, #GIE
.endif
        return

.ifdef PEEPROM
ESTORE:
        rcall   wait_silence
        mov.w   #0x7f, W1
        mov     W1, TBLPAG
.ifdef EEPROM_ERASE
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
        mov.w   #0x7f, W1
        mov.w   W1, TBLPAG
        nop
        tblrdl  [W0], W1
        nop
        mov     W1, [W14]
        clr     TBLPAG
        return
ECFETCH:
        mov     W0, W2
        bclr    W0, #0
        rcall   EFETCH
        btsc    W2, #0
        swap.w  W1
        and.w   #0xff, W1
        mov.w   W1, [W14]
        return
.else
;;; Only for TURNKEY, DP_FLASH, DP_RAM, LATEST !
;;;
;;; Read the last non-FFFF word from one flash block
;;; No size check, just finds the last non-FF entry.
; ( blockaddr -- data )
;        dw      link
;link    set     $
;        db      NFA|3,"ee@"
EEREAD:
        mov     [W14], W0
        mov     #IBUFSIZEL, W1
        add     W0, W1, W0      ; W0 = endof flash page.
        mov     #DPS_PAGE, W1
        mov     W1, TBLPAG
EEREAD1:
        tblrdl  [--W0], W1
.ifdef FLASH_WRITE_DOUBLE
        tblrdl  [--W0], W1  ; The first double word is used
.endif
        inc     W1, W1
        bra     z, EEREAD1
        dec     W1, W1
        mov     W1, [W14]
        clr     TBLPAG
        return
EECHECK:
        mov     #handle(DPS_BASE), W0
        mov     #DPS_PAGE, W1
        mov     W1, TBLPAG
        tblrdl  [W0], W1
        mov     W1, [++W14]
        clr     TBLPAG
        return
EEINIT:
        rcall   LOCKED
        rcall   EEERASE
        bra     EEWRITE

;;; Write of word to first free (lowword=FFFF) location in a flash block
;;; ( data blockaddr -- )
;        dw      link
;link    set     $
;        db      NFA|3,"ee!"
EEWRITE:
        rcall   wait_silence
        mov     #DPS_PAGE, W1
        mov     W1, TBLPAG
.ifdecl PIC2433E
        mov     W1, NVMADRU
.endif
        mov     [W14], W0
.ifdef FLASH_WRITE_DOUBLE
        mov     #IBUFSIZEH/2, W2
.else
        mov     #IBUFSIZEH, W2
.endif
EEWRITE1:
        tblrdl  [W0++], W1
        inc     W1, W1
        bra     nz, EEWRITE2
        dec2    W0, W0
        dec2    W14, W14
        bra     EEWRITE3
EEWRITE2:
.ifdef FLASH_WRITE_DOUBLE
        tblrdl  [W0++], W1   ; discard second word
.endif
        dec     W2, W2
        bra     nz, EEWRITE1
        rcall   EEERASE        ; ( blockstart -- blockstart)
        mov     [W14--], W0
EEWRITE3:
        mov     #FLASH_WRITE_SINGLE, W1
        mov     W1, NVMCON
.ifdecl PIC2433E
        mov     W0, NVMADR
        mov     #0xfa, W1
        mov     W1, TBLPAG
        clr     W0
.endif
        mov     [W14--], W3
        tblwtl  W3, [W0]
        rcall   EWENABLE0
        clr     TBLPAG
EEVERIFY:
        ; TODO
        return

; block-addr -- block-addr
EEERASE:
        rcall   wait_silence
.if DEBUG_FLASH == 1
        btss    U1STA, #TRMT
        bra     $-2
        mov     #'R', W2
        mov     W2, U1TXREG
.endif
        mov     #FLASH_ERASE, W0
        mov     W0, NVMCON
.ifdecl PIC2433E
        mov     #DPS_PAGE, W1
        mov     W1, NVMADRU
        mov     [W14], W1
        mov     W1, NVMADR
.else
        mov     #DPS_PAGE, W1
        mov     W1, TBLPAG
        mov     [W14], W0
        tblwtl  W0, [W0]
.endif
        rcall   EWENABLE0
        clr     TBLPAG
        return
.endif

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "fl+"
        .align  2
        bclr    iflags, #fLOCK
        return

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "fl-"
        .align  2
        bset    iflags, #fLOCK
        return
        
LOCKED:
        btss    iflags, #fLOCK
        return
        bset    INTCON1, #ADDRERR

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "u1+"
        .align  2
        bclr    iflags, #fFC1
        return

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "u1-"
        .align  2
        bset    iflags, #fFC1
        return

        .pword   paddr(9b)+PFLASH
.ifdecl BAUDRATE2
.ifdecl _U2RXREG
9:
        .byte   NFA|3
        .ascii  "u2+"
        .align  2
        bclr    iflags, #fFC2
        return

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "u2-"
        .align  2
        bset    iflags, #fFC2
        return

        .pword   paddr(9b)+PFLASH
.endif
.endif
9:
        .byte   NFA|1
        .ascii  "!"
        .align  2
STORE:
        mov.w   [W14--], W0
        mov.w   #PFLASH, W1
        cp      W0, W1
.ifdef PEEPROM
        bra     GEU, STORE1
.else
        bra     GEU, ISTORE
.endif

        mov.w   [W14--], [W0]
        return
STORE1:
.ifdef PEEPROM
        mov.w   #PEEPROM, W1
        cp      W0, W1
        bra     GEU, ESTORE
        bra     ISTORE
.endif

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "c!"
        .align  2
CSTORE:
        mov.w   [W14--], W0
        mov.w   #PFLASH, W1
        cp      W0, W1
.ifdef PEEPROM
        bra     GEU, CSTORE1
.else
        bra     GEU, ICSTORE
.endif
        mov.b   [W14], [W0]
        mov.w   [W14--], W0
        return
CSTORE1:
.ifdef PEEPROM
        mov.w   #PEEPROM, W1
        cp      W0, W1
        bra     GEU, ECSTORE
        bra     ICSTORE
.endif

        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|1
        .ascii  "@"
        .align  2
FETCH:
        mov.w   [W14], W0
        mov.w   #PFLASH, W1
        cp      W0, W1
.ifdef PEEPROM
        bra     GEU, FETCH1
.else
        bra     GEU, IFETCH
.endif
        mov.w   [W0], [W14]
        return
FETCH1:
.ifdef PEEPROM
        mov.w   #PEEPROM, W1
        cp      W0, W1
        bra     GEU, EFETCH
        bra     IFETCH
.endif


       .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "c@"
        .align  2
CFETCH:
        mov.w   [W14], W0
        clr     [W14]
        mov.w   #PFLASH, W1
        cp      W0, W1
.ifdef PEEPROM
        bra     GEU, CFETCH1
.else
        bra     GEU, ICFETCH
.endif
        mov.b   [W0], [W14]
        return
CFETCH1:
.ifdef PEEPROM
        mov.w   #PEEPROM, W1
        cp      W0, W1
        bra     GEU, ECFETCH
        bra     ICFETCH
.endif
.if WANT_X == 1
;;; Xtended Fetch from Flash Memory
;;; xu@ ( addrl addrh -- x c )
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "x@"
        .align  2
XEFETCH:
        push    TBLPAG
        mov     [W14--], W0
        mov     W0, TBLPAG
        mov     [W14], W0
        tblrdl  [W0], [W14++]
        clr     [W14]
        tblrdh.b [W0], [W14]
        pop     TBLPAG
        return

;;; Xtended Store to Flash Memory
;;; x! ( x c addrl addrh -- )
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "x!"
        .align  2
XESTORE:
        mov     [W14--], W1
        mov     W1, iaddrh
        mov     [W14--], W0
        mov     W0, iaddrl
        mov     #handle(KERNEL_END), W0
        mov     [W14--], W3               ; W3 has upper flash byte
        cp0     iaddrh
        bra     NZ, XUSTORE1
        cp      iaddrl
        bra     GTU, XUSTORE1
        bset    INTCON1, #ADDRERR ; Kernel write protection
XUSTORE1:
        rcall   ISTORE_SUB
        mov     W1, [W0]
        return
.endif
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "mset"
        .align  2
MSET:
        mov     [W14--], W0
        mov.w   [W14--], W1
        ior.w   W1, [W0],[W0] 
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "mclr"
        .align  2
MCLR:
        mov     [W14--], W0
        com.w   [W14--], W1
        and.w   W1, [W0],[W0] 
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "mtst"
        .align  2
MTST:
        mov     [W14--], W0
        mov     [W14--], W1
        and.w   W1, [W0], W0
        mov     W0, [++W14] 
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|6
        .ascii  "lshift"
        .align  2
LSHIFT:
        mov.w   [W14--], W0
        mov     [W14--], W1
        sl      W1, W0, W0
        mov     W0, [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|6
        .ascii  "rshift"
        .align  2
RSHIFT:
        mov.w   [W14--], W0
        mov     [W14--], W1
        lsr     W1, W0, W0
        mov     W0, [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
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
        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "emit"
        .align  2
EMIT:
        rcall   UEMIT
        goto    FEXECUTE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "key"
        .align  2
KEY:
        rcall   UKEY
        goto    FEXECUTE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "key?"
        .align  2
KEYQ:
        rcall   UKEYQ
        goto    FEXECUTE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "'emit"
        .align  2
UEMIT:
        rcall   DOUSER
        .word   uemit

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "'key"
        .align  2
UKEY:
        rcall   DOUSER
        .word   ukey

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "'key?"
        .align  2
UKEYQ:
        rcall   DOUSER
        .word   ukeyq

; >CQ ( c addr -- ) Put to character queue
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  ">cq"
        .align  2
CQUEUE_TO:
        rcall   FETCH
        mov     [W14--], W0 ; W0 is the base pointer (mask)
        add     W0, #8, W2  ; W2 is the buffer start pointer
        disi    #8
        mov     [W0+2], W1      ; W1 is the write offset
        mov.b   [W14], [W2+W1]
        dec2    W14, W14
        inc     W1, W1
        and     W1, [W0++], [W0++] ; Store new writeoffset
        inc     [++W0], [W0]      ; Increment level
        return

; >CQ? ( addr -- flag ) Space available ? false = queue is full
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  ">cq?"
        .align  2
CQUEUE_TOQ:
        rcall   FETCH
        mov     [W14], W0       ; W0 is the base pointer (length)
        dec     [W0], W2        ; W2 = length
        disi    #2
        mov     [W0+6], W1      ; W1 = FillLevel
        sub     W1, W2, W1      ; W1 = Fill - maxLength
        asr     W1, #15, W1     ; Extend the sign bit to a flag
        mov     W1, [W14]
        return

; CQ> ( addr -- c) Get from character queue
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "cq>"
        .align  2
CQUEUE_FROM:
        rcall   FETCH
        mov     [W14], W0       ; W0 is the base pointer (mask)
        clr     [W14]           ; Zero the upper byte of the result
        add     W0, #8, W2      ; W2 is the buffer start pointer
        mov     [W0+4], W1      ; W1 is the read offset
        disi    #6
        mov.b   [W2+W1], [W14]   ; remember the unqueued character
        inc     W1, W1
        and     W1, [W0++], [++W0] ; Store the new read offset
        dec     [++W0], [W0]       ; Decrement level
        return

; CQ>? ( addr -- flag ) Character available ? false = no
        .pword  paddr(9b)+PFLASH
9:
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
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "cq0"
        .align  2
CQUEUEZ:
        rcall   FETCHPP         ; addr baseaddr
        rcall   SWOP
        rcall   FETCH           ; baseaddr size
        mov     [W14--], W2     ; size
        mov     [W14--], W1     ; W1 points to the base
        disi    #6
        mov     W2, [W1]        ; Store the size
        clr     [++W1]          ; Write offset
        clr     [++W1]          ; Read offset
        clr     [++W1]          ; Fill level
        return
    
; CQ: ( size "name" -- ) Create a character queue
        .pword  paddr(9b)+PFLASH
9:
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
        mov     #9, W0
        add     W0, [W14], [W14]
        rcall   ALIGNED
        rcall   RAM
        rcall   ALLOT
        rcall   XDOES
CQUEUE_DOES:
        rcall   DODOES
        return

.if TX1_BUF_SIZE > 0
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|5
        .ascii  "u1txq"
        .align  2
U1TXQUEUE:
        mlit    handle(U1TXQUEUE_DATA)+PFLASH
        return
U1TXQUEUE_DATA:
        .word   txqueue1
        .word   TX1_BUF_SIZE

.endif
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|5
        .ascii  "u1rxq"
        .align  2
U1RXQUEUE:
        mlit    handle(U1RXQUEUE_DATA)+PFLASH
        return
U1RXQUEUE_DATA:
        .word   rxqueue1
        .word   RX1_BUF_SIZE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "tx1"
        .align  2
TX1:
        rcall   PAUSE
        rcall   TX1Q
        cp0     [W14--]
        bra     z, TX1
TX1_1:
.if TX1_BUF_SIZE > 0
        rcall   U1TXQUEUE
        rcall   CQUEUE_TO
        bset    IFS0, #U1TXIF       ; check if UART TX has space
.else
        mov     [W14--], W0
        mov     W0, U1TXREG
.endif
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "tx1?"
        .align  2
TX1Q:
.if TX1_BUF_SIZE > 0
        mlit    handle(U1TXQUEUE_DATA)+PFLASH
        goto    CQUEUE_TOQ
.else
.if ERRATA_UTXBF == 1
        btsc    U1STA, #TRMT
.else
        btss    U1STA, #UTXBF
.endif
        bra     TRUE_
        goto    FALSE_
.endif
        
        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "rx1?"
        .align  2
RX1Q:
        mov     rbuf_lv1, W0
        mov     W0, [++W14]
        cp0     [W14]
        bra     nz, RX1Q1
        btsc    iflags, #fFC1
        bra     RX1Q1      
.if FC1_TYPE == 1
        btst    iflags, #ixoff1
        bra     z, RX1Q1
        bclr    iflags, #ixoff1
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

.ifdecl BAUDRATE2
.ifdecl _U2RXREG
.if TX2_BUF_SIZE > 0
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|5
        .ascii  "u2txq"
        .align  2
U2TXQUEUE:
        mlit    handle(U2TXQUEUE_DATA)+PFLASH
        return
U2TXQUEUE_DATA:
        .word   txqueue2
        .word   TX2_BUF_SIZE

        .pword  paddr(9b)+PFLASH
.endif
9:
        .byte   NFA|INLINE|5
        .ascii  "u2rxq"
        .align  2
U2RXQUEUE:
        mlit    handle(U2RXQUEUE_DATA)+PFLASH
        return
U2RXQUEUE_DATA:
        .word   rxqueue2
        .word   RX2_BUF_SIZE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "tx2"
        .align  2
TX2:    
        rcall   PAUSE
        rcall   TX2Q
        cp0     [W14--]
        bra     z, TX2
.if TX2_BUF_SIZE > 0
        rcall   U2TXQUEUE
        rcall   CQUEUE_TO
        bset    IFS1, #U2TXIF       ; check if UART TX has space
.else
        mov     [W14--], W0
        mov     W0, U2TXREG
.endif
TX2_2:
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "tx2?"
        .align  2
TX2Q:
.if TX2_BUF_SIZE > 0
        mlit    handle(U2TXQUEUE_DATA)+PFLASH
        goto    CQUEUE_TOQ
.else
.if ERRATA_UTXBF == 1
        btsc    U2STA, #TRMT
.else
        btss    U2STA, #UTXBF
.endif
        btsc    U2STA, #TRMT
        bra     TRUE_
        goto    FALSE_
.endif
        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "rx2?"
        .align  2
RX2Q:
        mov     rbuf_lv2, W0
        mov     W0, [++W14]
        cp0     [W14]
        bra     nz, RX2Q1
        btsc    iflags, #fFC2
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
.endif
.endif
        
.if USB_CDC == 1
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "usb+"
        .align  2
USB_ON:
	bset    U1PWRC, #0	    ; Power up the USB module
	mov	#bdt_base, W0
	lsr	W0, #8, W0
	mov	W0, U1BDTP1	    ; Set the BDT base address
        bset    U1CON, #0
	mov	#0xD, W0
	mov	W0, U1EP0	    ; Ep0 is control endpoint
        rcall   USBPrepareForNextSetupTrf
        mov     #0x9000, W0
        mov     W0, OSCTUN
	return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "usb-"
        .align  2
USB_OFF:
        bclr    U1CON, #0
        bclr    U1PWRC, #0
        clr     OSCTUN
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "rxu?"
        .align  2
RXUQ:
        btsc.b  usb_device_state, #3
        btsc.b  ep2ostat, #7
        bra     FALSE_
        goto    TRUE_

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "rxu"
        .align  2
RXU:
        rcall   PAUSE
        btsc.b  usb_device_state, #3
        btsc.b  ep2ostat, #7
        bra     RXU
        mov     #cdc_data_rx, W0
        mov     ep2optr, W1
        mov.b   [W0+W1], W0
        ze      W0, W0
        mov     W0, [++W14]
        inc     ep2optr
        dec.b   ep2ocnt
        bra     nz, RXU_END
        mov     #CDC_BULK_OUT_EP_SIZE, W0
        mov.b   WREG, ep2ocnt
        mov     #(_DAT1|_USIE|_DTSEN), W0
        btsc.b  ep2ostat, #6
        mov     #(_DAT0|_USIE|_DTSEN), W0
        mov.b   WREG, ep2ostat
        clr     ep2optr
RXU_END:
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "txu"
        .align  2
TXU:
        rcall   PAUSE
        btss.b  usb_device_state, #3
        bra     TXU_DROP
        btsc.b  ep2istat, #7
        bra     TXU
        inc2    ms_count, WREG
        mov     WREG, ep2itmo
        mov     ep2icount, W2
        mov     #cdc_data_tx, W1
        mov     [W14--], W0
        mov.b   W0, [W1+W2]
        inc     ep2icount
        cp      W2, #(CDC_BULK_IN_EP_SIZE-2)
        bra     n, TXU_END
TXU_SEND:
        mov     ep2icount, W0
TXU_SEND2:
        mov.b   WREG, ep2icnt
        mov     #(_DAT1|_USIE|_DTSEN), W0
        btsc.b  ep2istat, #6
        mov     #(_DAT0|_USIE|_DTSEN), W0
        mov.b   WREG, ep2istat
        clr     ep2icount
        bra     TXU_END
TXU_DROP:
        dec2    W14, W14
TXU_END:
        return
.endif
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "dup"
        .align  2
DUP:
        mov.w   [W14++], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|7
        .ascii  "execute"
        .align  2
EXECUTE:
        mov.w   [W14--], W0
        mov     #PFLASH, W1
        sub     W0, W1, W0
        goto    W0

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "@ex"
        .align  2
FEXECUTE:
        rcall   FETCH
        mov.w   [W14--], W0
        mov     #PFLASH, W1
        sub     W0, W1, W0
        goto    W0

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|COMPILE|4
        .ascii  "exit"
        .align  2
EXIT:
        sub     #4, W15
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|COMPILE|3
        .ascii  "(,)"
        .align  2
DOCOMMAXT:
         pop.d  W0
         tblrdl [W0++], [++W14]
         push.d W0
         goto   COMMAXT

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|COMPILE|3
        .ascii  "(c)"
        .align  2
DOCREATE:
        mov.w   [W15-4], W0
;        clr     TBLPAG
        tblrdl  [W0], [++W14]
        sub     #4, W15
        return
DOEMIT:
        mov.w   [W15-4], W0
;        clr     TBLPAG
        tblrdl  [W0++], [++W14]
        mov.w   W0, [W15-4]
        goto    EMIT

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|COMPILE|3
        .ascii  "(d)"
        .align  2
DODOES:
        pop.d   W0
        pop.d   W2
;        clr     TBLPAG
        tblrdl  [W2], [++W14]
        goto    W0

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "sp!"
        .align  2
SPSTORE:
        mov.w   [W14], W14
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "sp@"
        .align  2
SPFETCH:
        mov     W14, [++W14]
        return

; RP points to the first empty stack cell.
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "rp@"
        .align  2
RPFETCH:
        dec2    W15, W0
        dec2    W0, [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|4
        .ascii  "drop"
        .align  2
DROP:
        sub     W14, #2, W14
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "swap"
        .align  2
SWOP:
        mov     [W14--], W0
        mov     [W14++], [W14]
        mov     W0, [W14-2]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|4
        .ascii  "over"
        .align  2
OVER:
        mov     [W14-0x2], W0
        mov     W0, [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "rot"
        .align  2
ROT:
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14--], W0
        mov     W1, [++W14]
        mov     W2, [++W14]
        mov     W0, [++W14]
        return
        
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  ">r"
        .align  2
TOR:
        push    [W14--]
        return

        .pword  paddr(9b)+PFLASH
        .align  2
9:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "r>"
        .align  2
RFROM:
        pop     [++W14]
        return
        
        .pword  paddr(9b)+PFLASH
        .align  2
9:
        .byte   NFA|INLINE|COMPILE|2
        .ascii  "r@"
        .align  2
RFETCH:
        mov     [W15-2], W0
        mov     W0, [++W14]
        return

;   ABS     n   --- n1      absolute value of n
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "abs"
        .align  2
ABS:
        mov     [W14++], [W14]      ; dup
        rcall   QNEGATE
        return

;   ABS     n   --- n1      absolute value of n
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "dabs"
        .align  2
DABS:
        mov     [W14++], [W14]      ; dup
        goto    QDNEGATE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|1
        .ascii  "+"
        .align  2
PLUS:
        mov     [W14--], W0
        add     W0, [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|1
        .ascii  "-"
        .align  2
MINUS:
        mov     [W14-2], W0
        sub     W0, [W14], [--W14]
        return

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "and"
        .align  2
AND:
        mov     [W14--], W0
        and     W0, [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "or"
        .align  2
OR:
        mov     [W14--], W0
        ior     W0, [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "xor"
        .align  2
XOR:
        mov     [W14--], W0
        xor     W0, [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|6
        .ascii  "invert"
        .align  2
INVERT:
        com     [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|7
        .ascii  "dinvert"
        .align  2
DINVERT:
        com     [W14], [W14--]
        com     [W14], [W14++]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|6
        .ascii  "negate"
        .align  2
NEGATE:
        neg     [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|7
        .ascii  "dnegate"
        .align  2
DNEGATE:
        com     [W14--], W1
        com     [W14], W0
        add     W0, #1, [W14++]
        addc    W1, #0, [W14]
        return
        
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|8
        .ascii  "?dnegate"
        .align  2
QDNEGATE:
        cp0     [W14--]
        bra     n, DNEGATE
        return        
        
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "1+"
        .align  2
ONEPLUS:
        inc     [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "2+"
        .align  2
TWOPLUS:
        inc2    [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "1-"
        .align  2
ONEMINUS:
        dec     [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "2-"
        .align  2
TWOMINUS:
        dec2    [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "2*"
        .align  2
TWOSTAR:
        sl      [W14], [W14]        ; 2*
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "d2*"
        .align  2
DTWOSTAR:
        sl      [--W14], [W14]        ; 2* msb -> c, 0 -> lsb
        rlc     [++W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "2/"
        .align  2
TWOSLASH:
        asr     [W14], [W14]        ; 2/
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "d2/"
        .align  2
DTWOSLASH:
        asr     [W14], [W14--]          ; 2/ lsb -> c
        rrc     [W14], [W14++]          ; 2/ c -> msb
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "0="
        .align  2
ZEROEQUAL:
        cp0     [W14]
        bra     nz, test_false
test_true:
        setm    [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "d0="
        .align  2
DZEROEQUAL:
        mov     [W14--], W0
        ior     W0, [W14], [W14]
        bra     nz, test_false
        goto    test_true

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "0<"
        .align  2
ZEROLESS:
        cp0     [W14]
        bra     n, test_true
test_false:
        clr     [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "d0<"
        .align  2
DZEROLESS:
        cp0     [W14--]
        bra     n, test_true
        goto    test_false

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "!p"
        .align  2
STORE_P:
        mov     [w14--], W13
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|COMPILE|INLINE|4
        .ascii  "!p>r"
        .align  2
STORE_P_TO_R:
        push    W13
        mov     [W14--], W13
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|COMPILE|3
        .ascii  "r>p"
        .align  2
R_TO_P:
        pop     W13
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "@p"
        .align  2
FETCH_P:
        mov     W13, [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "p@"
        .align  2
PFETCH:
        mov     W13, [++W14]
        goto    FETCH

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "pc@"
        .align  2
PCFETCH:    
        mov     W13, [++W14]
        goto    CFETCH      

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "p!"
        .align  2
PSTORE:
        mov     W13, [++W14]
        goto    STORE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "pc!"
        .align  2
PCSTORE:
        mov     W13, [++W14]
        goto    CSTORE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|2
        .ascii  "p+"
        .align  2
PPLUS:
        inc     W13, W13
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "p++"
        .align  2
PNPLUS:
        mov     [w14--], W0
        add     W0, W13, W13
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "s>d"
        .align  2
STOD:
        mov     [W14++], [W14]
        goto    ZEROLESS

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "um*"
        .align  2
UMSTAR:
        mov     [W14--], W0
        mul.uu  W0, [W14], W2
        mov     W2, [W14++]
        mov     W3, [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "m*"
        .align  2
MSTAR:
        mov     [W14--], W0
        mul.ss  W0, [W14], W2
        mov     W2, [W14++]
        mov     W3, [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "uq*"
        .align  2
        push.d  w8
        mov     [W14--], W3
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14--], W0
        mul.uu  w0, w2, w4
        mul.uu  w1, w2, w6
        add     w5, w6, w5
        addc    w7, #0, w6
        mul.uu  w0, w3, w8
        add     w5, w8, w5
        addc    w6, w9, w6
        mul.uu  w1, w3, w8
        addc    w9, #0, w9
        add     w6, w8, w6
        addc    w9, #0, w7
        mov     w4, [++w14]
        mov     w5, [++w14]
        mov     w6, [++w14]
        mov     w7, [++w14]
        pop.d   w8
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|6
        .ascii  "uq/mod"
        .align  2
        mov     #32, W0
        push    W0
        mov     [W14--], W1
        mov     [W14--], W0
        mov     [W14--], W5
        mov     [W14--], W4
        mov     [W14--], W3
        mov     [W14--], W2
UQMOD1:
        clr     W6
        bclr    SR, #0
        rlc     W2, W2
        rlc     W3, W3
        rlc     W4, W4
        rlc     W5, W5
        rlc     w6, w6
        sub     w4, w0, w7
        subb    w5, w1, w7
        subb    #0, w6
        bra     NC, UQMOD2
        sub     w4, w0, w4
        subb    w5, w1, w5
        bset    w2, #0
UQMOD2:
        dec     [--W15], [W15++]
        bra     nz, UQMOD1
        mov     w4, [++w14]
        mov     w5, [++w14]
        mov     w2, [++w14]
        mov     w3, [++w14]
        pop     w0
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "d>q"
        .align  2
        clr     W0
        btsc    [W14], #15
        dec     W0, W0
        mov     W0, [++W14]
        mov     W0, [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "qm+"
        .align  2
        clr     W2
        btsc    [W14], #15
        dec     W2, W2
        mov     [W14--], W1
        mov     [W14--], W0
        sub     W14, #6, W14
        add     W0, [W14],[W14++]
        addc    W1, [W14],[W14++]
        addc    W2, [W14],[W14++]
        addc    W2, [W14],[W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "q+"
        .align  2
        mov     [W14--], W3
        mov     [W14--], W2
        mov     [W14--], W1
        mov     [W14--], W0
        sub     W14, #6, W14
        add     W0, [W14],[W14++]
        addc    W1, [W14],[W14++]
        addc    W2, [W14],[W14++]
        addc    W3, [W14],[W14]
        return

; user ( n "name" -- )
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "user"
        .align  2
USER:
        rcall   CONSTANT
        rcall   XDOES
DOUSER:
        pop.d   W0
;        clr     TBLPAG
        tblrdl  [W0], W0
        add     upcurr, WREG
        mov     W0, [++W14]
        return

; value
        .pword  paddr(9b)+PFLASH
9:
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
        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|IMMED|2
        .ascii  "to"
        .align  2
TO:
        goto    IS


; >body xt -- a-addr transform a created words XT to it's data field address
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|5
        .ascii  ">body"
        .align  2
TOBODY:
        inc2    [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|8
        .ascii  "variable"
        .align  2
VARIABLE_:
        rcall   HERE
        rcall   CELL
        rcall   ALLOT
        goto    CON_

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|9
        .ascii  "2variable"
        .align  2
TWOVARIABLE:
        rcall   HERE
        mlit    #4
        rcall   ALLOT
        goto    CON_

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  "co:"
        .align  2
CONSTANT:
        rcall   CREATE
        rcall   CELL
        neg     [W14], [W14]
        rcall   IALLOT
        goto    ICOMMA

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|8
        .ascii  "constant"
        .align  2
CON_:
        rcall   COLON
        rcall   LITERAL
        goto    SEMICOLON

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|9
        .ascii  "2constant"
        .align  2
TWOCON_:
        rcall   SWOP
        rcall   COLON
        rcall   LITERAL
        rcall   LITERAL
        goto    SEMICOLON

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "pfl"
        .align  2
PFLASH_:
        mlit    PFLASH
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  ">xa"
        .align  2
TO_XA:
        mov     #PFLASH, W0
        mov     [W14], W1
        sub     W1, W0, [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "xa>"
        .align  2
XA_FROM:
        mov     #PFLASH, W0
        add     W0, [W14],[W14]
        return

        .pword  paddr(9b)+PFLASH
9:
FLASH_L:
        .byte   NFA|5
        .ascii  "flash"
        .align  2
FLASH:
        mov     #4, W0
        mov     W0, cse
        return

        .pword  paddr(9b)+PFLASH
.ifdef PEEPROM
9:
EEPROM_L:
        .byte   NFA|6
        .ascii  "eeprom"
        .align  2
EEPROM:
        mov     #2, W0
        mov     W0, cse
        return

        .pword  paddr(9b)+PFLASH
.endif
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "dp"
        .align  2
DP:
        mlit    dpRAM
        rcall   CSE
        goto    PLUS

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "here"
        .align  2
HERE:
        rcall   DP
        goto    FETCH

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|1
        .ascii  ","
        .align  2
COMMA:
        rcall   HERE
        rcall   STORE
        rcall   CELL
        goto    ALLOT

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "c,"
        .align  2
CCOMMA:
        rcall   HERE
        rcall   CSTORE
        rcall   ONE
        goto    ALLOT

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|4
        .ascii  "cell"
        .align  2
CELL:
        mov     #2, W0
        mov     W0, [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "align"
        .align  2
ALIGN:
        rcall   HERE
        rcall   ALIGNED
        rcall   DP
        goto    STORE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|7
        .ascii  "aligned"
        .align  2
ALIGNED:
        inc     [W14], [W14]
        bclr    [W14], #0
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|5
        .ascii  "cell+"
        .align  2
CELLPLUS:
        inc2    [W14], [W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|5
        .ascii  "cells"
        .align  2
CELLS:
        sl      [W14], [W14]        ; 2*
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|5
        .ascii  "char+"
        .align  2
CHARPLUS:
        inc     [W14], [W14]        ; char+
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|5
        .ascii  "chars"
        .align  2
CHARS:
        return

STORCOLON:
        mlit    #0xfffc; -4
        goto    IALLOT

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "2@"
        .align  2
TWOFETCH:
        mov     [W14], [W15++]
        rcall   FETCH
        inc2    [--W15], [++W14]
        goto    FETCH

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "2!"
        .align  2
TWOSTORE:
        mov     [W14], [W15++]
        inc2    [W14], [W14]
        rcall   STORE
        mov    [--W15], [++W14]
        goto    STORE

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|5
        .ascii  "2drop"
        .align  2
TWODROP:
        mov.d   [W14--], W0
;        sub     W14, #4, W14        ; 2drop
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "2dup"
        .align  2
TWODUP:
        rcall   OVER
        goto    OVER

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "2swap"
        .align  2
TWOSWAP:
        rcall   ROT
        push    [W14--]
        rcall   ROT
        pop     [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "cr"
        .align  2
CR:
        mlit    #0x0d       ; CR \r
        rcall   EMIT
        mlit    #0x0a       ; LF \n
        goto    EMIT

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|5
        .ascii  "space"
        .align  2
SPACE_:  
        rcall   BL
        goto    EMIT

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "umin"
        .align  2
UMIN:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     GTU, UMIN1
        mov     W0, [W14]
UMIN1:
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "umax"
        .align  2
UMAX:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     LEU, UMAX1
        mov     W0, [W14]
UMAX1:
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|1
        .ascii  "0"
        .align  2
ZERO:
        clr     [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|1
        .ascii  "1"
        .align  2
ONE:
        mlit    #1
        return

; ACCEPT  c-addr +n -- +n'  get line from terminal
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|6
        .ascii  "accept"
        .align  2
ACCEPT:
        rcall   OVER
        rcall   PLUS
        mov     [W14--], [W15++]  ; S: start R:end
        rcall   DUP               ; S: start start R:end
ACC1:
        rcall   KEY		  ; get char
        mov     [W14], W0	  ; move from stack to W0
        cp      W0, #0x0d	  ; is it CR?
        bra     z, ACC_CR	  ; no, then check for line feed
        cp      W0, #0x0a	  ; check for Line Feed
        bra     z, ACC_LF
        cp      W0, #8            ; BS
        bra     z,ACC_BS_DEL
        sub.b   #127, W0          ; DEL
        bra     z,ACC_BS_DEL
        bra     ACC3
ACC_CR:
        rcall   FCR               ; Mark CR received
        rcall   CSTORE
        bra     ACC6
ACC_LF:
        sub     W14, #2, W14
        rcall   FCR
        rcall   CFETCH
        rcall   ZEROSENSE
        bra     z, ACC6            ; LF end of line CR has not been received
        rcall   FALSE_
        rcall   FCR
        rcall   CSTORE
        bra     ACC1		   ; repeat getting chars until cr
ACC_BS_DEL:	   
	sub     W14, #2, W14
        mov     [W14-2], W0
        cp      W0, [W14]          ; start and current pointers
        bra     z, ACC1
        dec     [W14], [W14]
        rcall   XSQUOTE
        .byte   3,8,0x20,8
        rcall   TYPE
        bra     ACC1
ACC3:
	mov     [W14++], [W14]      ; dup
        rcall   EMIT		    ; and echo char

        rcall   FALSE_
        rcall   FCR
        rcall   CSTORE

        rcall   OVER
        rcall   CSTORE
        inc     [W14], [W14]
        mov     [W15-2], W0         ; r@
        cp      W0, [W14]
        bra     nz, ACC1
ACC6:
        pop     W0                  ; rdrop
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
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  ".id"
        .align  2
DOTID:
        rcall   CFETCHPP
        mov     #NFL, W0              ; nfu
        and     W0, [W14--], [W15++]
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
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|3
        .ascii  ">pr"
        .align  2
TO_PR:
        mov     #255, W0
        and     W0, [W14], [W14]
        mov     [W14++], [W14]
        rcall   BL
        mlit    0x7f
        rcall   WITHIN
        cp0     [W14--]
        bra     nz, TO_PR1
        mov     #'.', W0
        mov     W0, [W14]
TO_PR1:
        return

;   ><  Swap bytes
        .pword  paddr(9b)+PFLASH
9:
        .byte    NFA|2
        .ascii  "><"
        .align  2
SWAPB:
        mov     [W14], W0
        swap    W0
        mov     W0, [W14]
        return


        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|4
        .ascii  "(s" 
        .byte 0x22 
        .ascii ")"
        .align  2
XSQUOTE:
.if 0
        pop     W0
        mov     #PFLASH, W7
        add     W7, [--W15],[++W14]
        rcall   CFETCHPP
        rcall   TWODUP
        rcall   PLUS
        rcall   ALIGNED
        mov     [W14--], W0
        sub     W0, W7, W0
        goto    W0
.endif
        pop     W0
        pop     W0
        mov     #PFLASH+1, W2
        add     W2, W0, [++W14]
        clr     [++W14]
        tblrdl.b  [W0++], [W14]
        add     W0, [W14], W1
        inc     W1, W1
        bclr    W1, #0
        goto    W1

; S"      --            compile in-line string to flash
        .pword  paddr(9b)+PFLASH
9:
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
        .pword paddr(9b)+PFLASH
9:
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
        rcall   ALLOT
        rcall   PLACE
        goto    ALIGN

; ."       --            compile string to print into flash
        .pword  paddr(9b)+PFLASH
9:
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
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "+!" 
        .align  2
PLUSSTORE:
        mov     [W14], [W15++]
        rcall   FETCH
        mov     [W14--], W0
        add     W0, [W14], [W14]
        mov     [--W15], [++W14]
        goto    STORE

;   WITHIN      ( u ul uh -- t )
;               Return true if u is within the range of ul and uh. ( ul <= u < uh )
        .pword  paddr(9b)+PFLASH
9:
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
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "<>" 
        .align  2
NOTEQUAL:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     NZ, test_true
        bra     test_false

;   =       x1 x2 -- flag       return true if x1 = x2
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|1
        .ascii  "=" 
        .align  2
EQUAL:
        mov     [W14--], W0
        cp      W0, [W14]
        bra     Z, test_true
        bra     test_false

;   =       d1 d2 -- flag       return true if d1 = d2
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|2
        .ascii  "d=" 
        .align  2
DEQUAL:
        rcall   DMINUS
        goto    DZEROEQUAL

;   <       n1 n2 -- flag       return true if n1 < n2
        .pword  paddr(9b)+PFLASH
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
        mov     [W14--], W2
        mul.ss  W2, [W14--], W0
        mov     W0, [++W14]
        return

; U/      u1 u2 -- u3      16/16-> divide
        .pword  paddr(STAR_L)+PFLASH
USLASH_L:
        .byte   NFA|2
        .ascii  "u/" 
        .align  2
USLASH:
        mov     [W14--], W2
        mov     [W14--], W0
        repeat  #17
        div.u   W0, W2
        mov     W0, [++W14]
        return

; U*/MOD  u1 u2 u3 -- u4 u5    u1*u2/u3, rem&quot
        .pword  paddr(USLASH_L)+PFLASH
USSMOD_L:
        .byte   NFA|6
        .ascii  "u*/mod" 
        .align  2
USSMOD:
        mov     [W14--], W3
        mov     [W14--], W2
        mul.uu  W2, [W14], W0
        repeat  #17
        div.ud  W0, W3
        mov     W1, [W14]
        mov     W0, [++W14]
        return


; */MOD  n1 n2 n3 -- n4 n5    n1*n2/n3, rem&quot
        .pword  paddr(USSMOD_L)+PFLASH
SSMOD_L:
        .byte   NFA|5
        .ascii  "*/mod" 
        .align  2
SSMOD:
        mov     [W14--], W3
        mov     [W14--], W2
        mul.ss  W2, [W14], W0
        repeat  #17
        div.sd  W0, W3
        mov     W1, [W14]
        mov     W0, [++W14]
        return

; */  n1 n2 n3 -- n4    n1*n2/n3, quot
        .pword  paddr(SSMOD_L)+PFLASH
SS_L:
        .byte   NFA|2
        .ascii  "*/" 
        .align  2
SS:
        mov     [W14--], W3
        mov     [W14--], W2
        mul.ss  W2, [W14--], W0
        repeat  #17
        div.sd  W0, W3
        mov     W0, [++W14]
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
        push    [W14]
        mov     [W14--], W0
        mul.uu  W0, [W14--], W2
        mov     W2, W1
        pop     W0
        mul.uu  W0, [W14], W2
        mov     W2, [W14++]
        mov     W3, [W14]
        add     W1, [W14], [W14]
        return
        
; UD/MOD  ud u --u(rem) ud(quot)
        .pword  paddr(L_UDSTAR)+PFLASH
L_UDSLASHMOD:
        .byte   NFA|6
        .ascii  "ud/mod"
        .align  2
UDSLASHMOD:
        mov     [W14--], W2
        clr     W1
        mov     [W14--], W0
        repeat  #17
        div.ud  W0, W2
        mov     W0, W3         ; save quot.h
        mov     [W14--], W0
        repeat  #17
        div.ud  W0, W2
        mov     W1, [++W14]
        mov     W0, [++W14]
        mov     W3, [++W14]
        return

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
        mov     [W14--], W0
        mov     [W14--], W1
        mov     W0, [++W14]
        mov     W1, [++W14]
        mov     W0, [++W14]
        return

; ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;
        .pword  paddr(TUCK_L)+PFLASH
QNEGATE_L:
        .byte   NFA|7
        .ascii  "?negate" 
        .align  2
QNEGATE:
        cp0     [W14--]
        bra     nn, QNEGATE1
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
        mov     [W14--], W0
        cp      W0, [W14]
        bra     LT, MAX1
        mov     W0, [W14]
MAX1:
        return

; MIN    n1 n2 -- n3              signed minimum
;   2DUP > IF SWAP THEN DROP ;
        .pword  paddr(MAX_L)+PFLASH
MIN_L:
        .byte   NFA|3
        .ascii  "min" 
        .align  2
MIN:    
        mov     [W14--], W0
        cp      W0, [W14]
        bra     GT, MIN1
        mov     W0, [W14]
MIN1:
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

; digit   n -- c            convert to 0..9a..z
;   [ HEX ] DUP 9 > IF 27 + THEN 30 + ;
        .pword  paddr(LESSNUM_L)+PFLASH
TODIGIT_L:
        .byte   NFA|5
        .ascii  "digit" 
        .align  2
TODIGIT: 
        mov     [W14], W0
        cp      W0, #10
        bra     n, TODIGIT1
        add     #0x27, W0
TODIGIT1:
        add     #0x30, W0
        mov     W0, [W14]
        return

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
        mov     [--W14], W0
        ior     W0, [++W14], W1
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
        cp0     [W14--]    ; ZEROLESS
        bra     nn, SIGN1
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
        mov     #handle(FLASHHI)+PFLASH, W1
        mov     cse, W0
        add     W0, W1, [++W14]
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
        mov     upcurr, W1
        mov     #ubase, W0
        add     W1, W0, [++W14]
        return
;        rcall   DOUSER
;        .word   ubase

; SOURCE   -- adr n         current input buffer
;   'SOURCE 2@ ;        length is at higher adrs
        .pword  paddr(BASE_L)+PFLASH
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
        mov     [W14--], W0
        mov     [W14], W1
        sub     W1, W0, [W14--]
        add     W0, [W14], [W14++]
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
        inc     [--W14], [W14++]
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
        inc2     [--W14], [W14++]
        return

; NFA>CFA   nfa -- cfa    name adr -> code field
        .pword  paddr(FETCHPP_L)+PFLASH
NFATOCFA_L:
        .byte   NFA|3
        .ascii  "n>c"
        .align  2
NFATOCFA:
        rcall   CFETCHPP
        mov     #NFL, W0        ; nfu
        and     W0, [W14--], W1
        add     W1, [W14], [W14]
        inc     [W14], [W14]
        mov     #0xfffe, W0
        and     W0, [W14], [W14]
        return

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
        btss    [W14--], #7
        bra     CFATONFA1
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
        mov     #IMMED, W0
        and     W0, [W14], [W14]
        return

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
        mov     [W14], W0
        sub     #0x3a, W0
        bra     n, DIGITQ1
        sub     #0x27, W0
DIGITQ1:
        add     W0, #0xa, [W14]
        bra     n, FALSE_       ; digit smaller than 0
        mov     upcurr, W1
        mov     [W1+ubase], W1
        cp      W1, [W14]       ; base - digit
        bra     gtu, TRUE_      ; digit smaller than base
        goto    FALSE_

; SIGN?   adr n -- adr' n' f   get optional sign
        .pword  paddr(DIGITQ_L)+PFLASH
SIGNQ_L:
        .byte   NFA|5
        .ascii  "sign?"
        .align  2
SIGNQ:
        rcall   OVER
        rcall   CFETCH
        mov     [W14--], W0
        sub     #'+', W0
        bra     z, SIGNQPLUS
        sub     #2, W0
        bra     z, SIGNQMINUS
        bra     SIGNQEND
SIGNQMINUS:
        rcall   SLASHONE
        bra     TRUE_
SIGNQPLUS:
        rcall   SLASHONE
SIGNQEND:
        goto    FALSE_
SLASHONE:
        inc     [--W14],[W14++]
        dec     [W14], [W14]
        return

; >NUMBER  0 0  adr u -- ud.l ud.h  adr' u'
;                       convert string to number
        .pword  paddr(SIGNQ_L)+PFLASH
TONUMBER_L:
        .byte   NFA|7
        .ascii  ">number"
        .align  2
TONUMBER:
        mov     #1, W11         ; W11 = A register
TONUM1:
        cp0     [W14]           ; ud.l ud.h  adr u
        bra     z, TONUM3
        push    [W14--]
        push    [W14]
        rcall   CFETCH
        mov     #'.', W0
        cp      W0, [W14]
        bra     nz, TONUM_CONT
        rcall   DROP
        bra     TONUM_SKIP
TONUM_CONT:
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
        clr     W11
TONUM_SKIP:
        pop     [++W14]
        pop     [++W14]  
        rcall   SLASHONE
        bra     TONUM1
TONUM3:
        add     W11, [W14], [W14]
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
        rcall   SLASHONE
        bra     BASEQ2
BASEQ1:
        sub     W14, #2, W14
BASEQ2: 
        rcall   TONUMBER        ; a ud.l ud.h  a' u
        pop     [++W14]         ; a ud.l ud.h  a' u oldbase
        rcall   BASE            ; a ud.l ud.h  a' u oldbase addr
        rcall   STORE           ; a ud.l ud.h  a' u
        
        cp0     [W14--]         ; a ud.l ud.h  a'
        bra     z, QNUMD
        
QNUM_ERR:                       ; Not a number
        pop     [++W14]         ; a ud.l ud.h a' u sign
        sub     W14, #8, W14    ; 2drop 2drop
        rcall   FALSE_          ; a 0           Not a number
        bra     QNUM3

QNUMD:                          ; Single or Double number
                                ; a ud.l ud.h a'
        rcall   ONEMINUS
        rcall   CFETCH          ; a ud.l ud.h c
        rcall   TO_A            ; a ud.l ud.h
        pop     [++W14]         ; a ud.l ud.d sign
        cp0     [W14--]
        bra     z, QNUMD1
        rcall   DNEGATE
QNUMD1:
        mov     #'.', W0        ; a ud.l ud.h c
        cp      W0, W11         ; W11 is A register
        bra     nz, QNUM1
        rcall   ROT             ; d.l d.h a
        dec2    W14,W14         ; d.l d.h
        mlit    2               ; d.l ud.h 2    Double number
        bra     QNUM3
QNUM1:                          ; single precision dumber
                                ; a ud.l ud.h
        dec2    W14,W14         ; a n
        rcall   NIP             ; n
        rcall   ONE             ; n 1           Single number
QNUM3:
        return

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
        mov     #0x8, W0
        add     W0, [W14], [W14]
        goto    FETCH

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
; ------------------------------------------------------
;  INTERPRET  i*x c-addr u -- j*x   interpret given buffer
        .pword  paddr(INQ_L)+PFLASH
9:
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
        bclr    iflags, #tailcall   ; allow tailcall optimisation
        bclr    iflags, #noclear    ; dont clear flags in case of \
        mov     [W14++], [W14]      ; dup
        mlit    handle(BSLASH)+PFLASH
        rcall   EQUAL
        cp0     [W14--]
        btsc    SRL, #Z
        bset    iflags, #noclear
.if WRITE_METHOD == 2
        rcall   BFLUSH
.endif
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

;;;;;;  number ?
        sub     W14, #2, W14       ; DROP the FIND result code
        rcall   NUMBERQ            ; a f
        cp0     [W14]
.if FLOATS == 1
        bra     z, IFLOAT
.else
        bra     z, IUNKNOWN
.endif
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
        bra     INTER1              ; n/d
.if FLOATS == 1
IFLOAT:
        rcall   DROP
        rcall   DUP                 ; a
        rcall   CFETCHPP            ; a a n
        rcall   FLOATQ              ; a a n xt
        cp0     [W14]
        bra     z, NOT_FLOAT1
        rcall   EXECUTE             ; a d f
        cp0     [W14--]             ; a d
        bra     z, NOT_FLOAT2
        rcall   ROT
        rcall   DROP
        rcall   STATE               ; d f
        cp0     [W14--]
        bra     z, INTER1           ; d
        bra     IDOUBLE             ; d
NOT_FLOAT1:                         ;  a a n xt
        dec2    W14, W14
NOT_FLOAT2:                         ;  a dl dh
        dec2    W14, W14
.endif
IUNKNOWN:                           ;  a f
        dec2    W14, W14
        rcall   CFETCHPP
        rcall   TYPE
        rcall   FALSE_
        rcall   QABORTQ
;        bra     INTER1
INTER6: 
        sub     W14, #2, W14
        return

; a n -- float flag
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|6
        .ascii  "float?"
        .align  2
FLOATQ:
        rcall   VALUE_DOES
        .word   tofloat

        .pword  paddr(9b)+PFLASH
9:
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

        .pword  paddr(9b)+PFLASH
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
        mov     #handle(DPS_BASE), W0
        bra     DPS_ALIGN
FRAM_A:
        mov     #handle(DPS_BASE) + IBUFSIZEL, W0
        bra     DPS_ALIGN
FLATEST_A:
        mov     #handle(DPS_BASE) + IBUFSIZEL*2, W0
        bra     DPS_ALIGN
FFLASH_A:
        mov     #handle(DPS_BASE) + IBUFSIZEL*3, W0
DPS_ALIGN:
        mov     W0, [++W14]
        return

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
        mov     #4, W0
        push    W0
DP_TO_RAM1:
        rcall   DUP
        rcall   EEREAD
        rcall   PSTORE
        rcall   PPLUS2
        rcall   PLUSPAGE
        dec     [--W15], [W15++] ; XNEXT
        bra     nz, DP_TO_RAM1
DP_TO_RAM2:
        pop     W0               ; UNNEXT
        pop     W13
        goto    DROP

PLUSPAGE:
        mlit    IBUFSIZEL
        bra     PLUS
.endif

; >dp ( -- ) Copy only changed ini, dp's and latest from ram to eeprom
        .pword  paddr(DP_TO_RAM_L)+PFLASH
DP_TO_EEPROM_L:
        .byte   NFA|3
        .ascii  ">dp"
        .align  2
DP_TO_EEPROM:
.if WRITE_METHOD == 2
        bclr    iflags, #edirty
.endif
.ifdef  PEEPROM
        push    W13
        mov     #dp_start, W13
        mlit    dpSTART
        mov     #5, W0
        push    W0
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
        btss    U1STA, #TRMT
        bra     $-2
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
        push    W13             ; P to return stack
        mov     #dpSTART, W13   ; src in ram
        rcall   FTURNKEY_A      ; dst in flash
        mov     #4, W0
        push    W0
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
        btss    U1STA, #TRMT
        bra     $-2
        mov     #'E', W2
        mov     W2, U1TXREG
.endif
DP_TO_EEPROM_1:
        rcall   PLUSPAGE
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
.if WRITE_METHOD == 2
DP_PUSH:
        mov     #dpSTART, W0
        mov     #dpSAVE, W1
        bra     DP_POP_LOOP
DP_POP:
        mov     #dpSTART, W1
        mov     #dpSAVE, W0
DP_POP_LOOP:
        repeat  #MARKER_LENGTH-1
        mov     [W0++], [W1++]
        return
.endif
       
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
.if WRITE_METHOD == 2
        rcall   DP_PUSH
.endif
.if WRITE_METHOD == 1
        rcall   DP_TO_RAM
.endif
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
.if WRITE_METHOD == 2
        rcall   SET_FLASH_W_TMO
.endif
        rcall   STATE
        cp0     [W14--]
        bra     nz, QUIT1
.if WRITE_METHOD == 1
        rcall   IFLUSH
        rcall   DP_TO_EEPROM ; If dirty then write after timeout in PAUSE
.endif
        rcall   XSQUOTE
        .byte   3
        .ascii  " ok"
        .align  2
        rcall   TYPE
        rcall   PROMPT
        goto    QUIT0

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
        rcall   IFLUSH
.if WRITE_METHOD == 2
        rcall   DP_POP
        rcall   DP_TO_EEPROM        ; If dirty then write after timeout in PAUSE
.endif
        goto    QUIT            ; QUIT never returns

; ?ABORT   f --       abort & print ?
        .pword  paddr(ABORT_L)+PFLASH
QABORTQ_L:
        .byte   NFA|7
        .ascii  "?abort?"
        .align  2
QABORTQ:
        rcall   XSQUOTE
        .byte   3
        .byte   '\?',7,7
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

        rcall   IHERE
        rcall   ALIGNED
        rcall   IDP             ; Align the flash DP.
        rcall   STORE
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
        cp0     [W14--]             ; ZEROLESS
        bra     nn, POSTPONE1
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
        mov     #PFLASH, W1
        add     W1, [--W15], [++W14]     ; a
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
        mov     [W14--], W0
        cp      W0, #7
        bra     nz, SEMICOLON1
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
.ifdef  PAIVT
        rcall   RETFIE_
.else
        mlit    handle(ALT_INT_EXIT)+PFLASH
        rcall   AGAINC
.endif
.if WRITE_METHOD == 2
        rcall   IFLUSH
        rcall   DP_TO_EEPROM
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
        cp      W3, #6
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
9:
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

        .pword  paddr(9b)+PFLASH
9:
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
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|COMPILE|5
        .ascii  "endit"
        .align  2
LEAVE:
        pop     W0
        clr     [W15++]
        return

; unnext compile a pop
        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|COMPILE|5
        .ascii  "rdrop"
        .align  2
RDROP:
        pop     W0
        return

; leave clear top of return stack

; BL      -- char                 an ASCII space
        .pword  paddr(9b)+PFLASH
BL_L:
        .byte   NFA|INLINE|2
        .ascii  "bl"
        .align  2
BL:
        mlit    0x20
        return

; IND      -- char                 an ASCII space
        .pword  paddr(BL_L)+PFLASH
IND_L:
        .byte   NFA|INLINE|3
        .ascii  "ind"
        .align  2
IND:
        mlit    index
        return

; STATE   -- a-addr             holds compiler state
; In RAM
        .pword  paddr(IND_L)+PFLASH
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
9:
        .byte   NFA|2
        .ascii  "ms"
        .align  2
MS:
        rcall   TICKS
        rcall   PLUS
MS_1:
        rcall   PAUSE
        mov     [W14], W1
        mov     ms_count, W0
        sub     W1, W0, W0         ; time - ticks
        bra     nn, MS_1
        sub     W14, #2, W14
        return

;;;;;;;;;;;;;;
CMP:
        mov     [W14--], [W15++]
        bra     CMP2
CMP1:
        rcall   CFETCHPP
        rcall   ROT
        rcall   CFETCHPP
        rcall   ROT
        rcall   MINUS
        rcall   ZEROSENSE
        bra     nz, TWODROPZ
CMP2:
        dec     [--W15], [W15++]
        bra     c, CMP1
        bra     TWODROPNZ
;;;;;;;;;;;;;;;;;;;;
LIKEQ:
        rcall   CFETCHPP
        mlit    0xf
        rcall   AND
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
        mov     [W14--], [W15++]
        bra     LIKEQ3
LIKEQ1:
        rcall   TWODUP
        rcall   FETCH_P
        rcall   PPLUS
        rcall   SWOP
        rcall   CMP
        bra     z, LIKEQ3
TWODROPNZ:
        bclr    SR, #Z
        bra     LIKEQ4
LIKEQ3:
        dec     [--W15], [W15++]
        bra     c, LIKEQ1
TWODROPZ:
        bset     SR, #Z
LIKEQ4:
        pop     W0
        goto    TWODROP


;;;;;;;;;;;;;;;;;;;;
LIKES:
        rcall   TWODUP
        rcall   LIKEQ
        bra     z, LIKES1
        rcall   DUP
        rcall   DOTID
        rcall   SPACE_
LIKES1:
        rcall   TWOMINUS
        rcall   FETCH
        cp0     [W14]
        bra     nz, LIKES
        goto    TWODROP

 ; WORDS    --          list all words in dict.
        .pword  paddr(9b)+PFLASH
WORDS_L:
        .byte   NFA|5
        .ascii  "words"
        .align  2
WORDS:
        rcall   BL
        rcall   WORD
        rcall   DUP
        mlit    handle(kernellink)+PFLASH
        rcall   WDS1
        rcall   LATEST
        rcall   FETCH
WDS1:   rcall   CR
        goto    LIKES

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
.if WRITE_METHOD == 2
        rcall   SET_EEPROM_W_TMO
.endif
        rcall   IDP
        goto    PLUSSTORE

; ALLOT   n --    allocate n bytes in current data section
        .pword  paddr(DOTS_L)+PFLASH
ALLOT_L:
        .byte   NFA|5
        .ascii  "allot"
        .align  2
ALLOT:
.if WRITE_METHOD == 2
        rcall   SET_EEPROM_W_TMO
.endif
        rcall   DP
        goto    PLUSSTORE

;   DUMP  ADDR U --       DISPLAY MEMORY
        .pword  paddr(ALLOT_L)+PFLASH
9:
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
        mov     #0x10, W0
        push    W0
DUMP2:
        rcall   CFETCHPP
        rcall   CELL
        rcall   UDOTR
        dec     [--W15], [W15++] ; XNEXT
        bra     nz, DUMP2
        pop     W0

        mov     #-0x10, W0         ; 0x10
        add     W0, [W14], [W14]   ; -
        neg     W0, [W15++]        ; 0x10 >r
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
        .pword   paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "Fcy"
        .align  2
FCY_:
        mov     #(FCY/1000), W0
        mov     W0, [++W14]
        return

.if FLOATS == 1
.include "floats.s"
.endif
; C4+ ( n1 -- n2) call C  function unsigned  short func(unsigned short n1);
        .pword  paddr(9b)+PFLASH

.if C_EXAMPLE == 1
9:
        .byte   NFA|3
        .ascii  "C4+"
        .align  2
CFOURADD_:
        mov     [W14], W0
        .extern C4add
        call    _C4add
        mov     W0, [++W14]
        return

        .pword  paddr(9b)+PFLASH
.endif
9:
        .byte   NFA|INLINE|5
        .ascii  "false"
        .align  2
FALSE_:                     ; TOS is 0000 (FALSE)
        clr     [++W14]
        return

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|4
        .ascii  "true"
        .align  2
TRUE_:                      ; TOS is ffff (TRUE)
        setm    [++W14]
        return

.include "registers.inc"

        .pword  paddr(9b)+PFLASH
9:
        .byte   NFA|INLINE|3
        .ascii  "p2+"
        .align  2
PPLUS2:
        inc2    W13, W13
        return


.equ kernellink, 9b

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

.palign IBUFSIZEL, 0xff
.ifndef PEEPROM
.if DPS_LOW == 1
.pspace IBUFSIZEL*6, 0xff
.endif
.endif
KERNEL_END:
.end
