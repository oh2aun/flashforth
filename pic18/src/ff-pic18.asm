;**********************************************************************
;                                                                     *
;    Filename:      ff-pic18.asm                                      *
;    Date:          01.04.2020                                        *
;    File Version:  5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     *
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2020  Mikael Nordman
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
; in the header of this file, and in the identification
; displayed by the word VER.
;**********************************************************************


#include "p18f-main.cfg"
#include "p18fxxxx.cfg"

#ifdef USB_CDC 
#if USB_OPERATOR_UART == 0
#define OPERATOR_TX  TX0
#define OPERATOR_RX  RX0
#define OPERATOR_RXQ RX0Q
#else
#define OPERATOR_TX  TX1_
#define OPERATOR_RX  RX1_
#define OPERATOR_RXQ RX1Q
#define OPERATOR_RX_IS_UART
#endif
        extern USBDriverService
        extern cdc_data_tx
        extern cdc_data_rx
        extern usb_device_state
        extern ep2istat
        extern ep2icnt
        extern ep2ostat
        extern ep2ocnt
        extern ep2itmo
        extern ep2icount
        extern ep2optr
        global TX0_SEND2
#else ; Normal UART

#define OPERATOR_TX  TX1_
#define OPERATOR_RX  RX1_
#define OPERATOR_RXQ RX1Q
#define OPERATOR_RX_IS_UART
#endif

#ifndef K42
#define movff_ movff
#else
#define movff_ movffl
#define INTCON INTCON0
#define EEADR NVMADRL
#ifdef NVMADRH
#define EEADRH NVMADRH
#endif
#define EEDATA NVMDAT
#define EECON1 NVMCON1
#define EEPGD 7
#define CFGS 6
#define EECON2 NVMCON2
#endif


;   FSR0    Sp  - Parameter Stack Pointer
;   FSR1    Tp  - Temporary Ram Pointer
;   FSR2    Rp  - R stack pointer
;   STKPTR      - Return Stack pointer for subroutine threading
;               Note: The HW return stack is 31 cells deep
;   FSR1L, FSR1H, TABLAT, TBLPTRL, TBLPTRH, PCLATH are used as interrupt
;               safe temporary registers.
; The parameter stack grows upwards
; Parameter stack only contains 16 bit values. (= A cell is 16 bits)
; When leaving a word the stack pointer points to 
; the Most Significant Byte (odd address)
; Single precision arithmetic is 16 bit aritmetic.
; A Character value on the stack is always 16 bits wide

Sptr    equ 0           ; Only for lfsr
Sp      equ FSR0L       ; current parameter stack pointer points 
                        ; to the MS byte of the 16 bit value
Sbank   equ FSR0H
Srw     equ INDF0       ; read/write TOS
Sminus  equ POSTDEC0    ; pop one byte
Splus   equ POSTINC0    ; 
plusS   equ PREINC0     ; push one byte
SWrw    equ PLUSW0      ; Offset to Sp in W

Tptr    equ 1           ; Only for lfsr
Tp      equ FSR1L       ; Temporary ram pointer
Tbank   equ FSR1H
Trw     equ INDF1       ; Read/write 
Tminus  equ POSTDEC1    ; pop
Tplus   equ POSTINC1    ; push
plusT   equ PREINC1
TWrw    equ PLUSW1      ; Offset to Tp in W

Rptr    equ 2           ; Only for lfsr
Rp      equ FSR2L       ; R stack pointer
Rbank   equ FSR2H
Rrw     equ INDF2       ; Read/write
Rminus  equ POSTDEC2    ; pop
Rplus   equ POSTINC2    ; push
plusR   equ PREINC2
RWrw    equ PLUSW2      ; Offset to Rp in W

;;; For Flow Control
XON_    equ h'11'
XOFF_   equ h'13'

CR_     equ h'0d'
LF_     equ h'0a'
BS_     equ h'08'

;;; Flags for the Name Field
COMPILE equ 0x10
INLINE  equ 0x20
IMMED   equ 0x40
NFA     equ 0x80
NFAmask equ 0x0f

;;; FLAGS2
f32secs equ 2           ; 32 seconds since start, shut down unconfigured USB
fFC     equ 1           ; 0=Flow Control, 1 = no Flow Control
ixoff   equ 0           ; 1=XOFF has been sent

;;; FLAGS1
noclear equ 6           ; dont clear optimisation flags         
idup    equ 5           ; Use dupzeroequal instead of zeroequal 
izeroeq equ 4           ; Use bnz instead of bz if zeroequal    
istream equ 3
fLOCK   equ 2           ; Lock writes to flash and eeprom  
fTAILC  equ 1           ; Disable tailcall optimisation    
idirty  equ 0           ; Flash buffer dirty flag

;;; Memory mapping prefixes
PRAM        equ RAM_LO      ; h'f000' or h'c000'
PEEPROM     equ EEPROM_LO   ; h'ec00' or h'bc00'

ussize      equ PARAMETER_STACK_SIZE
utibsize    equ TIB_SIZE + HOLD_SIZE


;****************************************************
; USE ACCESS BANK to R/W these registers
; Internal variables used by asm code
FORTH_VARS udata_acs ; Variables in Access Ram
upcurr   res 2       ; Current USER area pointer
p_lo     res 1       ; P and DO LOOP INDEX
p_hi     res 1
ibase_lo res 1       ; Memory address of ibuffer
ibase_hi res 1       ; Memory address of ibuffer
#if XSTORE == ENABLE
ibase_up res 1       ; Memory address of ibuffer
#endif
iaddr_lo res 1       ; Instruction Memory access address
iaddr_hi res 1       ; Instruction Memory access address
#if XSTORE == ENABLE
iaddr_up res 1       ; Instruction Memory access address
#endif
FLAGS1   res 1       ; Some flags                                    
FLAGS2   res 1       ; More flags
RXhead   res 1       ; Head of serial RX interrupt buffer
RXtail   res 1       ; Tail of serial RX interrupt buffer
RXcnt    res 1       ; Number of characters in the RX fifo
ms_count res 2       ; millisecond counter 4 bytes
cse      res 1       ; Current data section 0=flash, 2=eeprom, 4=ram 
state    res 1       ; State value. Can only be changed by []        
wflags   res 1       ; Word flags from word header
fstatus  res 1       ; if zero, cpu idle is allowed
irq_v    res 2       ; Interrupt vector
areg     res 1       ; A register
aregh    res 1
tickval  res 2       ; timer value for system tick
#if IDLE_MODE == ENABLE
#if CPU_LOAD == ENABLE
load_acc res 1       ;
load_res res 3       ; 256 ms load result
#endif
#endif
;;; Interrupt low priority save variables
#ifndef K42
ilw         res 1
ilstatus    res 1
ilbsr       res 1
#endif

;;; Interrupt high priority save variables
ihpclath    res 1
ihtablat    res 1
ihtblptrl   res 1
ihtblptrh   res 1
#ifndef K42
ihsp        res 1
ihsbank     res 1
ihprodl     res 1
ihprodh     res 1
#endif
ihtp        res 1
ihtbank     res 1


#ifdef USB_CDC
#ifdef __18F14K50
#if ussize < h'49'
FLASH_BUF udata
flash_buf   res flash_block_size
IRQ_STACK udata
irq_s0      res PARAMETER_STACK_SIZE_IRQ   ; Interrupt parameter stack.
#define USBUF_DEFINED
usbuf       res ussize
#define TIB_DEFINED
wordlenchar res 1
utibbuf     res utibsize
#endif

#else       ; NOT __18F14K50
IRQ_STACK udata
irq_s0      res PARAMETER_STACK_SIZE_IRQ   ; Interrupt parameter stack.
flash_buf   res flash_block_size
#endif      ; __18F14K50

#else       ; NOT USB
IRQ_STACK   udata
irq_s0      res PARAMETER_STACK_SIZE_IRQ   ; Interrupt parameter stack.
flash_buf   res flash_block_size
#endif      ; USB_CDC

#ifdef p18fxx2xx8_fix_1
SINTCON     res 1       ; Save INTCON before disabling interrupts
SPIE1       res 1       ; Save PIE1 before disabling interrupts
SPIE2       res 1       ; Save PIE2 before disabling interrupts
#endif

;;; The UART interface interrupt buffer areas
#ifdef USB_CDC
;UART_RX udata 
#endif
RXbufmask   equ RX_BUF_SIZE - 1
RXbuf       res RX_BUF_SIZE


#ifdef USB_CDC
USER_AREA udata
#endif
;;; FORTH variables
dpSTART     res 2
dpFLASH     res 2
dpEEPROM    res 2 
dpRAM       res 2
dpLATEST    res 2


;;; USER AREA for the OPERATOR task
ursize      equ  RETURN_STACK_SAVE_SIZE
; ursize can be decreased depending on how deep PAUSE has been nested in your application.
userarea:
us0         equ -d'26'          ; Start of parameter stack
uemit       equ -d'24'
ukey        equ -d'22'
ukeyq       equ -d'20'
utask       equ -d'18'
ubase       equ -d'16'
utib        equ -d'14'
uflg        equ -d'12'           ; ACCEPT true =  CR has been received  
ustatus     equ -d'11'
uhp         equ -d'10'
usource     equ -d'8'           ; Two cells
utoin       equ -d'4'
ulink       equ -d'2'
urptr       equ d'0'            ; Top of the saved return stack
uvars       res -us0
u0          res 2 + UADDSIZE
urbuf       res ursize
#ifndef USBUF_DEFINED
usbuf       res ussize
#endif
#ifndef TIB_DEFINED

wordlenchar res 1
utibbuf     res utibsize
#endif

;;; Start of free ram
#ifdef USB_CDC
#ifndef __18F14K50
HERE udata
#endif
#endif
dpdata      res 2

;;; Variables in EEPROM
beeprom     equ PEEPROM
dp_start    equ beeprom + h'0000' ; Deferred TURNKEY execution vector
dp_flash    equ beeprom + h'0002' ; FLASH dictionary pointer
dp_eeprom   equ beeprom + h'0004' ; EEPROM dictionary pointer
dp_ram      equ beeprom + h'0006' ; RAM dictionary pointer
latest      equ beeprom + h'0008' ; Pointer to latest dictionary word
prompt      equ beeprom + h'000a' ; Deferred prompt action
dpeeprom    equ beeprom + h'000c'

;**************************************************
; Code **********************************************
FF_RESET code 0x0000
        nop                      ; 18f252/18f258 ERRATA
        goto    main
;;***************************************************
;; Interrupt routines
;;; ****** HIGH PRIORITY INTERRUPT     
FF_INT_HI code 0x0008
#ifdef IDLEN
#if IDLE_MODE == ENABLE
#if CPU_LOAD == ENABLE
#ifndef K42
        bsf     T0CON, TMR0ON
#else 
        bsf     T0CON0, T0EN
#endif
#endif
#endif
#endif
; NOTE: irq_v must be initialized before any interrupt is enabled. 
; ' interrupt-routine 0 int!
irq_user:
        push
        movf    irq_v+1, W
        movwf   TOSH
        movf    irq_v, W
        movwf   TOSL
        return              ; Now the interrupt routine is executing
;;; *******************************************
;;; ****** LOW PRIORITY INTERRUPT     
;;; Timer 0 interrupt must be marked low priority   
FF_INT_LO code 0x0018
#ifndef K42
        movwf   ilw
        movff   STATUS, ilstatus
#endif
#ifdef IDLEN
#if IDLE_MODE == ENABLE
#if CPU_LOAD == ENABLE
#ifndef K42
        bsf     T0CON, TMR0ON
        btfss   INTCON, TMR0IF
        bra     irq_t0_end
        bcf     INTCON, TMR0IF
        incf    load_acc
        bra     irq_end
#else 
        bsf     T0CON0, T0EN
        banksel PIR3
        btfss   PIR3, TMR0IF, BANKED
        bra     irq_t0_end
        bcf     PIR3,TMR0IF
        incf    load_acc
        retfie  1
#endif
irq_t0_end:
#endif
#endif
#endif
;;; ****** LOW PRIORITY INTERRUPT    
;;; The tick counter interrupt and UART interrupt must be marked as low priority    
irq_low:
;; 1 millisecond tick counter
irq_ms:
#ifndef K42
#if MS_TMR == 1  ;****************************
        btfss   PIR1, TMR1IF
        bra     irq_ms_end
        movf    tickval, W   ;   low(tmr1ms_val)
        subwf   TMR1L, F
        movf    tickval+1, W ;   high(tmr1ms_val)
        subwfb  TMR1H, F
        bcf     PIR1, TMR1IF
#endif
#if MS_TMR == 2 ;******************************
        btfss   PIR1, TMR2IF
        bra     irq_ms_end
        bcf     PIR1, TMR2IF
#endif
#if MS_TMR == 3 ;******************************
        btfss   PIR2, TMR3IF
        bra     irq_ms_end
        movlw   low(tmr1ms_val)
        subwf   TMR3L, F
        movlw   high(tmr1ms_val)
        subwfb  TMR3H, F
        bcf     PIR2, TMR3IF
#endif
#if MS_TMR == 4 ;******************************
        btfss   PIR5, TMR4IF
        bra     irq_ms_end
        bcf     PIR5, TMR4IF
#endif
#if MS_TMR == 5 ;******************************
        btfss   PIR5, TMR5IF
        bra     irq_ms_end
        banksel T5CON
        movlw   low(tmr1ms_val)
        subwf   TMR5L, F, BANKED
        movlw   high(tmr1ms_val)
        subwfb  TMR5H, F, BANKED
        bcf     PIR5, TMR5IF
#endif
#if MS_TMR == 6 ;******************************
        btfss   PIR5, TMR6IF
        bra     irq_ms_end
        bcf     PIR5, TMR6IF
#endif
#else ; K42 K83
#if MS_TMR == 1  ;****************************
        banksel PIR4
        btfss   PIR4, TMR1IF, BANKED
        bra     irq_ms_end
        bcf     PIR4, TMR1IF, BANKED
        movlw   low(tmr1ms_val)
        subwf   TMR1L, F
        movlw   high(tmr1ms_val)
        subwfb  TMR1H, F
#endif
#if MS_TMR == 2 ;******************************
        banksel PIR4
        btfss   PIR4, TMR2IF, BANKED
        bra     irq_ms_end
        bcf     PIR4, TMR2IF, BANKED
#endif
#if MS_TMR == 3 ;******************************
        banksel PIR6
        btfss   PIR6, TMR3IF, BANKED
        bra     irq_ms_end
        movlw   low(tmr1ms_val)
        subwf   TMR3L, F, A
        movlw   high(tmr1ms_val)
        subwfb  TMR3H, F, A
        bcf     PIR6, TMR3IF, BANKED
#endif
#if MS_TMR == 4 ;******************************
        banksel PIR7
        btfss   PIR7, TMR4IF, BANKED
        bra     irq_ms_end
        bcf     PIR7, TMR4IF, BANKED        
#endif
#if MS_TMR == 5 ;******************************
        banksel PIR8
        btfss   PIR8, TMR5IF, BANKED
        bra     irq_ms_end
        movlw   low(tmr1ms_val)
        subwf   TMR5L, F, A
        movlw   high(tmr1ms_val)
        subwfb  TMR5H, F, A
        bcf     PIR8, TMR5IF, BANKED
#endif
#if MS_TMR == 6 ;******************************
        banksel PIR9
        btfss   PIR9, TMR6IF, BANKED
        bra     irq_ms_end
        bcf     PIR9, TMR6IF, BANKED        
#endif
#endif
        movlw   TICK_TIME
        addwf   ms_count, F
        movlw   0
        addwfc  ms_count+1, F
#ifdef IDLEN
#if IDLE_MODE == ENABLE
#if CPU_LOAD == ENABLE
        movf    ms_count, W
        bnz     irq_load_end
        movf    TMR0L, W
        movwf   load_res
        movf    TMR0H, W
        clrf    TMR0H
        clrf    TMR0L
        movwf   load_res+1
        movf    load_acc, W
        movwf   load_res+2
        clrf    load_acc
#endif
#endif
#endif
irq_load_end:
#ifndef K42
        bra     irq_end
#else ; K42 K83
        retfie 1
#endif
irq_ms_end:
;;; ************************************************
;;; UART RX interrupt routine
;;; Feeds the input buffer with characters
;;; from the serial line
irq_async_rx:
#if UART == 1
#ifndef K42
        btfss   PIR1, RCIF
#else
        banksel PIR3
        btfss   PIR3, U1RXIF, BANKED
#endif
#else   ; UART == 2
#ifndef K42
        btfss   PIR3, RC2IF
#else
#ifdef K83
        banksel PIR7
        btfss   PIR7, U2RXIF, BANKED
#else
        banksel PIR6
        btfss   PIR6, U2RXIF, BANKED
#endif
#endif
#endif
        bra     irq_async_rx_end
#ifndef K42
        movf    Tp, W
        movwf   ihtp
        movf    Tbank, W
        movwf   ihtbank
#endif

        bsf     FLAGS1, istream      ; Indicate input stream activity to FLASH write routine
        movf    RXcnt, W
        addlw   d'255' - RX_OFF_FILL
        bnc     irq_async_rx_2
        
#if FC_TYPE_SW == ENABLE
        btfss   FLAGS2, fFC
        rcall   XXOFF
#endif
irq_async_rx_1:
#ifdef  HW_FC_CTS_PORT
        btfss   FLAGS2, fFC
        bsf     HW_FC_CTS_PORT, HW_FC_CTS_PIN
#endif

irq_async_rx_2:
        lfsr    Tptr, RXbuf
        movf    RXhead, W
#if UART == 1
#ifndef K42
        movff   RCREG, TWrw
        btfsc   RCSTA, OERR
        bcf     RCSTA, CREN ; Restart RX on case of RX overrun
        bsf     RCSTA, CREN

#else
        movffl  U1RXB, TWrw
        banksel U1ERRIR
        bcf     U1ERRIR, RXFOIF, BANKED
#endif
#else  ; UART 2
#ifndef K42
        movff   RCREG2, TWrw
        movff   BSR, ilbsr
        banksel RCSTA2
        btfsc   RCSTA2, OERR2, BANKED
        bcf     RCSTA2, CREN2, BANKED ; Restart RX on case of RX overrun
        bsf     RCSTA2, CREN2, BANKED
        movff   ilbsr, BSR
#else
        movffl  U2RXB,  TWrw
        banksel U2ERRIR
        bcf     U2ERRIR, RXFOIF, BANKED
#endif
#endif
        movf    TWrw, W
        sublw   0x0f                    ; ctrl-o
#if CTRL_O_WARM_RESET == ENABLE
        bnz     irq_async_rx_3 
        reset                           ; Make a warm start
#endif
irq_async_rx_3:
#if FC_TYPE_SW == ENABLE
        addlw   0x04                    ; ctrl-s, xoff 0x13, 0xf - 0x13 + 0x4 = 0
        btfss   FLAGS2, fFC             ; receive xoff if FC is off
        bz      irq_async_rx_end        ; Do not receive  xoff
#endif
irq_async_rx_4:
        incf    RXhead, F
        movlw   RXbufmask               ; Wrap the RXhead pointer. 
        andwf   RXhead, F
        incf    RXcnt, F
irq_async_rx_end:
;;; *****************************************************************
;; Restore Tp and Tbank
#ifndef K42
        movf    ihtbank, W
        movwf   Tbank
        movf    ihtp, W
        movwf   Tp
#endif
irq_end:
#ifndef K42
        movf    ilw, W
        movff   ilstatus, STATUS
        retfie  0
#else ; K42 K83
        retfie  1
#endif
;;; ******************************************
;;; ******** HIGH PRIORITY INTERRUPT
;irq_user:
;        push
;        movwf   TOSH
;        movf    irq_v, W
;        movwf   TOSL
;        return              ; Now the interrupt routine is executing
;;; *************************************************

; *******************************************************************
;;; WARM user area data
warmlitsize equ d'28'
WARMLIT:
;        dw      u0+PRAM        ; UP
        dw      usbuf+(PRAM-1) ; S0
        dw      OPERATOR_TX    ; EMIT vector
        dw      OPERATOR_RX    ; KEY vector
        dw      OPERATOR_RXQ   ; KEY? vector
        dw      OPERATOR_AREA  ; TASK vector 
        dw      DEFAULT_BASE   ; BASE
        dw      utibbuf+(PRAM) ; TIB
        dw      0              ; ustatus & uflg
        dw      0,0,0,0
        dw      0; u0+h'f000'  ; ulink
        dw      urbuf+PRAM     ; urptr

;;; **************************************

;;; EMPTY dictionary data
STARTV: dw      h'0000'
DPC:    dw      dpcode     ; dp_user_dictionary
DPE:    dw      dpeeprom
DPD:    dw      dpdata+PRAM
LW:     dw      lastword
STAT:   dw      DOTSTATUS
; *******************************************************************

; EXIT --   Compile a return
;        variable link
        dw      0
L_EXIT:
        db      NFA|4,"exit"
EXIT:
        decf    STKPTR, F, A     ; pop does not work on K42 and K83
        return

; idle
        dw      L_EXIT
#if IDLE_MODE == ENABLE
L_IDLE:
        db      NFA|4,"idle"
IDLE:
        bsf    fstatus, 7
        return
        
; busy
        dw      L_IDLE
L_BUSY:
        db      NFA|4,"busy"
BUSY:
        clrf    fstatus
        return

; load -- n
        dw      L_BUSY
#if CPU_LOAD == ENABLE
L_LOAD:
        db      NFA|4,"load"
LOAD:
        movf    load_res, W
        movwf   plusS
        movf    load_res+1, W
        movwf   plusS
        movf    load_res+2, W
        movwf   plusS
        clrf    plusS
        call    LIT
        dw      CPU_LOAD_VAL
        call    UMSLASHMOD
        goto    NIP
        
; a, ( -- 0 ) Force Access bank
        dw      L_LOAD
#endif
#endif
L_A_:
        db      NFA|2,"a,"
A_:
        goto    FALSE_

; w, ( -- 0 ) Destination W register
        dw      L_A_
L_W_:
        db      NFA|2,"w,"
W_:
        goto    FALSE_

; movf, ( f d a -- )
        dw      L_W_
L_MOVF_
        db      NFA|5,"movf,"
MOVF_:
        rcall   AS3_DOES
        dw      h'0050'

; andlw, ( k -- )
        dw      L_MOVF_
L_ANDLW_:
        db      NFA|6,"andlw,"
ANDLW_:
        rcall   AS1_DOES
        dw      h'0b00'

; [i   --    Save registers for the Forth interrupt context
;;;     16 instruction cycles
        dw      L_ANDLW_
L_LI:
        db      NFA|INLINE|COMPILE|2,"[i"
#ifndef K42
        movf    PRODL, W
        movwf   ihprodl
        movf    PRODH, W
        movwf   ihprodh
        movf    Sp, W
        movwf   ihsp
        movf    Sbank, W
        movwf   ihsbank 
        movf    Tp, W
        movwf   ihtp
        movf    Tbank, W
        movwf   ihtbank
 #endif
        movf    TBLPTRL, W
        movwf   ihtblptrl
        movf    TBLPTRH, W
        movwf   ihtblptrh
        movf    TABLAT, W
        movwf   ihtablat
        lfsr    Sptr, irq_s0 - 1  ; 0xf05f
        return

; i]   --    Restore registers for the Forth interrupt context
;;;     14 instruction cycles
        dw      L_LI
L_IR:
        db      NFA|INLINE|COMPILE|2,"i]"
#ifndef K42
        movf    ihtbank, W
        movwf   Tbank
        movf    ihtp, W
        movwf   Tp
        movf    ihsbank, W
        movwf   Sbank
        movf    ihsp, W
        movwf   Sp
        movf    ihprodl, W
        movwf   PRODL
        movf    ihprodh, W
        movwf   PRODH
#endif
        movf    ihtablat, W
        movwf   TABLAT
        movf    ihtblptrl, W
        movwf   TBLPTRL
        movf    ihtblptrh, W
        movwf   TBLPTRH
        return

;***************************************************
; TX1   c --    output character to the TX1 buffer
        dw      L_IR
L_TX1_:
        db      NFA|3,"tx1"
TX1_:
#if UART == 1
#ifndef U1ERRIR
        btfsc   PIR1, TXIF
#else
        banksel PIR3
        btfsc   PIR3, U1TXIF, BANKED
#endif
#else
#ifndef U2ERRIR
        btfsc   PIR3, TX2IF
#else
#ifdef K83
        banksel PIR7
        btfsc   PIR7, U2TXIF, BANKED
#else
        banksel PIR6
        btfsc   PIR6, U2TXIF, BANKED
#endif
#endif
#endif
        bra     TX1_SEND
        rcall   PAUSE
        bra     TX1_
TX1_SEND:
        movf    Sminus, W
        movf    Sminus, W
#if USE_8BIT_ASCII == DISABLE
        andlw   h'7f'
#endif
#if UART == 1
#ifndef U1TXB
        movwf   TXREG
#else
        banksel U1TXB
        movwf   U1TXB, BANKED
#endif 
#else
#ifndef U2TXB
        banksel TXREG2
        movwf   TXREG2, BANKED
#else 
        banksel U2TXB
        movwf   U2TXB, BANKED
#endif 
#endif
        goto    PAUSE
;***************************************************
; RX1    -- c    get character from the serial line
        dw      L_TX1_
L_RX1_:
        db      NFA|3,"rx1"
RX1_:
        rcall   PAUSE
        rcall   RX1Q
        movf    Sminus, W
        iorwf   Sminus, W
        bz      RX1_
        lfsr    Tptr, RXbuf
        movf    RXtail, W
        movff_   TWrw, plusS    ;  Take a char from the buffer
        clrf    plusS
        incf    RXtail, F
        movlw   RXbufmask
        andwf   RXtail, F
        decf    RXcnt, F
        return
;***************************************************
; RX1?  -- n    return the number of characters in queue
        dw      L_RX1_
L_RX1Q:
        db      NFA|4,"rx1?"
RX1Q:
#if UART == 1
#ifndef K42
        btfsc   RCSTA, OERR
        bcf     RCSTA, CREN ; Restart RX on case of RX overrun
        bsf     RCSTA, CREN    
#else
        banksel U1ERRIR
        btfsc   U1ERRIR, RXFOIF, BANKED
        bcf     U1ERRIR, RXFOIF, BANKED ; Restart RX on case of RX overrun
        bsf     U1ERRIR, RXFOIF, BANKED
#endif 
#else ; UART == 2
#ifndef K42
        banksel RCSTA2
        btfsc   RCSTA2, OERR2, BANKED
        bcf     RCSTA2, CREN2, BANKED ; Restart RX on case of RX overrun
        bsf     RCSTA2, CREN2, BANKED
#else 
        banksel U2ERRIR
        btfsc   U2ERRIR, RXFOIF, BANKED
        bcf     U2ERRIR, RXFOIF, BANKED ; Restart RX on case of RX overrun
        bsf     U2ERRIR, RXFOIF, BANKED
#endif 
#endif
        movf    RXhead, W
        subwf   RXtail, W
        movwf   plusS
        bnz     RX1Q2
#if FC_TYPE_SW == ENABLE
        btfss   FLAGS2, fFC
        rcall   XXON
#endif
#ifdef  HW_FC_CTS_PORT
        bcf     HW_FC_CTS_PORT, HW_FC_CTS_PIN
#endif
        clrf    RXcnt
RX1Q2:
        clrf    plusS
        return

;*******************************************************
;;; Multiplication routine from the PIC datasheet adapted to FORTH.
;;; Uses the registers:
;;; TBLPTRH TBLPTRL PCLATH TABLAT TOSL TOSH PRODL PRODH
;;; 42 clock cycles
umstar0:
        rcall   TOTBLP
        movf    Sminus, W
        movwf   PCLATH
        movf    Sminus, W
        movwf   TABLAT
        movf    TBLPTRL, W
        
        push
        mulwf   TABLAT       ; ARG1L * ARG2L ->  PRODH:PRODL
        movf    PRODL, W
        movwf   plusS
        movf    PRODH, W
        movwf   plusS

        movf    TBLPTRH, W
        mulwf   PCLATH       ; ARG1H * ARG2H -> PRODH:PRODL
        movf    PRODL, W
        movwf   TOSL
        movf    PRODH, W
        movwf   TOSH

        movf    TBLPTRL, W
        mulwf   PCLATH       ; ARG1L * ARG2H -> PRODH:PRODL
        movf    PRODL, W
        addwf   Srw, F
        movf    PRODH, W
        addwfc  TOSL, F
        clrf    WREG
        addwfc  TOSH, F

        movf    TBLPTRH, W   
        mulwf   TABLAT       ; ARG1H * ARG2L -> PRODH:PRODL
        movf    PRODL, W     ;
        addwf   Srw, F       ; Add cross
        movf    PRODH, W     ; products
        addwfc  TOSL, F
        clrf    WREG
        addwfc  TOSH, F

        movf    TOSL, W
        movwf   plusS
        movf    TOSH, W
        movwf   plusS
        decf    STKPTR, F, A
        return

;***********************************************************
;;; 320 - 384 cycles for 16 or 32 bit division
;;; TBLPTRH TBLPTRL PCLATH TABLAT Tp Tbank PRODL PRODH

#define DIVIDEND_0      PRODL
#define DIVIDEND_1      PRODH
#define DIVIDEND_2      TABLAT
#define DIVIDEND_3      PCLATH
#define DIVISOR_0       TBLPTRL
#define DIVISOR_1       TBLPTRH
#define DCNT            Tbank

umslashmod0:
        rcall   TOTBLP     ; DIVISOR_1, DIVISOR_0
        tstfsz  DIVISOR_0
        bra     umslashmod3
        tstfsz  DIVISOR_1
        bra     umslashmod3
        bsf     fstatus, 0  ; Signal divide by zero error
        bra     WARM
umslashmod3:
        movf    Sminus, W
        movwf   DIVIDEND_3
        movf    Sminus, W
        movwf   DIVIDEND_2
        movf    Sminus, W
        movwf   DIVIDEND_1
        movf    Sminus, W
        movwf   DIVIDEND_0
        movlw   d'16'
        movwf   DCNT             ; count to 16; 19
UMSLASHMOD1:
        clrf    Tp
        bcf     STATUS, C
        rlcf    DIVIDEND_0, F
        rlcf    DIVIDEND_1, F
        rlcf    DIVIDEND_2, F
        rlcf    DIVIDEND_3, F
        rlcf    Tp, F

        movf    DIVISOR_0, W
        subwf   DIVIDEND_2, W 
        movf    DIVISOR_1, W
        subwfb  DIVIDEND_3, W
        movlw   0
        subwfb  Tp, W
        bnc     UMSLASHMOD2

        movf    DIVISOR_0, W
        subwf   DIVIDEND_2, F
        movf    DIVISOR_1, W
        subwfb  DIVIDEND_3, F
        bsf     DIVIDEND_0, 0
UMSLASHMOD2:
        decfsz  DCNT, F
        bra     UMSLASHMOD1         ; 16*(18-22) = ~320
        movf    DIVIDEND_2, W
        movwf   plusS            ; remainder
        movf    DIVIDEND_3, W
        movwf   plusS
        movf    DIVIDEND_0, W
        movwf   plusS            ; quotient
        movf    DIVIDEND_1, W
        movwf   plusS
        return                  ; 11 cycles 
; *******************************************************************
;if (ibaselo != (iaddrlo&0xc0))&& (ibasehi != iaddrhi)
;   if (idirty)
;       writebuffer_to_imem
;   endif
;   fillbuffer_from_imem
;   ibaselo = iaddrlo&0xc0
;   ibasehi = iaddrhi
;endif
iupdatebuf:
#if XSTORE == ENABLE
        movf    iaddr_up, W
        cpfseq  ibase_up
        bra     iupdatebuf0
#endif
        movf    iaddr_hi, W
        cpfseq  ibase_hi
        bra     iupdatebuf0
        movlw   flash_pointer_mask
        andwf   iaddr_lo, W
        cpfseq  ibase_lo
        bra     iupdatebuf0
        return

iupdatebuf0:
        rcall   IFLUSH
        movlw   flash_pointer_mask
        andwf   iaddr_lo, W
        movwf   ibase_lo
        movf    iaddr_hi, W
        movwf   ibase_hi
#if XSTORE == ENABLE
        movf    iaddr_up, W
        movwf   ibase_up
#endif
fill_buffer_from_imem:
        movlw   flash_block_size ; d'64' or d'128'
        movwf   PCLATH
        rcall   init_ptrs             ; Init TBLPTR and ram pointer
fill_buffer_from_imem_1:
        tblrd*+
        movf    TABLAT, W
        movwf   Tplus
        decfsz  PCLATH, F
        bra     fill_buffer_from_imem_1
#if XSTORE == ENABLE
        clrf    TBLPTRU
#endif
        return
;***********************************************************
write_buffer_to_imem:
;; Loop here until there are no more characters has been received for a while
;; from the UART.
;; The assumption is that the serial line is silent then.
#ifdef OPERATOR_RX_IS_UART
#if FC_TYPE_SW == ENABLE
        btfss   FLAGS2, fFC
        rcall   XXXOFF
#endif
#ifdef  HW_FC_CTS_PORT
        btfss   FLAGS2, fFC
        bsf     HW_FC_CTS_PORT, HW_FC_CTS_PIN
#endif

wbtil:
        bcf     FLAGS1, istream
        call    LIT
        dw      (d'19200'/baud)+d'2'
        call    MS
        btfsc   FLAGS1, istream
        bra     wbtil
#endif
        bcf     INTCON, GIE  ; Disable Interrupts

#ifdef p18fxx2xx8_fix_1
        movff_   PIE1, SPIE1
        movff_   PIE2, SPIE2
        movff_   INTCON, SINTCON ; TMR0IF, INT0IF, RBIF 
        clrf    INTCON       ; may be lost
        clrf    PIE1
        clrf    PIE2
#endif
        rcall   init_ptrs             ; Init TBLPTR and ram pointer
        banksel EECON1
        bsf     EECON1, EEPGD, BANKED ; Erase the flash block
        bcf     EECON1, CFGS, BANKED
        bsf     EECON1, WREN, BANKED
        bsf     EECON1, FREE, BANKED
        rcall   magic
        bcf     EECON1, FREE, BANKED

        TBLRD*-
        movlw   flash_write_outer_loop
        movwf   PRODH
write_buffer_to_imem_1:
        movlw   flash_write_inner_loop
        movwf   PRODL
write_buffer_to_imem_2:
        movf    Tplus, W
        movwf   TABLAT
        tblwt+*
        decfsz  PRODL, F
        bra     write_buffer_to_imem_2
        rcall   magic
        decfsz  PRODH, F
        bra     write_buffer_to_imem_1
        bcf     EECON1, WREN, BANKED
#ifdef p18fxx2xx8_fix_1
        movff_   SPIE2, PIE2
        movff_   SPIE1, PIE1
        movff_   SINTCON, INTCON
#endif
        bsf     INTCON, GIE        
verify_imem:
        movlw   flash_block_size     ; d'64' or d'128'
        movwf   PCLATH
        rcall   init_ptrs
verify_imem_1:
        tblrd*+
        movf    TABLAT, W
        cpfseq  Tplus
        reset
        decfsz  PCLATH, F
        bra     verify_imem_1

        bcf     FLAGS1, idirty ; Mark flash buffer clean
        setf    ibase_hi ; Mark flash buffer empty
#if XSTORE == ENABLE
        setf    ibase_up ; Mark flash buffer empty
	clrf    TBLPTRU
#endif
        return
init_ptrs:
        lfsr    Tptr, flash_buf
        movf    ibase_lo, W
        movwf   TBLPTRL
        movf    ibase_hi, W
        movwf   TBLPTRH
#if XSTORE == ENABLE
        movf    ibase_up, W
        movwf   TBLPTRU
#endif
        return
magic:
        movlw   h'55'
        movwf   EECON2, BANKED
        movlw   h'aa'
        movwf   EECON2, BANKED
        bsf     EECON1, WR, BANKED
        return

;***************************************************
asmemit:
#if UART == 1
#ifndef K42
        btfss   PIR1, TXIF
        bra     asmemit
        movwf   TXREG
#else 
        banksel PIR3
        btfss   PIR3, U1TXIF, BANKED
        bra     asmemit
        banksel U1TXB
        movwf   U1TXB, BANKED
#endif 
#else ; UART == 2
#ifndef K42
        btfss   PIR3, TX2IF
        bra     asmemit
        banksel TXREG2
        movwf   TXREG2, BANKED
#else 
#ifdef K83
        banksel PIR7
        btfss   PIR7, U2TXIF, BANKED
#else
        banksel PIR6
        btfss   PIR6, U2TXIF, BANKED
#endif
        bra     asmemit
        banksel U2TXB
        movwf   U2TXB, BANKED
#endif 
#endif
        return
;***************************************************
; N=    c-addr nfa u -- n   string:name cmp
;             n=0: s1==s2, n=c-addr: s1!=s2
; N= is specificly used for finding dictionary entries
; It can also be used for comparing strings shorter than 16 characters,
; but the first string must be in ram and the second in flash memory.
        dw      L_RX1Q
L_NEQUAL:
        db      NFA|2,"n="
NEQUAL:
        movf    Sminus, W           ; count_hi
        movf    Sminus, W
        movwf   PCLATH              ; count_lo
        rcall   TOTBLP              ; nfa in flash
        rcall   ICFETCH1            ; ICFETCH1 uses Tp, Tbank (=FSR1)
        movf    Sminus, W
        movf    Sminus, W
        andlw   NFAmask             ; MASK NFA, IMMED, INLINE, COMPILE BITS
        xorwf   PCLATH, W
        bnz     NEQUAL_TRUE         ; NO MATCH
        call    ONEPLUS
NEQUAL0:
        call    CFETCHPP
        rcall   ICFETCH1
        movf    Sminus, W
        movf    Sminus, W
        movf    Sminus, F
        xorwf   Sminus, W
        bnz     NEQUAL_TRUE
NEQUAL1:                            ; check next character
        decfsz  PCLATH, F
        bra     NEQUAL0
NEQUAL2:
        clrf    Sminus
        clrf    Splus
NEQUAL_TRUE:
        return

; SKIP   c-addr u c -- c-addr' u'
;                          skip matching chars
; u (count) must be smaller than 256
        dw      L_NEQUAL
L_SKIP:
        db      NFA|4,"skip"
SKIP:
        rcall   TOTBLP          ; c character
        movf    Sminus, W       ; skip count_hi
        movf    Sminus, W
        movwf   PCLATH          ; count_lo
        movf    Sminus, W
        movwf   Tbank
        movf    Sminus, W
        movwf   Tp              ; c-addr
        movf    PCLATH, W
        bz      SKIP4           ; zero flag comes from the previous movf
SKIP0:
        movlw   0x9             ; SKIP TAB
        subwf   Trw, W
        bz      SKIP1

        movf    Trw, W
        subwf   TBLPTRL, W
        bnz     SKIP4
SKIP1:                          ; check next character
        movf    Tplus, W

        decfsz  PCLATH, F
        bra     SKIP0
        swapf   Tminus, W
SKIP4:
                                ; found start of word
                                ; restore the stack
        movf    Tp, W
        movwf   plusS
        movf    Tbank, W
        iorlw   high(PRAM)
        movwf   plusS
        movf    PCLATH, W
        movwf   plusS
        clrf    plusS
        return

; SCAN   c-addr u c -- c-addr' u'
;                          find matching chars
; u(count) must be smaller than 256
; TAB will always give a match. This works OK
; with tabbed source files that have space as a delimiter.
; When using scan with other delimiters there may be 
; problems, because TAB will always terminate the scan.

        dw      L_SKIP
L_SCAN:
        db      NFA|4,"scan"
SCAN:
        rcall   TOTBLP              ; c character
        movf    Sminus, W           ; count_hi
        movf    Sminus, W           ; count_lo
        movwf   PCLATH
        bz      SCAN4
        movf    Sminus, W
        movwf   Tbank
        movf    Sminus, W
        movwf   Tp                  ; c-addr
SCAN0:
        movf    Trw, W
        subwf   TBLPTRL, W
        bz      SCAN3               ; Found a match
SCAN1:
        movf    Trw, W
        sublw   h'9'                ; Check for TAB
        bz      SCAN3               ; TAB is handled as the delimiter c .
SCAN2:
        movf    Tplus, W            ; check next character
        decfsz  PCLATH, F
        bra     SCAN0
SCAN3:                              ; found start of word
                                    ; restore the stack
        movf    Tp, W
        movwf   plusS
        movf    Tbank, W
        iorlw   high(PRAM)
        movwf   plusS
SCAN4:
        movf    PCLATH, W
        movwf   plusS
        clrf    plusS
        return

; ei  ( -- )    Enable interrupts
        dw      L_SCAN
L_EI:
        db      NFA|INLINE|2,"ei"
        bsf     INTCON, GIE
        return
        
; di  ( -- )    Disable interrupts
        dw      L_EI
L_DI:
        db      NFA|INLINE|2,"di"
        bcf     INTCON, GIE
        return
        
; ;i  ( -- )    End definition of user interrupt routine
        dw      L_DI
L_IRQ_SEMI:
        db      NFA|IMMED|2,";i"
IRQ_SEMI:
        rcall   LIT
        dw      0x11      ; retfie 1
        rcall   ICOMMA
        goto    LEFTBRACKET
        
; INT!   addr vector --     Store the interrupt vector
        dw      L_IRQ_SEMI
L_INT:
        db      NFA|4,"int!"
INT:
        call    DROP
        movf    Sminus, W
        movff_	Sminus, irq_v
        movwf   irq_v+1
        return

; LITERAL  x --           compile literal x as native code
        dw      L_INT
L_LITERAL:
        db      NFA|IMMED|7,"literal"
LITERAL:
        movf    Sminus, W
        movff_   Srw, Tp
        movwf   Srw
        movf    Tp, W
        movwf   plusS
        rcall   LITER0
LITER0:
        movf    Srw, W
        bz      LITER2
        movlw   h'ff'
        cpfslt  Srw
        bra     LITER3
LITER1:
        movlw   h'0e'            ; movlw literal
        movwf   plusS
        rcall   ICOMMA
        rcall   LIT
        dw      h'6eec'          ; movwf plusS
        rcall   ICOMMA
        bra     LITER5
LITER2:
        rcall   LIT
        dw      h'6aec'          ; clrf plusS
        bra     LITER4
LITER3:
        rcall   LIT
        dw      h'68ec'          ; setf plusS
LITER4:
        rcall   ICOMMA
        movf    Sminus, W
LITER5:
        return

;**************************************************
;   INSTRUCTION MEMORY INSTRUCTIONS
FETCHLIT:
        movwf   TBLPTRH
pfetch0:
        tblrd*+
        movf    TABLAT, W
        movwf   plusS
        tblrd*+
        movf    TABLAT, W
        movwf   plusS
        return
pcfetch0:
        tblrd*+
        movf    TABLAT, W
        movwf   plusS
        clrf    plusS
        return
        
ISTORE_SETUP:
        rcall   LOCKED
; check that writes are not to the kernel code
        rcall   ISTORECHK
;check if program memory row is already in buffer
        movf    Sminus, W
        movwf   iaddr_hi
        movf    Sminus, W
        movwf   iaddr_lo
        rcall   iupdatebuf
; set write address
        movf    iaddr_lo, W
        andlw   flash_block_mask;0x3f
        lfsr    Tptr, flash_buf
        addwf   Tp, F
        return

; I!       x a-addr --    store cell in Code mem
ISTORE:
        rcall   ISTORE_SETUP
        movf    Tplus, W
        movf    Sminus, W
        movwf   Tminus
        bra     ICSTORE1

; IC!       x addr --    store byte in Code mem
ICSTORE:
        rcall   ISTORE_SETUP
        swapf   Sminus, W
ICSTORE1:
        movf    Sminus, W
        movwf   Trw
;mark_buffer_dirty
        bsf     FLAGS1, idirty
        return

; >TBLP  x --
        dw      L_LITERAL
L_TOTBLP:
        db      NFA|5,">tblp"
TOTBLP:
        movf    Sminus, W
        movwf   TBLPTRH
        movff_   Sminus, TBLPTRL
        return
; I@       a-addr -- x  fetch cell from Code mem
; 25 cycles when fetching from buffer
; 18-22 cycles when pfetching directly from flash
IFETCH:
        rcall   TOTBLP
        cpfseq  ibase_hi       ; compare with W
        bra     pfetch0
        movlw   flash_pointer_mask
        andwf   TBLPTRL, W
        cpfseq  ibase_lo
        bra     pfetch0
;read_cell_from_buffer
        movf    TBLPTRL, W
        andlw   flash_block_mask;0x3f
        lfsr    Tptr, flash_buf
        addwf   Tp, F
        goto    FETCH2

;  IC@      addr -- x  fetch char from Code mem
ICFETCH:
        rcall   TOTBLP
ICFETCH1:                       ; Called directly by N=
        movf    TBLPTRH, W
        cpfseq  ibase_hi
        bra     pcfetch0
        movlw   flash_pointer_mask 
        andwf   TBLPTRL, W
        cpfseq  ibase_lo
        bra     pcfetch0
;read_byte_from_buffer
        movf    TBLPTRL, W
        andlw   flash_block_mask
        lfsr    Tptr, flash_buf
        addwf   Tp, F
        tblrd*+                 ; To satisfy optimisation in N=
        goto    CFETCH2

; E!      x a-addr --    store cell in data EEPROM
ESTORE:
        rcall   LOCKED
        movf    Sminus, W
        banksel EEADR
#ifdef EEADRH
        movwf   EEADRH, BANKED
#endif
        movf    Sminus, W
        movwf   EEADR, BANKED
        incf    EEADR, F, BANKED
        movf    Sminus, W
        movwf   EEDATA, BANKED
        rcall   ECSTORE1
        decf    EEADR, F, BANKED
        movf    Sminus, W
        movwf   EEDATA, BANKED
        bra     ECSTORE1

; EC!       c addr --    store char in data EEPROM
ECSTORE:
        rcall   LOCKED
        movf    Sminus, W
        banksel EEADR
#ifdef EEADRH
        movwf   EEADRH, BANKED
#endif
        movf    Sminus, W
        movwf   EEADR, BANKED
        movf    Sminus, W
        movf    Sminus, W
        movwf   EEDATA, BANKED
ECSTORE1:
        bcf     EECON1, EEPGD, BANKED
        bcf     EECON1, CFGS, BANKED
        bsf     EECON1, WREN, BANKED
        bcf     INTCON, GIE
        movlw   h'55'
        movwf   EECON2, BANKED
        movlw   h'aa'
        movwf   EECON2, BANKED
        bsf     EECON1, WR, BANKED
        bsf     INTCON, GIE
ECSTORE2:
        btfsc   EECON1, WR, BANKED
        bra     ECSTORE2
        bcf     EECON1, WREN, BANKED
        return


; E@       a-addr -- x  fetch cell from data EEPROM
EFETCH:
        movf    Sminus, W
        banksel EEADR
#ifdef EEADRH
        movwf   EEADRH, BANKED
#endif
        movf    Sminus, W
        movwf   EEADR, BANKED
        rcall   asmecfetch
        incf    EEADR, F, BANKED
        bra     asmecfetch

; EC@      addr -- c  fetch char from data EEPROM
ECFETCH:
        movf    Sminus, W
        banksel EEADR
#ifdef EEADRH
        movwf   EEADRH, BANKED
#endif
        movf    Sminus, W
        movwf   EEADR, BANKED
        rcall   asmecfetch
        clrf    plusS
        return
asmecfetch:
#ifdef p18fxx2xx8_fix_1
        bcf     INTCON, GIE          ; 18f252 ERRATA
#endif
        bcf     EECON1, EEPGD, BANKED
        bcf     EECON1, CFGS, BANKED
        bsf     EECON1, RD, BANKED
        movf    EEDATA, W, BANKED
        movwf   plusS
#ifdef p18fxx2xx8_fix_1
        bsf     INTCON, GIE          ; 18f252 ERRATA
#endif
        return

;;; Disable writes to flash and eeprom
        dw      L_TOTBLP
L_FLOCK:
        db      NFA|3,"fl-"
        bsf     FLAGS1, fLOCK
        return

;;; Enable writes to flash and eeprom
        dw      L_FLOCK
L_FUNLOCK:
        db      NFA|3,"fl+"
        bcf     FLAGS1, fLOCK
        return

;;; Enable flow control
        dw      L_FUNLOCK
L_FCON:
        db      NFA|3,"u1+"
        bcf     FLAGS2, fFC
        return

;;; Disable flow control
        dw      L_FCON
L_FCOFF:
        db      NFA|3,"u1-"
        bsf     FLAGS2, fFC
        return

;;; Clear watchdog timer
        dw      L_FCOFF
L_CWD:
        db      NFA|INLINE|3,"cwd"
        clrwdt
        return

; VALUE
        dw      L_CWD
L_VALUE:
        db      NFA|5,"value"
VALUE:
        call    CREATE
        rcall   COMMA
        call    XDOES
VALUE_DOES:
        rcall   DODOES
        goto    FETCH

; DEFER
        dw      L_VALUE
L_DEFER:
        db      NFA|5,"defer"
;DEFER:
        call    CREATE
        rcall   LIT
        dw      ABORT
        rcall   COMMA
        call    XDOES
DEFER_DOES:
        rcall   DODOES
        goto    FEXECUTE

; TO
        dw      L_DEFER
L_TO:
        db      NFA|IMMED|2,"to"
TO_:
        goto    IS

; IS
        dw      L_TO
L_IS:
        db      NFA|IMMED|2,"is"
IS:
        call    TICK
        call    TOBODY
        rcall   FETCH
        movf    state, W
        bz      IS1
        rcall   LITERAL
        rcall   DOCOMMAXT
        dw      STORE
        bra     IS2
IS1:
        rcall   STORE
IS2:
        return

        dw      L_IS
L_TURNKEY:
        db      NFA|7,"turnkey"
TURNKEY:
        call    VALUE_DOES      ; Must be call for IS to work.
        dw      dpSTART+PRAM

;;; *******************************************************
; PAUSE  --     switch task
;;;  38 us @ 12 MHz, 11,4 us @ 40 Mhz  9us at 48 Mhz  ( with 4 cells on return stack )
;;; save stack to current uarea, link -> up, restore stack
        dw      L_TURNKEY
L_PAUSE
        db      NFA|5,"pause"
PAUSE:
        clrwdt
#ifdef USB_CDC
        movf    upcurr, W
        sublw   low(u0)        ; check usb only in operator task
        bnz     PAUSE_USB_CDC_END ; Lowers execution cpu load when many tasks are running
        btfss   UCON, USBEN
        bra     PAUSE_USB_CDC_END
        call    USBDriverService
        movf    ep2icount, W
        bz      PAUSE_USB_CDC_END
        movf    ep2itmo, W
        subwf   ms_count, W
        bn      PAUSE_USB_CDC_END
        rcall   TX0_SEND
PAUSE_USB_CDC_END:
#endif
#ifdef IDLEN
#if IDLE_MODE == ENABLE
        movf    fstatus, W     ; idle allowed ?
        bz      PAUSE_IDLE1
        movf    upcurr, W
        sublw   low(u0)        ; Sleep only in operator task
        bnz     PAUSE000       ; Lowers execution delay when many tasks are running
#if CPU_LOAD_LED == ENABLE
#if CPU_LOAD_LED_POLARITY == POSITIVE
        bcf     CPU_LOAD_PORT, CPU_LOAD_BIT
#else
        bsf     CPU_LOAD_PORT, CPU_LOAD_BIT
#endif
#endif
#if CPU_LOAD == ENABLE
#ifndef K42
        bcf     T0CON, TMR0ON   ; TMR0 Restart in interrupt routine
#else
        bcf     T0CON0, T0EN   ; TMR0 Restart in interrupt routine
#endif
#endif
        sleep
PAUSE_IDLE1:
#if CPU_LOAD_LED == ENABLE
#if CPU_LOAD_LED_POLARITY == POSITIVE
        bsf     CPU_LOAD_PORT, CPU_LOAD_BIT
#else
        bcf     CPU_LOAD_PORT, CPU_LOAD_BIT
#endif
#endif
#endif
#endif
PAUSE000:
        ; Set user pointer in Tp, Tbank (FSR1
        movf    upcurr, W
        movwf   Tp
        movf    (upcurr+1), W
        movwf   Tbank

;; Switch tasks only if some background tasks are running
        movf    Tminus, W
        movf    Tplus, W
        bz      PAUSE_RET

        ; Save parameter stack pointer
        movf    Sp, W
        movwf   plusR
        movf    Sbank, W
        movwf   plusR

        ; Save P pointer
        movf    p_lo, W
        movwf   plusR
        movf    p_hi, W
        movwf   plusR

        ; Remember the return stack counter
        movf    STKPTR, W
        movwf   TBLPTRL

        ; Save the return stack
pause1:
        movf    TOSL, W
        movwf   plusR
        movf    TOSH, W
        movwf   plusR
        decfsz  STKPTR, F
        bra     pause1

        ; Save the return stack counter
        movf    TBLPTRL, W
        movwf   plusR

        ; Save the saved return stack pointer urptr
        movf    Rp, W
        movwf   Tplus
        movf    Rbank, W
        movwf   Tminus

        ; Move to the next user area
        movf    Tminus, W
        movf    Tminus, W
        movwf   (upcurr+1)
        movf    Tminus, W
        movwf   (upcurr)

        ; Put new user pointer in Tp, Tbank
        movf    upcurr, W
        movwf   Tp
        movf    (upcurr+1), W
        movwf   Tbank

        ; Set the return stack restore pointer  in Rp
        movf    Tplus, W
        movwf   Rp
        movf    Tminus, W
        movwf   Rbank

        ; Set the return stack counter
        movf    Rminus, W
        movwf   TBLPTRL

        ; Restore the return stack
pause2:
        push
        movf    Rminus, W
        movwf   TOSH
        movf    Rminus, W
        movwf   TOSL

        decfsz  TBLPTRL, F
        bra     pause2

        ; Restore the P pointer
        movf    Rminus, W
        movwf   p_hi
        movf    Rminus, W
        movwf   p_lo

        ; Restore the parameter stack pointer
        movf    Rminus, W
        movwf   Sbank
        movf    Rminus, W
        movwf   Sp
PAUSE_RET:
        return

#ifdef USB_CDC
;***************************************************
; TX0  c --    output character to the USB serial emulation
        dw      L_PAUSE
L_TX0:
        db      NFA|3,"txu"
TX0:
        rcall   PAUSE
        banksel ep2istat
        btfss   usb_device_state, 3, BANKED
        bra     TX0_DROP
        btfsc   ep2istat, 7, BANKED
        bra     TX0
        movf    ms_count, W
        addlw   2+TICK_TIME
        movwf   ep2itmo
        lfsr    Tptr, cdc_data_tx
        movf    Sminus, W
        movf    ep2icount, W
        movff_  Sminus, TWrw
        addlw   1
        movwf   ep2icount
        sublw   6                       ; l - w
        bnn     TX0_END
TX0_SEND:
        movf    ep2icount, W
TX0_SEND2:
        movwf   ep2icnt, BANKED
        movlw   0xc8
        btfsc   ep2istat, 0x6, BANKED
        movlw   0x88
        movwf   ep2istat, BANKED
        clrf    ep2icount
        bra     TX0_END
TX0_DROP:
        call    DROP
TX0_END:
        return
;***************************************************
; KEY   -- c    get character from the USB line
        dw      L_TX0
L_RX0:
        db      NFA|3,"rxu"
RX0:
        rcall   PAUSE
        banksel ep2ostat
        btfsc   usb_device_state, 3, BANKED
        btfsc   ep2ostat, 7, BANKED
        bra     RX0
        lfsr    Tptr, cdc_data_rx
        movf    ep2optr, W
        movff_  TWrw, plusS
        incf    ep2optr, F
        decf    ep2ocnt, F, BANKED
        bnz     RX0_END
        movlw   8
        movwf   ep2ocnt, BANKED
        movlw   h'C8'
        btfsc   ep2ostat, 6, BANKED
        movlw   h'88'
        movwf   ep2ostat, BANKED
        clrf    ep2optr
RX0_END:
        clrf    plusS
        return
;***************************************************
; KEY?  -- f    return true if a char is waiting
        dw      L_RX0
L_RX0Q:
        db      NFA|4,"rxu?"
RX0Q:
        banksel ep2ostat
        btfsc   usb_device_state, 3, BANKED
        btfsc   ep2ostat, 7, BANKED
RX0Q1:
        goto    FALSE_
        goto    TRUE_
#endif
; ***************************************************
#if FC_TYPE_SW == ENABLE
XXOFF:
        btfsc   FLAGS2, ixoff
        return
XXXOFF: 
        bsf     FLAGS2, ixoff
        movlw   XOFF_
        bra     asmemit
XXON:
        btfss   FLAGS2, ixoff
        return
XXXON:  
        bcf     FLAGS2, ixoff
        movlw   XON_
        bra     asmemit
#endif
;****************************************************
;if (idirty)
;   writebuffer_to_imem
;endif
#ifdef USB_CDC
        dw      L_RX0Q
#else
        dw      L_PAUSE
#endif
L_IFLUSH:
        db      NFA|6,"iflush"
IFLUSH:
        btfsc   FLAGS1, idirty
        bra     write_buffer_to_imem
        return

; *********************************************
; Bit masking 8 bits, only for ram addresses !
; : mset ( mask addr -- )
;   dup >r c@ swap or r> c!
; ;
        dw      L_IFLUSH
L_MSET:
        db      NFA|4,"mset"
MSET:
        movf    Sminus, W
        movwf   Tbank
        movf    Sminus, W
        movwf   Tp
        movf    Sminus, W
        movf    Sminus, W
        iorwf   Trw, F
        return
        
; : mclr  ( mask addr -- )
;  dup >r c@ swap invert and r> c!
; ;
        dw      L_MSET
L_MCLR:
        db      NFA|4,"mclr"
MCLR_:
        movf    Sminus, W
        movwf   Tbank
        movf    Sminus, W
        movwf   Tp
        movf    Sminus, W
        comf    Sminus, W
        andwf   Trw, F
        return

; Print restart reason
RQ:
        call    CR
RQ_DIVZERO:
        btfss   2, 0
        bra     RQ_STKFUL
        call    XSQUOTE
        db      d'1',"M"
        call    TYPE
RQ_STKFUL:
        btfss   0, 7
        bra     RQ_STKUNF
        call   XSQUOTE
        db      d'1',"O"
        rcall   TYPE
RQ_STKUNF:
        btfss   0, 6
        bra     RQ_BOR
        call    XSQUOTE
        db      d'1',"U"
        rcall   TYPE
RQ_BOR:
        btfsc   1, BOR
        bra     RQ_POR
        call    XSQUOTE
        db      d'1',"B"
        rcall   TYPE
RQ_POR: 
        btfsc   1, POR
        bra     RQ_TO
        rcall   XSQUOTE
        db      d'1',"P"
        rcall   TYPE
RQ_TO:
#ifndef K42
        btfsc   1, TO
#else
        btfsc   1, RWDT
#endif 
        bra     RQ_RI
        rcall   XSQUOTE
        db      d'1',"W"
        rcall   TYPE
RQ_RI:
        btfsc   1, RI
        bra     RQ_END
        rcall   XSQUOTE
        db      d'1',"S"
        rcall   TYPE
RQ_END:
        return

; : mtst ( mask addr -- flag )
;   c@ and 
; ;
        dw      L_MCLR
L_MTST:
        db      NFA|4,"mtst"
MTST:
        rcall   CFETCH
        goto    AND

;;; Fcy returns the cpu clock Fcy in KHz. Unsigned value. 
        dw      L_MTST
L_CPUCLK:
        db      NFA|3,"Fcy"
        rcall   DOCREATE
        dw      clock / d'4000'

        dw      L_CPUCLK
L_OPERATOR:
        db      NFA|8,"operator"
OPERATOR:       
        call    DOCREATE        ; Must be a call !
        dw      OPERATOR_AREA
OPERATOR_AREA:  
        dw      u0+PRAM         ; User pointer
        dw      UADDSIZE, ursize
        dw      ussize, utibsize

; I,   x --             append cell to Flash
;   IHERE ! 1 CELLS IALLOT ;
        dw      L_OPERATOR
L_ICOMMA:
        db      NFA|2,"i,"
ICOMMA:
        rcall   IHERE_P
        rcall   STORE
        rcall   CELL
        goto    IALLOT

;   IHERE ! 1 CHARS IALLOT ;
        dw      L_ICOMMA
L_ICCOMMA:
        db      NFA|3,"ic,"
ICCOMMA:
        rcall   IHERE_P
        rcall   CSTORE
        rcall   ONE
        goto    IALLOT

;   LSHIFT      x1 u -- x2
        dw      L_ICCOMMA
L_LSHIFT:
        db      NFA|6,"lshift"
LSHIFT:
        swapf   Sminus, W
        movf    Sminus, W
        bz      LSHIFT2
        movwf   Tp
        swapf   Sminus, W
LSHIFT1:
        bcf     STATUS, C
        rlcf    Splus, F
        rlcf    Sminus, F

        decfsz  Tp, F
        bra     LSHIFT1
        swapf   plusS, W
LSHIFT2:
        return

;   RSHIFT      x1 u -- x2
        dw      L_LSHIFT
L_RSHIFT:
        db      NFA|6,"rshift"
RSHIFT:
        swapf   Sminus, W
        movf    Sminus, W
        bz      RSHIFT2
        movwf   Tp
RSHIFT1: 
        bcf     STATUS, C
        rrcf    Sminus, F
        rrcf    Splus, F    

        decfsz  Tp, F
        bra     RSHIFT1
RSHIFT2:        
        return
;*******************************************************
; Assembler
;*******************************************************
;: as0 ( opcode "name" -- ) ( -- )
;  co:
;  does> i, ;
;
;: as2 ( opcode "name" -- ) ( f a -- )
;  co:
;  does> rot ic, or ic, ;

;       as1 ( opcode "name" -- ) ( k -- )
        dw      L_RSHIFT
L_AS1:
        db      NFA|3,"as1"
AS1:
        rcall   CONSTANT_
        call    XDOES
AS1_DOES:
        rcall   DODOES
AS1_1:  
        rcall   OR_A
        bra     ICOMMA

;       as2 ( opcode "name" -- ) ( f a -- )
        dw      L_AS1
L_AS2:
        db      NFA|3,"as2"
AS2:
        rcall   CONSTANT_
        call    XDOES
AS2_DOES:
        rcall   DODOES
        rcall   ROT
        rcall   ICCOMMA
        bra     AS3_2

;       as3 ( opcode "name" --) ( f d/b a -- )  
;       write a 3 operand asm intruction to flash
        dw      L_AS2
L_AS3:
        db      NFA|IMMED|3,"as3"
AS3:    
        rcall   CONSTANT_
        call    XDOES
AS3_DOES:
        rcall   DODOES          ;  f d/b a opcode
        rcall   TOR             ;  f d/b A
        rcall   ROT             ;  d/b a f
        rcall   ICCOMMA         ;  d/b a
        rcall   SWOP            ;  a d/b
        call    TWOSTAR
        rcall   OR_A
        rcall   RFROM
AS3_2:  
        rcall   OR_A
        bra     ICCOMMA

;       br2 ( opcode "name" -- ) ( rel-addr -- ) \ bra and rcall
        dw      L_AS3
L_BR2:
        db      NFA|3,"br2"
BR2:
        rcall   CONSTANT_
        call    XDOES
BR2_DOES:
        rcall   DODOES
        rcall   SWOP            ; opcode rel-addr
        rcall   LIT
        dw      h'0fff'         ; opcode rel-addr limit
        call    BRQ             ; opcode clipped-rel-addr
        bra     AS1_1

;       br3 ( opcode "name" -- ) ( abs-addr -- ) \ goto and call
        dw      L_BR2
L_BR3:
        db      NFA|3,"br3"
BR3:
        rcall   CONSTANT_
        call    XDOES
BR3_DOES:
        rcall   DODOES          ; abs-addr opcode
        rcall   TOR             ; abs-addr
        rcall   LIT
        dw      h'01'
        rcall   RSHIFT          ; abs-addr
        rcall   DUP
        rcall   LIT             ; abs-addr abs-addr ff
        dw      h'ff'
        call    AND
        rcall   RFROM
        rcall   OR_A
        rcall   ICOMMA
        rcall   LIT
        dw      h'08'
        rcall   RSHIFT
        rcall   LIT
        dw      h'f000'
        bra     AS1_1

;       goto, ( abs-addr -- )
        dw      L_BR3
L_GOTO:
        db      NFA|5,"goto,"
GOTO_:
        rcall   BR3_DOES
        dw      h'ef00'

;       call, ( abs-addr -- )
        dw      L_GOTO
L_CALL:
        db      NFA|5,"call,"
CALL_:
        rcall   BR3_DOES
        dw      h'ec00'

;       rcall, ( rel-addr -- )
        dw      L_CALL
L_RCALL:
        db      NFA|6,"rcall,"
RCALL_:
        rcall   BR2_DOES
        dw      h'd800'

;       bra, ( rel-addr -- )
        dw      L_RCALL
L_BRA:
        db      NFA|4,"bra,"
BRA_:
        rcall   BR2_DOES
        dw      h'd000'
        
;       bcf, ( f b a -- )
        dw      L_BRA
L_BCF:
        db      NFA|4,"bcf,"
BCF_:
        rcall   AS3_DOES
        dw      h'0090'
        
;       bsf, ( f b a -- )
        dw      L_BCF
L_BSF:
        db      NFA|4,"bsf,"
BSF_:
        rcall   AS3_DOES
        dw      h'0080'
        
;       btfsc, ( f b a -- )
        dw      L_BSF
L_BTFSC:
        db      NFA|6,"btfsc,"
BTFSC_:
        rcall   AS3_DOES
        dw      h'00b0'
        
;       btfss, ( f b a -- )
        dw      L_BTFSC
L_BTFSS:
        db      NFA|6,"btfss,"
BTFSS_:
        rcall   AS3_DOES
        dw      h'00a0'

;;;
LOCKED:
        btfss   FLAGS1, fLOCK
        return
        bra     ISTORERR
;******************************************************
        dw      L_BTFSS
L_EMPTY:
        db      NFA|5,"empty"
EMPTY:
        rcall   LIT
        dw      STARTV
        rcall   LIT
        dw      dp_start
        rcall   LIT
        dw      h'000c'
        call    CMOVE
        goto    DP_TO_RAM

        dw      L_EMPTY
#ifdef USB_CDC
L_USB_ON:
        db      NFA|4,"usb+"
USB_ON:
        btfsc   UCON, USBEN
        bra     USB_ON_RET
#ifdef USB_SPEED
#if USB_SPEED == 0
        movlw   0x10
#else
        movlw   0x14
#endif
#endif
        movwf   UCFG
        clrf    UCON,
        bsf     UCON, USBEN
        clrf    UIR
        banksel usb_device_state
        clrf    usb_device_state, BANKED
        clrf    ep2icount
USB_ON_RET:
        return
;*******************************************************
        dw      L_USB_ON
L_USB_OFF:
        db      NFA|5,"usb-?"
USB_OFF:
        bsf     UCON, SUSPND
        clrf    UCON
        banksel usb_device_state
        clrf    usb_device_state, BANKED
        return
;*******************************************************
        dw      L_USB_OFF
#endif
L_WARM:
        db      NFA|4,"warm"
WARM_:  
#ifdef USB_CDC
        goto    main
#else
        reset                   ; Perform a reset, jumps to h'0000' and resets stuff
#endif
main:
#ifndef K42
        movlw   0xf
        iorwf   ADCON1, F
#endif 

        clrf    TBLPTRU
#ifdef OSCCON
        movlw   0x70            ; Use full internal OSC frequency
        movwf   OSCCON
#endif
#ifdef PLL
#if PLL == ENABLE
        movlw   0x40
        movwf   OSCTUNE
#endif
#endif
                                ; Clear ram
WARM:
#ifndef K42
        movf    STKPTR, W
        movwf   0               ; Save return stack reset reasons
        movf    RCON, W
        movwf   1               ; Save reset reasons
        movf    fstatus, W
        movwf   2
        movlw   h'9f'
        movwf   RCON
#else
        movf    PCON0, W
        movwf   0               ; Save reset reasons
        movwf   1               ; Save reset reasons
        movf    fstatus, W
        movwf   2
        movlw   h'3f'           ; Clearing the flags in PCON0
        movwf   PCON0
#endif
        clrf    STKPTR       ; Clear return stack

        lfsr    Sptr, h'10'         ; Zero ram from h'10' upwards
#ifdef USB_CDC
        lfsr    Tptr, usb_device_state+1
WARM_ZERO_1:
        clrf    Splus
        movf    Sbank, W
        subwf   Tbank, W
        bnz     WARM_ZERO_1
WARM_ZERO_3:
        clrf    Tplus
        movf    Tbank, W
        sublw   h'0f'
        bnz     WARM_ZERO_3
#else
WARM_ZERO_1:
        clrf    Splus
        movf    Sbank, W
        sublw   h'0f'
        bnz     WARM_ZERO_1
#endif

        setf    ibase_hi           ; Mark flash buffer empty
#ifndef K42
        lfsr    Sptr, (usbuf-1)       ; Initalise Parameter stack
        lfsr    Rptr, urbuf
#else
        movlw   low(usbuf-1)
        movwf   Sp
        movlw   high(usbuf-1)
        movwf   Sbank
        movlw   low(urbuf)
        movwf   Rp
        movlw   high(urbuf)
        movwf   Rbank
#endif
        banksel PIE1            
#ifdef PIE0                     ; Disable all peripheral interrupts
        clrf    PIE0, BANKED
#endif
        clrf    PIE1, BANKED    ; Disable all peripheral interrupts
        clrf    PIE2, BANKED
#ifdef PIE3
        clrf    PIE3, BANKED
#endif
#ifdef PIE4
        clrf    PIE4, BANKED
#endif
#ifdef PIE5
        clrf    PIE5, BANKED
#endif
#ifdef PIE6
        clrf    PIE6, BANKED
#endif
#ifdef PIE7
        clrf    PIE7, BANKED
#endif 
#ifdef PIE8
        clrf    PIE8, BANKED
#endif
#ifdef PIE9
        clrf    PIE9, BANKED
#endif
#ifdef PIE10
        clrf    PIE10, BANKED
#endif
        banksel TOSU   ; SFR bank(16)
#ifndef K42
#if UART == 1 ; ----------------------------------------------
#ifdef BRG16
        bsf     BAUDCON, BRG16
        movlw   high(spbrgvalx4)
        movwf   SPBRGH
        movlw   low(spbrgvalx4)
#else
        movlw   spbrgval
#endif
        movwf   SPBRG
; TX enable
        movlw   b'00100100'
        movwf   TXSTA
#ifdef USB_CDC
        movlw   b'00000000'  ; Reset the UART since
        movwf   RCSTA        ; USB warm start does not reset the chip
#endif
; RX enable
        movlw   b'10010000'
        movwf   RCSTA
        bsf     PIE1, RCIE
        bcf     IPR1, RCIP
#ifdef ANSELH
#ifdef ANS11
        bcf     ANSELH, ANS11 ; Enable digital RB5 for RX
#endif
#endif
#ifdef ANSELC
#ifdef ANSC7
        bcf     ANSELC, ANSC7, BANKED   ; Enable digital RC7 for RX
#endif
#endif
#else  ; UART == 2 ---------------------------------------
#ifdef BRG16
        bsf     BAUDCON2, BRG16
        movlw   high(spbrgvalx4)
        movwf   SPBRGH2
        movlw   spbrgvalx4
#else
        movlw   spbrgval
#endif
        movwf   SPBRG2, BANKED
; TX enable
        movlw   b'00100100'
        movwf   TXSTA2, BANKED
; RX enable
        movlw   b'10010000'
        movwf   RCSTA2, BANKED
        bsf     PIE3, RC2IE
        bcf     IPR3, RC2IP

        bcf     ANCON2, ANSEL18, BANKED ; Enable digital RG2 for RX2 (18F87K22 family)
                                        ; Other values may be needed depending on chip
#endif
#else  ; K42 K83
#if UART == 1 ; ----------------------------------------------
; PPS configure pins for RX and TX
        banksel RX_ANSEL
        bcf     RX_ANSEL, RX_BIT, BANKED    ; disable analogue on PORTx so RX can function
        bcf     TX_ANSEL, TX_BIT, BANKED    ; disable analogue on PORTx so TX can function
        bcf     TX_TRIS, TX_BIT
        bsf     TX_LAT, TX_BIT
; Unlock the PPS
        banksel PPSLOCK         ; required sequence
        movlw   h'55'
        movwf   PPSLOCK, BANKED
        movlw   h'AA'
        movwf   PPSLOCK, BANKED
        bcf     PPSLOCK, PPSLOCKED, BANKED  ; disable the pps lock
; Set the pins
        movlw   RX_PPS 
        movwf   U1RXPPS, BANKED
        
        movlw   b'00000000'
        movwf   U1CTSPPS, BANKED
        
        movlw   b'00010011'     ; TX1
        movwf   TX_PPS, BANKED

; Re-lock the PPS
        movlw   h'55'
        movwf   PPSLOCK, BANKED
        movlw   h'AA'
        movwf   PPSLOCK, BANKED
        bsf     PPSLOCK, PPSLOCKED, BANKED  ; enable the pps lock

; Set the Baud Rate
        movlw   low(spbrgvalx4)        ; ((clock/baud)/d'4') - 1
        banksel U1BRGL
        movwf   U1BRGL, BANKED
        movlw   high(spbrgvalx4)
        movwf   U1BRGH, BANKED

; TX enable
        movlw   b'10110000'     ; HIGH SPEED BAUD RATE / NO AUTO DETECT BOARD / 
                                ; ENABLE TX / ENABLE RX / ASYNC 8 BIT MODE
        movwf   U1CON0, BANKED
        bsf     U1CON2, RUNOVF, BANKED
        bsf     U1CON1, ON_U1CON1, BANKED   ; turn on TX

; RX enable
        banksel PIE3
        bsf     PIE3, U1RXIE, BANKED    ; enable RX interupt
        bcf     IPR3, U1RXIP, BANKED    ; low priority
        bsf     RX_TRIS, RX_BIT         ; configure XY as an input

#else  ; UART == 2 ---------------------------------------

; PPS configure pins for RX and TX
        banksel RX_ANSEL
        bcf     RX_ANSEL, RX_BIT, BANKED    ; disable analogue on PORTx so RX can function
        banksel TX_ANSEL
        bcf     TX_ANSEL, TX_BIT, BANKED    ; disable analogue on PORTx so TX can function
; Unlock the PPS
        banksel PPSLOCK         ; required sequence
        movlw   h'55'
        movwf   PPSLOCK, BANKED
        movlw   h'AA'
        movwf   PPSLOCK, BANKED
        bcf     PPSLOCK, PPSLOCKED, BANKED  ; disable the pps lock
; Set the pins
        movlw   RX_PPS
        movwf   U2RXPPS, BANKED        ; configure the RX pin to XY
        
        banksel U2CTSPPS
        movlw   b'00000000'
        movwf   U2CTSPPS, BANKED        ; clear so always disabled
        
        banksel TX_PPS          ; configure TX pin to XY
        movlw   b'00010110'     ; TX2
        movwf   TX_PPS, BANKED

; Re-lock the PPS
        banksel PPSLOCK         ; required sequence
        movlw   h'55'
        movwf   PPSLOCK, BANKED
        movlw   h'AA'
        movwf   PPSLOCK, BANKED
        bsf     PPSLOCK, PPSLOCKED, BANKED  ; enable the pps lock

; Set the Baud Rate
        movlw   low(spbrgvalx4)        ; ((clock/baud)/d'4') - 1
        banksel U2BRGL
        movwf   U2BRGL, BANKED
        movlw   high(spbrgvalx4)
        movwf   U2BRGH, BANKED

; TX enable
        movlw   b'10110000'     ; HIGH SPEED BAUD RATE / NO AUTO DETECT BOARD / 
                                ; ENABLE TX / ENABLE RX / ASYNC 8 BIT MODE
        movwf   U2CON0, BANKED
        bsf     U2CON2, RUNOVF, BANKED
        bsf     U2CON1, ON_U2CON1, BANKED ; turn on TX

; RX enable
#ifdef K83
        banksel PIE7
        bsf     PIE7, U2RXIE, BANKED    ; enable RX interupt
#else ; K42
        banksel PIE6
        bsf     PIE6, U2RXIE, BANKED    ; enable RX interupt
        bcf     IPR6, U2RXIP, BANKED     ; low priority
#endif
        banksel RX_TRIS
        bsf     RX_TRIS, RX_BIT, BANKED ; configure C7 as an input
#endif
#endif
#ifdef IDLEN
#if IDLE_MODE == ENABLE
#if CPU_LOAD_LED == ENABLE
        bcf     CPU_LOAD_TRIS, CPU_LOAD_BIT
#endif
#ifndef K42
        bsf     OSCCON, IDLEN   ; Only IDLE mode supported
#else
        banksel CPUDOZE
        bsf     CPUDOZE, IDLEN, BANKED ; Only IDLE mode supported  
#endif  
#endif
#endif
#if CPU_LOAD == ENABLE
#ifndef K42
        movlw   h'08'               ; TMR0 used for CPU_LOAD
        movwf   T0CON               ; prescale = 1
        bsf     INTCON, TMR0IE
        bcf     INTCON2, TMR0IP
#else
        bsf     T0CON0, T0MD16      ; 16 bit timer
        movlw   h'40'               ; TMR0 used for CPU_LOAD
        movwf   T0CON1              ; Instruction clock 1:1 
        banksel PIE3 
        bsf     PIE3, TMR0IE, BANKED
        bcf     IPR3, TMR0IP, BANKED; Low priority
#endif
#endif
        movlw   low(tmr1ms_val)
        movwf   tickval
        movlw   high(tmr1ms_val)
        movwf   tickval+1
#if MS_TMR == 1                 ;; Timer 1 for 1 ms system tick
#ifndef K42
        movlw   h'01'           ; Fosc/4,prescale = 1, 8-bit write
        movwf   T1CON
        bsf     PIE1, TMR1IE
        bcf     IPR1, TMR1IP
#else
        movlw   h'01'           ; Fosc/4,prescale = 1, 8-bit write
        movwf   T1CON
        movlw   h'01'           ; fosc/4
        movwf   T1CLK
        banksel PIE4
        bsf     PIE4, TMR1IE, BANKED
        bcf     IPR4, TMR1IP, BANKED
#endif
#endif
#if MS_TMR == 2                 ;; Timer 2 for 1 ms system tick
#ifndef K42
        movlw   h'7d'           ; Prescale = 4, Postscale = 16
        movwf   T2CON
        movlw   tmr2ms_val
        movwf   PR2
        bsf     PIE1, TMR2IE
        bcf     IPR1, TMR2IP
#else
        movlw   h'01'
        movwf   T2CLK
        movlw   tmr2ms_val
        movwf   T2PR
        movlw   h'af'       ; Prescale = 4, Postscale = 16
        movwf   T2CON
        banksel PIE4
        bsf     PIE4, TMR2IE, BANKED
        bcf     IPR4, TMR2IP, BANKED
#endif
#endif
#if MS_TMR == 3                 ;; Timer 3 for 1 ms system tick
#ifndef K42
        movlw   h'01'           ; Fosc/4,prescale = 1, 8-bit write
        movwf   T3CON
        bsf     PIE2, TMR3IE
        bcf     IPR2, TMR3IP
#else
        movlw   h'01'           ; Fosc/4,prescale = 1, 8-bit write
        movwf   T3CON
        bsf     PIE2, TMR3IE
        movlw   h'01'           ; fosc/4
        movwf   T3CLK
        banksel PIE6
        bsf     PIE6, TMR3IE, BANKED
        bcf     IPR6, TMR3IP, BANKED
#endif
#endif
#if MS_TMR == 4                 ;; Timer 4 for 1 ms system tick
#ifndef K42
        movlw   h'7d'           ; Prescale = 4, Postscale = 16
        movwf   T4CON, BANKED
        movlw   tmr2ms_val
        movwf   PR4, BANKED
        bsf     PIE5, TMR4IE
        bcf     IPR5, TMR4IP
#else
        movlw   h'01'
        movwf   T4CLK
        movlw   tmr2ms_val
        movwf   T4PR
        movlw   h'af'       ; Prescale = 4, Postscale = 16
        movwf   T4CON
        banksel PIE7
        bsf     PIE7, TMR4IE, BANKED
        bcf     IPR7, TMR4IP, BANKED
#endif
#endif
#if MS_TMR == 5                 ;; Timer 5 for 1 ms system tick
#ifndef K42
        movlw   h'01'           ; Fosc/4,prescale = 1, 8-bit write
        movwf   T5CON, BANKED
        bsf     PIE5, TMR5IE
        bcf     IPR5, TMR5IP
#else
        movlw   h'01'           ; Fosc/4,prescale = 1, 8-bit write
        movwf   T5CON, BANKED
        bsf     PIE5,TMR5IE
        movlw   h'01'           ; fosc/4
        movwf   T5CLK
        banksel PIE8
        bsf     PIE8, TMR5IE, BANKED
        bcf     IPR8, TMR5IP, BANKED
#endif
#endif
#if MS_TMR == 6                 ;; Timer 6 for 1 ms system tick
#ifndef K42
        movlw   h'7d'           ; Prescale = 4, Postscale = 16
        movwf   T6CON, BANKED
        movlw   tmr2ms_val
        movwf   PR6, BANKED
        bsf     PIE5, TMR6IE
        bcf     IPR5, TMR6IP
#else
        movlw   h'01'
        movwf   T6CLK
        movlw   tmr2ms_val
        movwf   T6PR
        movlw   h'af'       ; Prescale = 4, Postscale = 16
        movwf   T6CON
        banksel PIE9
        bsf     PIE9, TMR6IE, BANKED
        bcf     IPR9, TMR6IP, BANKED
#endif
#endif
#ifdef USB_CDC
        rcall   USB_ON
#endif
        movlw   low(u0+PRAM)        ; UP
        movwf   upcurr
        movlw   high(u0+PRAM)
        movwf   upcurr+1

        rcall   LIT
        dw      WARMLIT
        rcall   LIT
        dw      userarea+PRAM
        rcall   LIT
        dw      warmlitsize
        call    CMOVE
        
        rcall   FRAM
        clrf    INTCON
#ifdef K42
        bsf     INTCON, 5
#endif
        bsf     INTCON, GIEL
        bsf     INTCON, GIEH

        rcall   LIT
        dw      dp_start
        rcall   FETCH
        call    TRUE_
        rcall   EQUAL
        call    ZEROSENSE
        bz      WARM_2
        rcall   EMPTY
WARM_2:
        call    DP_TO_RAM
#if FC_TYPE_SW == ENABLE
        bsf     FLAGS2, ixoff ; Force sending of XON in RX1?
#endif
#ifdef HW_FC_CTS_TRIS
        bcf     HW_FC_CTS_TRIS, HW_FC_CTS_PIN
#endif
        rcall   RQ
        rcall   VER
        
        rcall   TURNKEY
        call    ZEROSENSE
        bz      STARTQ2
        rcall   XSQUOTE
        db      d'3',"ESC"
        rcall   TYPE
        rcall   LIT
        dw      TURNKEY_DELAY
        call    MS
        rcall   KEYQ
        call    ZEROSENSE
        bz      STARTQ1
        rcall   KEY
        rcall   LIT
        dw      h'1b'
        rcall   XOR ; NOTEQUAL
        call    ZEROSENSE
        bz      STARTQ2
STARTQ1:
        rcall   TURNKEY
        rcall   EXECUTE
STARTQ2:
        goto    ABORT

;*******************************************************
        dw      L_WARM
L_VER:
                db              NFA|3,"ver"
VER:
        rcall   XSQUOTE
         ;        12345678901234 +   11  + 12345678901234567890
        db d'38'," FlashForth 5 ",PICTYPE," 01.04.2020\r\n"
        goto    TYPE
;*******************************************************
ISTORECHK:
#if XSTORE == ENABLE
        tstfsz  iaddr_up
        return
#else
        movlw   HIGH FLASH_HI+1
        cpfslt  Srw
        bra     ISTORERR
#endif
        movlw   HIGH(dpcode) ;(dp_user_dictionary>>8) ;
        cpfslt  Srw
        return
        bra     ISTORERR

;**********************************************************             
        db      NFA|2,"or"
OR_A:   
        bra     OR
;************************************************************
;;; Check parameter stack pointer
        db      NFA|3,"sp?"
check_sp:
        rcall   SPFETCH
        call    S0
        rcall   FETCH
        call    TIB
        rcall   WITHIN
        rcall   XSQUOTE
        db      d'3',"SP?"
        call    QABORT
        return
;***************************************************
; EMIT  c --    output character to the emit vector
        dw      L_VER
L_EMIT:
        db      NFA|4,"emit"
EMIT:
        rcall   UEMIT
        goto    FEXECUTE

;***************************************************
; KEY   -- c    get char from UKEY vector
        dw      L_EMIT
L_KEY:
        db      NFA|3,"key"
KEY:
        rcall   UKEY
        goto    FEXECUTE

;***************************************************
; KEY   -- c    get char from UKEY vector
        dw      L_KEY
L_KEYQ:
        db      NFA|4,"key?"
KEYQ:
        rcall   UKEYQ
        goto    FEXECUTE

;***************************************************
; LIT   -- x    fetch inline 16 bit literal to the stack
; 17 clock cycles
;       dw      link
;link    set     $
        db      NFA|3,"lit"
LIT:
        movf    TOSL, W
        movwf   TBLPTRL
        movf    TOSH, W
        rcall	FETCHLIT
        movf    TBLPTRH, W
        movwf   TOSH
        movf    TBLPTRL, W
        movwf   TOSL
        return
    
;****************************************************
; EXECUTE  xt -- execute word at xt
; 6 clock cycles
        dw      L_KEYQ
L_EXECUTE:
        db      NFA|7,"execute"
EXECUTE:
        movf    Sminus, W
        movwf   PCLATH
        movf    Sminus, W
        movwf   PCL           ;  after this, xt is executing

; @EX  addr -- execute xt from addr
; 6 clock cycles
        dw      L_EXECUTE
L_FEXECUTE:
        db      NFA|3,"@ex"
FEXECUTE:
        rcall   FETCH
        goto    EXECUTE

;****************************************************
;****************************************************
; VARIABLE name --            define a Forth 16 bit VARIABLE
; The data of this variable is stored in data space (PIC ram, eeprom, rom).
;   CREATE CELL ALLOT ;
        dw      L_FEXECUTE
L_VARIABLE:
        db      NFA|8,"variable"
VARIABLE_:
        rcall   HERE            ; Make space at here
        rcall   CELL            ; for a cell
        rcall   ALLOT           ; in current data space
        goto    CON_            ; Constant as inline literal

        dw      L_VARIABLE
L_2VARIABLE:
        db      NFA|9,"2variable"
TWOVARIABLE_:
        rcall   HERE            ; Make space at here
        rcall   LIT             ; for a two cells
        dw      h'4'
        rcall   ALLOT           ; in current data space
        goto    CON_            ; Constant as inline literal

;******************************************************
; CONSTANT x name --      define a Forth constant
;  : CO: CREATE  CELL NEGATE IALLOT I, ;
; Note that the constant is stored in flash.
        dw      L_2VARIABLE
L_CONSTANT:
        db      NFA|3,"co:"
CONSTANT_:
        call    CREATE      ; Create a word that in runtime leaves the current DP on the stack
        rcall   CELL
        rcall   NEGATE
        call    IALLOT
        goto    ICOMMA      ; Append the constant value

;;; CON is a faster version of CONSTANT for numeric constants
;;; CON does not work in conjuction with DOES> . Use CONSTANT together with DOES>
;;; Execution time for a CON word is 7-8 cycles, 4 more cycles than a inline literal
;;; : con create -6 iallot postpone literal postpone  ; ;
        dw      L_CONSTANT
L_CON:
        db      NFA|8,"constant"
CON_:
        call    COLON         ; Create a word header
        rcall   LITERAL       ; Append the constant value  as inline literal
        goto    SEMICOLON     ; Compile return

        dw      L_CON
L_2CON:
        db      NFA|9,"2constant"
TWOCON:
        rcall   SWOP
        call    COLON         ; Create a word header
        rcall   LITERAL       ; Append the constant value  as inline literal
        rcall   LITERAL       ; Append the constant value  as inline literal
        goto    SEMICOLON     ; Compile return

; DOCREATE, code action of CREATE
; Fetch the next cell from program memory to the parameter stack
; 15 clock cycles
;       dw      link
;link    set     $
        db      NFA|3,"(c)"
DOCREATE: ; -- addr  exec action of CREATE
        movf    TOSL, W
        movwf   TBLPTRL
        movf    TOSH, W
        rcall   FETCHLIT
        decf    STKPTR, F, A
RETURN2:
        return

;;; Resolve the runtime action of the word created by using does>
; 20 clock cycles 6,3 us@12MHz
;       dw      link
;link   set     $
        db      NFA|3,"(d)"
DODOES:
        movf    TOSL, W
        movwf   Tp
        movf    TOSH, W
        movwf   PCLATH
        decf    STKPTR, F, A
        movf    TOSL, W
        movwf   TBLPTRL
        movf    TOSH, W
        rcall   FETCHLIT
        movf    PCLATH, W
        movwf   TOSH
        movf    Tp, W
        movwf   TOSL
        return
	
;;; Compile inline address as subroutine call  
        db      NFA|3,"(,)"
DOCOMMAXT:
        movf    TOSL, W
        movwf   TBLPTRL
        movf    TOSH, W
        rcall   FETCHLIT
        movf    TBLPTRH, W
        movwf   TOSH
        movf    TBLPTRL, W
        movwf   TOSL
        goto    COMMAXT

;   SP@     -- addr         get parameter stack pointer
        dw      L_2CON
L_SPFETCH:
        db      NFA|3,"sp@"
SPFETCH:
        movf    Sp, W
        movwf   Tp
        movf    Sbank, W
        movff_   Tp, plusS
        iorlw   high(PRAM)
        movwf   plusS
        return


; !     x addrl addru  --   store x at addr in memory
; 17 clock cycles for ram. 3.5 us @ 12 Mhz
        dw      L_SPFETCH
#if XSTORE == ENABLE
L_XSTORE:
        db      NFA|2,"x!"
        movf    Sminus, W
        movf    Sminus, W
        movwf   iaddr_up
        call    ISTORE
        clrf    iaddr_up
        return

; MEMORY OPERATIONS =============================
; !     x addr --   store x at addr in memory
; 17 clock cycles for ram. 3.5 us @ 12 Mhz
        dw      L_XSTORE
#endif
L_STORE:
        db      NFA|1,"!"
STORE:
        movlw   high(PRAM)
        cpfslt  Srw
        bra     STORE1
        movlw   high(PEEPROM)
        cpfslt  Srw
        bra     ESTORE
        bra     ISTORE
STORE1:
        movf    Sminus, W
        movwf   Tbank
        movf    Sminus, W
        movwf   Tp
        movf    Tplus, W
        movf    Sminus, W
        movwf   Tminus
        movf    Sminus, W
        movwf   Trw
return1:
        return

;   C!      x addr -- store lower byte of cell x in memory
;;; 15 cycles + chkramaddr (6-10 cycles) for ram
        dw      L_STORE
L_CSTORE:
        db      NFA|2,"c!"
CSTORE:
        movlw   high(PRAM)
        cpfslt  Srw
        bra     CSTORE1
        movlw   high(PEEPROM)
        cpfslt  Srw
        bra     ECSTORE
        goto    ICSTORE
CSTORE1:
        movf    Sminus, W
        movwf   Tbank
        movf    Sminus, W
        movwf   Tp
        movf    Sminus, W
        movf    Sminus, W
        movwf   Trw
        return

;   @       addr -- x    fetch cell from memory
; 16 cycles for ram.
; 26-33 cycles for rom
        dw      L_CSTORE
L_FETCH:
        db      NFA|1,"@"
FETCH:
        movlw   high(PRAM)
        cpfslt  Srw
        bra     FETCH1
        movlw   high(PEEPROM)
        cpfslt  Srw
        bra     EFETCH
        goto    IFETCH
FETCH1:
        movf    Sminus, W
        movwf   Tbank
        movf    Sminus, W
        movwf   Tp
FETCH2:
        movf    Tplus, W
        movwf   plusS
        movf    Tplus, W
        movwf   plusS
        return


;   C@      addr -- x fetch char from memory
;;; 15 cycles for ram.
        dw      L_FETCH
L_CFETCH:
        db      NFA|2,"c@"
CFETCH:
        movlw   high(PRAM)
        cpfslt  Srw
        bra     CFETCH1
        movlw   high(PEEPROM)
        cpfslt  Srw
        bra     ECFETCH
        goto    ICFETCH
CFETCH1:
        movf    Sminus, W   
        movwf   Tbank
        movf    Sminus, W
        movwf   Tp
CFETCH2:
        movf    Trw, W
        movwf   plusS
        clrf    plusS
        return

;   RPEMPTY     -- EMPTY THE RETURN STACK       
;   empty the return stack and jump to the caller
;       dw      link
;link   set     $
        db      NFA|3,"rp0"
RPEMPTY:
        ;lfsr    Rptr, urbuf
        movlw   low(urbuf)
        movwf   Rp
        movlw   high(urbuf)
        movwf   Rbank

        movf    TOSH, W
        movwf   PCLATH    ; Save the return address
        movf    TOSL, W
        clrf    STKPTR
        movwf   PCL

ISTORERR:
ISTORERR2:
        call    DOTS
        rcall   XSQUOTE
        db      3,"AD?"
        rcall   TYPE
        bra     STARTQ2        ; goto    ABORT

;   x@       addrl addru -- x    fetch cell from flash
        dw      L_CFETCH
#if XSTORE == ENABLE
L_XFETCH:
        db      NFA|2,"x@"
XFETCH:
        movf    Sminus, W
        movf    Sminus, W
        movwf   TBLPTRU
        movf    Sminus, W
        movwf   TBLPTRH
        movf    Sminus, W
        movwf   TBLPTRL
        call    pfetch0
        clrf    TBLPTRU
        return

; DICTIONARY POINTER FOR the current section
; Flash -- sets the data section to flash
        dw      L_XFETCH
#endif
L_FLASH:
ROM_N:  
        db      NFA|5,"flash"
ROM:
        clrf    cse
        return

; EEPROM -- sets the data section to EEPROM data memory
        dw      L_FLASH
L_EEPROM:
EROM_N: 
        db      NFA|6,"eeprom"
EROM:
        movlw   2
        movwf   cse
        return
        
; RAM -- sets the data section to RAM memory
        dw      L_EEPROM
L_RAM:
FRAM_N: 
        db      NFA|3,"ram"
FRAM:
        movlw   4
        movwf   cse
        return

; DP    -- a-addr          
; Fetched from EEPROM
        dw      L_RAM
L_DP:
        db      NFA|2,"dp"
DP:
        call    IDP
        rcall   CSE
        goto    PLUS


;;; 
        db      NFA|3,"cse"
CSE:
        movf    cse, W
        movwf   plusS
        clrf    plusS
        return

; HERE    -- addr    get current data space ptr
;   DP @ ;
        dw      L_DP
L_HERE:
        db      NFA|4,"here"
HERE:
        rcall   DP
        goto    FETCH

        db      NFA|5,"ihere"
IHERE_P: 
        goto   IHERE
; ,   x --             append cell to current data space
;   HERE ! CELL ALLOT ;
        dw      L_HERE
L_COMMA:
        db      NFA|1,","
COMMA:
        rcall   HERE
        rcall   STORE
        rcall   CELL
        goto    ALLOT

; C,  c --             append char to current data space
;   HERE C! 1 ALLOT ;
        dw      L_COMMA 
L_CCOMMA:
        db      NFA|2,"c,"
CCOMMA:
        rcall   HERE
        rcall   CSTORE
        rcall   ONE
        goto    ALLOT


; CELL     -- n                 size of one cell
        dw      L_CCOMMA
L_CELL:
        db      NFA|INLINE|4,"cell"
CELL:
        movlw   h'2'
        movwf   plusS,A
        clrf    plusS,A
        return

; ALIGN    --                         align DP
        dw      L_CELL
L_ALIGN:
        db      NFA|5,"align"
ALIGN:
        rcall   HERE
        rcall   ALIGNED
        rcall   DP
        goto    STORE

; ALIGNED  addr -- a-addr       align given addr
        dw      L_ALIGN
L_ALIGNED:
        db      NFA|7,"aligned"
ALIGNED:
        swapf   Sminus, W
        incf    Srw, F
        bcf     Splus, 0
        bnc     ALIGNED1
        incf    Srw, F
ALIGNED1:
        return

; CELL+    a-addr1 -- a-addr2      add cell size
;   2 + ;
        dw      L_ALIGNED
L_CELLPLUS:
        db      NFA|5,"cell+"
CELLPLUS:
        goto    TWOPLUS

; CELLS    n1 -- n2            cells->adrs units
        dw      L_CELLPLUS
L_CELLS:
        db      NFA|5,"cells"
CELLS:
        goto    TWOSTAR

; CHAR+    c-addr1 -- c-addr2   add char size
        dw      L_CELLS
L_CHARPLUS:
        db      NFA|5,"char+"
CHARPLUS:
        goto    ONEPLUS

; CHARS    n1 -- n2            chars->adrs units
        dw      L_CHARPLUS
L_CHARS:
        db      NFA|INLINE|5,"chars"
CHARS:  return



; cf,    xt --  append codefield
        dw      L_CHARS
L_COMMAXT:
        db      NFA|3,"cf,"
COMMAXT:
        rcall   DUP
        rcall   IHERE_P
        rcall   MINUS
        rcall   ABS 
        rcall   LIT
        dw      0x7f0
        rcall   GREATER
        rcall   ZEROSENSE
        bz      STORECF1
STORECFF1: 
        rcall   CALL_
        bra     STORECF2 
STORECF1:
        rcall   IHERE_P
        rcall   MINUS
        call    TWOMINUS
        rcall   RCALL_
STORECF2:
        return

; !COLON   --       change code field to docolon
;   -6 IALLOT ; 
;       dw      link
;link   set     $
        db      NFA|6,"!colon"
STORCOLON:
        rcall   LIT
        dw      h'fffa'         ;  -6
        goto    IALLOT


; 2@    a-addr -- x1 x2            fetch 2 cells
;   DUP @ SWAP CELL+ @ ;
;   the lower address will appear on top of stack
        dw      L_COMMAXT
L_TWOFETCH:
        db      NFA|2,"2@"
TWOFETCH:
        rcall   DUP
        rcall   FETCH
        rcall   SWOP
        rcall   CELLPLUS
        goto    FETCH

; 2!    x1 x2 a-addr --            store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
        dw      L_TWOFETCH
L_TWOSTORE:
        db      NFA|2,"2!"
TWOSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   CELLPLUS
        rcall   STORE
        goto    STORE

; 2DROP  x1 x2 --                   drop 2 cells
;   DROP DROP ;
        dw      L_TWOSTORE
L_TWODROP:
#ifndef K42
        db      NFA|5,"2drop"
#else
        db      NFA|INLINE|5,"2drop"
#endif
TWODROP:
#ifndef K42
        rcall   DROP
        goto    DROP
#else
        subfsr  Sptr, 4
        return
#endif

; 2DUP   x1 x2 -- x1 x2 x1 x2    dup top 2 cells
;   OVER OVER ;
        dw      L_TWODROP
L_TWODUP:
        db      NFA|4,"2dup"
TWODUP:
        movlw   -3
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        return


; 2SWAP   x1 x2 x3 x4 -- x3 x4 x1 x2    dup top 2 cells
        dw      L_TWODUP
L_TWOSWAP
        db      NFA|5,"2swap"
TWOSWAP:
        rcall   ROT
        rcall   TOR
        rcall   ROT
        rcall   RFROM
        return

; INPUT/OUTPUT ==================================

; SPACE   --                      output a space
;   BL EMIT ;
        dw      L_TWOSWAP
L_SPACE:
        db      NFA|5,"space"
SPACE_:  
        call    BL
        goto    EMIT

; SPACES   n --                  output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
        dw      L_SPACE
L_SPACES:
        db      NFA|6,"spaces"
SPACES:
SPCS1:
        rcall   DUPZEROSENSE
        bz      SPCS2
        rcall   SPACE_
        rcall   ONEMINUS
        bra     SPCS1
SPCS2:  goto    DROP


; umin     u1 u2 -- u           unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
        dw      L_SPACES
L_UMIN:
        db      NFA|4,"umin"
UMIN:
        rcall   TWODUP
        rcall   UGREATER
        bra     MINMAX

; umax    u1 u2 -- u            unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
        dw      L_UMIN
L_UMAX
        db      NFA|4,"umax"
UMAX:
        rcall   TWODUP
        rcall   ULESS
        bra     MINMAX

        dw      L_UMAX
L_ONE:
        db      NFA|INLINE|1,"1"
ONE:
        movlw   1
WTOS:   
        movwf   plusS
        clrf    plusS
        return

; ACCEPT  c-addr +n -- +n'  get line from terminal
        dw      L_ONE
L_ACCEPT:
        db      NFA|6,"accept"
ACCEPT:
        rcall   OVER
        rcall   PLUS
        rcall   TOR
        rcall   DUP
ACC_1:
        rcall   KEY
        movf    Sminus, W
        movf    Splus, W
        sublw   8             ; L - W
        bz      ACC_BS_DEL
        sublw   -2            ; 
        bz      ACC_LF
        sublw   3             ;
        bz      ACC_CR
        sublw   d'114'
        bz      ACC_BS_DEL
        bra     ACC_3
ACC_CR:
        rcall   FCR             ; Mark CR received
        rcall   CSTORE
        bra     ACC_6           ; CR END OF LINE
ACC_LF:
        rcall   DROP
        rcall   FCR
        rcall   CFETCH
        rcall   ZEROSENSE
        bz      ACC_6           ; LF END OF LINE, CR has not been received
        call    FALSE_
        rcall   FCR
        rcall   CSTORE
        bra     ACC_1           ; CR has been received
ACC_BS_DEL:
        call    FALSE_
        rcall   FCR
        rcall   CSTORE
        rcall   DROP
        rcall   TWODUP
        rcall   EQUAL
        rcall   ZEROSENSE
        bnz     ACC_1
        rcall   ONEMINUS
        rcall   XSQUOTE
        db      3,8,h'20',8
        rcall   TYPE
        bra     ACC_1
ACC_3:
        rcall   DUP
        rcall   EMIT
        rcall   OVER
        rcall   CSTORE
        rcall   ONEPLUS
        rcall   RFETCH
        rcall   OVER
        rcall   EQUAL
        rcall   ZEROSENSE
        bz      ACC_1
ACC_6:
        call    RDROP
        rcall   SWOP
        goto    MINUS

        db      NFA|3,"fcr"
FCR:
        rcall   DOUSER
        dw      uflg
; TYPE    c-addr u --   type line to terminal u < $100
; : type for c@+ emit next drop ;

        dw      L_ACCEPT
L_TYPE:
        db      NFA|4,"type"
TYPE:
        rcall   TOR             ; XFOR
        bra     TYPE2           ; XFOR
TYPE1:  
        rcall   CFETCHPP
        rcall   EMIT
TYPE2:
        decf    Rrw , F      ; XNEXT
        bc      TYPE1           ; XNEXT
        movwf   Rminus
        movwf   Rminus    ; UNNEXT
        goto    DROP

; (S"    -- c-addr u      run-time code for S"
        dw      L_TYPE
L_XSQUOTE:
        db      NFA|3,"(s",0x22
XSQUOTE:
        movf    TOSL, W
        movwf   plusS
        movf    TOSH, W
        movwf   plusS
        rcall   CFETCHPP
        rcall   TWODUP
        rcall   PLUS
        rcall   ALIGNED
        movf    Sminus, W
        movwf   TOSH
        movf    Sminus, W
        movwf   TOSL
        return


; S"      --            compile in-line string to flash
        dw      L_XSQUOTE
L_SQUOTE:
        db      NFA|IMMED|COMPILE|2,"s",0x22
SQUOTE:
        rcall   DOCOMMAXT
        dw      XSQUOTE
        rcall   ROM
        rcall   CQUOTE
        goto    FRAM
        
; ,"      --           store a string to current data space
        dw      L_SQUOTE
L_CQUOTE:
        db      NFA|2,",",0x22
CQUOTE: 
        rcall   LIT
        dw      0x22
        rcall   PARSE
        rcall   HERE
        rcall   OVER
        rcall   ONEPLUS
        rcall   ALLOT
        rcall   PLACE
        goto    ALIGN


; ."       --            compile string to print into flash
        dw      L_CQUOTE
L_DOTQUOTE:
        db      NFA|IMMED|COMPILE|2,".",0x22
DOTQUOTE: 
        rcall   SQUOTE
        rcall   DOCOMMAXT
        dw      TYPE
        return


; ALLOT   n --    allocate n bytes in current data section
        dw      L_DOTQUOTE
L_ALLOT:
        db      NFA|5,"allot"
ALLOT:
        rcall   DP
        goto    PLUSSTORE

;************************************************
; DROP  x --                    DROP top of stack
; 2 cycles.
        dw      L_ALLOT
L_DROP
        db      NFA|INLINE|4,"drop"
DROP:
#ifndef K42
        movwf   Sminus       ; no status change
        movwf   Sminus
#else
        subfsr  Sptr, 2
#endif
        return
    

; SWAP  x1 x2 -- x2 x1          SWAP two top items
; 17 cycles
        dw      L_DROP
L_SWOP:
        db      NFA|4,"swap"
SWOP:
        movlw   -2
        movff_   SWrw, Tp
        movff_   Srw, SWrw
        movff_   Tp, Sminus
        movff_   SWrw, Tp
        movff_   Srw, SWrw
        movf    Tp, W
        movwf   Splus
        return

; OVER  x1 x2 -- x1 x2 x1           OVER
; 9 cycles
        dw      L_SWOP
L_OVER:
        db      NFA|4,"over"
OVER:
        movlw   -3
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        return

;   ROT x1 x2 x3 -- x2 x3 x1        ROT
; 34+24+4=62  cycles
        dw      L_OVER
L_ROT:
        db      NFA|3,"rot"
ROT:
        rcall   TOR
        rcall   SWOP
        rcall   RFROM
        goto    SWOP
;   >R       x --   R: -- x   push to return stack
; 12 cycles
        dw      L_ROT
L_TOR:
        db      NFA|2,">r"
TOR:
        movf    Sminus, W
        movwf   plusR
        movf    Sminus, W
        movwf   plusR
        return

;   R> -- x R: x --             pop from R stack
; 12 cycles
        dw      L_TOR
L_RFROM:
        db      NFA|2,"r>"
RFROM:
        movf    Rminus, W
        movwf   plusS
        movf    Rminus, W
        movwf   plusS
        return
;  R@  -- x  R: x -- x         fetch from R stack
;  4 cycles 
        dw      L_RFROM
L_RFETCH:
        db      NFA|2,"r@"
RFETCH:
        movf    Rminus, W
        movwf   plusS
        movf    Rplus,  W
        movwf   plusS
        return

;   DUP x -- x x    duplicate top of stack cell
; 9 cycles including call and return
        dw      L_RFETCH
L_DUP:
        db      NFA|3,"dup"
DUP:
        movlw   -1
        movff_   SWrw, plusS
        movff_   SWrw, plusS
        return

;***********************************************************
;   ABS     n   --- n1      absolute value of n
        dw      L_DUP
L_ABS:
        db      NFA|3,"abs"
ABS:
        rcall   DUP
        goto    QNEGATE

;   +
        dw      L_ABS
L_PLUS:
        db      NFA|1,"+"
PLUS:
        movf    Sminus, W
        movwf   Tp
        movf    Sminus, W
        movf    Sminus, F
        addwf   Srw, F
        movf    Tp, W
        addwfc  plusS, F
        return

; M+       d n -- d         add single to double
        dw      L_PLUS
L_MPLUS:
        db      NFA|2,"m+"
MPLUS:
        call    STOD
        goto    DPLUS

;   -   n1/u1 n2/u2 -- n3/u3 n3 = n1 - n2 
        dw      L_MPLUS
L_MINUS:
        db      NFA|1,"-"
MINUS:
        swapf   Sminus, W
        movwf   Tp 
        movf    Sminus, W
        movf    Sminus, F
        subwf   Srw, F
        swapf   Tp, W 
        subwfb  plusS, F
        return

;   AND
        dw      L_MINUS
L_AND:
        db      NFA|3,"and"
AND:
        movf    Sminus, W
        movff_   Sminus, Tp
        andwf   Sminus, F
        movf    Tp, W
        andwf   Splus, F
        return

;   OR ( n n -- )
        dw      L_AND
L_OR:
        db      NFA|2,"or"
OR:
        movf    Sminus, W
        movff_   Sminus, Tp
        iorwf   Sminus, F
        movf    Tp, W
        iorwf   Splus, F
        return

;   XOR   ( n n -- )
        dw      L_OR
L_XOR
        db      NFA|3,"xor"
XOR:
        movf    Sminus, W
        movff_   Sminus, Tp
        xorwf   Sminus, F
        movf    Tp, W
        xorwf   Splus, F
        return

;   INVERT
        dw      L_XOR
L_INVERT:
        db      NFA|6,"invert"
INVERT:
        comf   Sminus, F
        comf   Splus, F
        return

;   NEGATE
        dw      L_INVERT
L_NEGATE:
        db      NFA|6,"negate"
NEGATE:
        rcall   INVERT
        goto    ONEPLUS

;   1+
        dw      L_NEGATE
L_ONEPLUS:
        db      NFA|2,"1+"
ONEPLUS:
        swapf   Sminus, W
        infsnz  Splus, F
        incf    Srw, F
        return

;   1-
        dw      L_ONEPLUS
L_ONEMINUS:
        db      NFA|2,"1-"
ONEMINUS:
        swapf   Sminus, W
        decf    Splus, F
        movlw   h'0'
        subwfb  Srw, F
        return

; 2+    n -- n-2      2 + CELL+
        dw      L_ONEMINUS
L_TWOPLUS:
        db      NFA|2,"2+"
TWOPLUS:
        swapf   Sminus, W
        movlw   2
        addwf   Splus, F
        movlw   0
        addwfc  Srw, F
        return

; >body xt -- a-addr transform a created words XT to it's data field address
; : >body 2+ 2+ ;
        dw      L_TWOPLUS
L_TOBODY:
        db      NFA|5,">body"
TOBODY:
        rcall   TWOPLUS
        goto    TWOPLUS

;   2*
        dw      L_TOBODY
L_TWOSTAR:
        db      NFA|2,"2*"
TWOSTAR:
        swapf   Sminus, W
        bcf     STATUS, C
        rlcf    Splus, F
        rlcf    Srw, F
        return

;   2/
        dw      L_TWOSTAR
L_TWOSLASH:
        db      NFA|2,"2/"
TWOSLASH:
        bcf     STATUS, C
        btfsc   Srw, 7
        bsf     STATUS, C
        rrcf    Sminus, F
        rrcf    Splus, F
        return

;   +!      n/u addr --     add cell to data memory
        dw      L_TWOSLASH
L_PLUSSTORE:
        db      NFA|2,"+!"
PLUSSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   FETCH
        rcall   PLUS
        rcall   SWOP
        goto    STORE
        
;***************************************************
;   WITHIN      ( u ul uh -- t )
;               Return true if u is within the range of ul and uh. ( ul <= u < uh )
        dw      L_PLUSSTORE
L_WITHIN:
        db      NFA|6,"within"
WITHIN:
        rcall   OVER
        rcall   MINUS
        rcall   TOR
        rcall   MINUS
        rcall   RFROM
        goto    ULESS

;***************************************************
;   <>      x1 x2 -- flag       return true if not equal 
        dw      L_WITHIN
L_NOTEQUAL:
        db      NFA|2,"<>"
NOTEQUAL:
        rcall   MINUS
        movf    Sminus, W
        iorwf   Srw
        bnz     test_true               ; x1 not equal to x2
        bra     test_false              ; x1 equal to x2

;***************************************************
;   0=      n/u -- flag         return true if TOS=0
        dw      L_NOTEQUAL
L_ZEROEQUAL:
        db      NFA|2,"0="
ZEROEQUAL:
        movf    Sminus, W
        iorwf   Srw, W
        bnz     test_false
test_true:                      ; TOS is ffff (TRUE)
        setf    Srw
        setf    plusS
        return

;***************************************************
;   0<      n -- flag           return true if TOS is negative
        dw      L_ZEROEQUAL
L_ZEROLESS:
        db      NFA|2,"0<"
ZEROLESS:
        btfsc   Sminus, 7
        bra     test_true
test_false:                    ; TOS is 0000 (FALSE)
        clrf    Srw         ; TOS_LO = 00
        clrf    plusS       ; TOS_HI = 00
        return

;***************************************************
;   =       x1 x2 -- flag       return true if x1 = x2
        dw      L_ZEROLESS
L_EQUAL:
        db      NFA|1,"="
EQUAL:
        rcall   MINUS
        goto    ZEROEQUAL

;***************************************************
;   <       n1 n2 -- flag       return true if n1 < n2
        dw      L_EQUAL
L_LESS:
        db      NFA|1,"<"
LESS:
        rcall   MINUS               ; n1 - n2 in TOS
        goto    ZEROLESS            ; if negative return true

;***************************************************
;   >       n1 n2 -- flag       return true if n1 > n2
        dw      L_LESS
L_GREATER
        db      NFA|1,">"
GREATER:
        rcall   SWOP
        goto    LESS

;***************************************************
;   U<      u1 u2 -- flag       test unsigned less
        dw      L_GREATER
L_ULESS:
        db      NFA|2,"u<"
ULESS:
        rcall   MINUS
        swapf   Sminus, W
        bnc     test_true
        bra     test_false

;***************************************************
;   U>      u1 u2 -- flag       test unsigned greater than
        dw      L_ULESS
L_UGREATER:
        db      NFA|2,"u>"
UGREATER:
        rcall   SWOP
        goto    ULESS

;***************************************************
        dw      L_UGREATER
L_STORE_P:
        db      NFA|2,"!p"
STORE_P:
        movf    Sminus, W
        movwf   p_hi
        movf    Sminus, W
        movwf   p_lo
        return

;***************************************************
        dw      L_STORE_P
L_STORE_P_TO_R:
        db      NFA|4,"!p>r"
STORE_P_TO_R:
        movf    p_hi, W
        movwf   plusR
        movf    p_lo, W
        movwf   plusR
        goto    STORE_P           ; Set the new pointer
;***************************************************
        dw      L_STORE_P_TO_R
L_R_TO_P:
        db      NFA|3,"r>p"
R_TO_P:
        movf    Rminus, W
        movwf   p_lo
        movf    Rminus, W
        movwf   p_hi
        return
;***************************************************
        dw      L_R_TO_P
L_PFETCH:
        db      NFA|2,"p@" ; ( -- u ) Fetch cell from pointer
PFETCH:
        movf    p_lo, W
        movwf   plusS
        movf    p_hi, W
        movwf   plusS
        goto    FETCH
;***************************************************    
        dw      L_PFETCH
L_PSTORE:
        db      NFA|2,"p!"  ; store cell to pointer
PSTORE:
        movf    p_lo, W
        movwf   plusS
        movf    p_hi, W
        movwf   plusS
        goto    STORE
;***************************************************    
        dw      L_PSTORE
L_PCSTORE:
        db      NFA|3,"pc!" ; store char to pointer
PCSTORE:
        movf    p_lo, W
        movwf   plusS
        movf    p_hi, W
        movwf   plusS
        goto    CSTORE
;***************************************************    
        dw      L_PCSTORE
L_PPLUS:
        db      NFA|INLINE|2,"p+"  ; Increment p by one
PPLUS:
        infsnz  p_lo, F
        incf    p_hi, F
        return
;***************************************************
        dw      L_PNPLUS
L_PTWOPLUS:
kernellink:
        db      NFA|3,"p2+" ; ( n -- ) Add 2 to p
PTWOPLUS:
        movlw   2
        addwf   p_lo, F
        movlw   0
        addwfc  p_hi, F
        return
;***************************************************
; 'EMIT  -- addr         Address of EMIT user vector
        dw      L_PPLUS
L_UEMIT:
        db      NFA|5,"'emit"
UEMIT:
        rcall   DOUSER
        dw      uemit&h'ffff'
        
;***************************************************
; UKEY  -- addr         Address of KEY user vector
        dw      L_UEMIT
L_UKEY:
        db      NFA|4,"'key"
UKEY:
        rcall   DOUSER
        dw      ukey&h'ffff'
        
;***************************************************
; UKEYQ  -- addr         Address of KEYQ user vector
        dw      L_UKEY
L_UKEYQ:
        db      NFA|5,"'key?"
UKEYQ:
        rcall   DOUSER
        dw      ukeyq&h'ffff'
        
;***************************************************    

;  n --    Set the Zero STATUS bit if TOS is zero.
;          ALWAYS inlined by the compiler.
;       dw      link
;link   set     $
        db      NFA|3,"?0="
ZEROSENSE:
        movf    Sminus, W
        iorwf   Sminus, W
        return

;  n -- n  Set the Zero STATUS bit if TOS is zero.
;;; DupZerosense that does not destroy the top of stack.
        db      NFA|3,"d0="
DUPZEROSENSE:
        movf    Sminus, W
        iorwf   Splus, W
        return


; MULTIPLY AND DIVIDE ===========================
; UM*     u1 u2 -- ud   unsigned 16x16->32 mult.
        dw      L_UKEYQ
L_UMSTAR:
        db      NFA|3,"um*"
UMSTAR:
        goto    umstar0

; UM/MOD   ud u1 -- u2(rem) u3(quot)     unsigned 32/16->16
        dw      L_UMSTAR
L_UMSLASHMOD:
        db      NFA|6,"um/mod"
UMSLASHMOD:
        goto    umslashmod0

; U/MOD   u1 u2 -- u3(rem) u4(quot)     16/16->16 divide
;   0 swap um/mod
;
        dw      L_UMSLASHMOD
L_USLASHMOD:
        db      NFA|5,"u/mod"
USLASHMOD:
        rcall   FALSE_
        rcall   SWOP
        goto    umslashmod0

; *      n1|u1 n2|u2 -- n3|u3      16*16->16 multiply
;   um* drop ;
        dw      L_USLASHMOD
L_STAR:
        db      NFA|1,"*"
STAR: 
        rcall   UMSTAR
        goto    DROP

; U/      u1 u2 -- u3      16/16-> divide
        dw      L_STAR
L_USLASH:
        db      NFA|2,"u/"
USLASH:
        rcall   USLASHMOD
        goto    NIP


; U*/MOD  u1 u2 u3 -- u4 u5    u1*u2/u3, rem&quot
;   >R UM* R> UM/MOD ;
        dw      L_USLASH
L_USSMOD:
        db      NFA|6,"u*/mod"
USSMOD:
        rcall   TOR
        rcall   UMSTAR
        rcall   RFROM
        goto    UMSLASHMOD



; / n1 n2 -- n3  signed 16/16->16 divide
        dw      L_USSMOD
L_SLASH:
        db      NFA|1,"/"
SLASH: 
        rcall   TWODUP
        rcall   XOR
        rcall   TOR
        rcall   ABS
        rcall   SWOP
        rcall   ABS
        rcall   SWOP
        rcall   USLASH
        rcall   RFROM
        goto    QNEGATE

;   NIP x1 x2 -- x2         NIP
        dw      L_SLASH
L_NIP:
        db      NFA|3,"nip"
NIP:
        movlw   -1
        movff_   Sminus, SWrw
        movff_   Sminus, SWrw
        return

;   TUCK    x1 x2 -- x2 x1 x2
        dw      L_NIP
L_TUCK:
        db      NFA|4,"tuck"
TUCK:
        rcall   SWOP
        goto    OVER

;***************************************************
; ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;
        dw      L_TUCK
L_QNEGATE:
        db      NFA|7,"?negate"
QNEGATE:
        rcall   ZEROLESS
        rcall   ZEROSENSE
        bz      QNEGATE1
        rcall   NEGATE
QNEGATE1:
        return

; MAX    n1 n2 -- n3              signed maximum
;   2DUP < IF SWAP THEN DROP ;
        dw      L_QNEGATE
L_MAX:
        db      NFA|3,"max"
MAX:    
        rcall   TWODUP
        rcall   LESS
MINMAX:
        rcall   ZEROSENSE
        bz      max1
        rcall   SWOP
max1:   goto    DROP


; MIN    n1 n2 -- n3              signed minimum
;   2DUP > IF SWAP THEN DROP ;
        dw      L_MAX
L_MIN:
        db      NFA|3,"min"
MIN:    rcall   TWODUP
        rcall   GREATER
        bra     MINMAX

        db      NFA|2,"c@"
CFETCH_A:       
        bra     CFETCH

        
; UP    -- a-addr       Current User Area pointer
        dw      L_MIN
L_UPTR:
        db      NFA|2,"up"
UPTR:
        rcall   DOCREATE_A
        dw      upcurr+PRAM

; NUMERIC OUTPUT ================================
; HOLD  char --        add char to output string
;   -1 HP +!  HP @ C! ;
        dw      L_UPTR
L_HOLD:
        db      NFA|4,"hold"
HOLD:   rcall   TRUE_
        rcall   HP
        rcall   PLUSSTORE
        rcall   HP
        rcall   FETCH
        goto    CSTORE

; <#    --              begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
        dw      L_HOLD
L_LESSNUM:
        db      NFA|2,"<#"
LESSNUM: 
        rcall   PAD
        rcall   HP
        goto    STORE

; digit   n -- c            convert to 0..9a..z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
        dw      L_LESSNUM
L_TODIGIT:
        db      NFA|5,"digit"
TODIGIT:
        movf    Sminus, W
        movf    Srw, W
        addlw   h'f6'
        bn      TODIGIT1
        addlw   h'27'
TODIGIT1:
        addlw   h'3a'
        movwf   Srw
        clrf    plusS
        return


; #     ud1 -- ud2     convert 1 digit of output
;   base @ ud/mod rot digit hold ;
        dw      L_TODIGIT
L_NUM
        db      NFA|1,"#"
NUM:
        rcall   BASE
        rcall   FETCH
        rcall   UDSLASHMOD
        rcall   ROT
        rcall   TODIGIT
        goto    HOLD

; #S    ud1 -- ud2      convert remaining digits
;   begin # 2dup or 0= until ;
        dw      L_NUM
L_NUMS:
        db      NFA|2,"#s"
NUMS:
        rcall   NUM
        rcall   TWODUP
        rcall   OR
        rcall   ZEROSENSE
        bnz     NUMS
        return

; #>    ud1 -- c-addr u    end conv., get string
;   2drop hp @ pad over - ;
        dw      L_NUMS
L_NUMGREATER:
        db      NFA|2,"#>"
NUMGREATER:
        rcall   TWODROP
        rcall   HP
        rcall   FETCH
        rcall   PAD
        rcall   OVER
        goto    MINUS


; SIGN  n --               add minus sign if n<0
;   0< IF 2D HOLD THEN ; 
        dw      L_NUMGREATER
L_SIGN:
        db      NFA|4,"sign"
SIGN:   
        rcall   ZEROLESS
        rcall   ZEROSENSE
        bz      SIGN1
        rcall   LIT_A
        dw      h'2D'
        rcall   HOLD
SIGN1:
        return

; U.    u --                  display u unsigned
;   <# 0 #S #> TYPE SPACE ;
        dw      L_SIGN
L_UDOT:
        db      NFA|2,"u."
UDOT:
        rcall   LESSNUM
        rcall   FALSE_
        rcall   NUMS
        rcall   NUMGREATER
        rcall   TYPE
        goto    SPACE_

; U.R    u +n --      display u unsigned in field of n. 1<n<=255 
;    0 swap <# 1- for # next #s #> type space ;
        dw      L_UDOT
L_UDOTR:
        db      NFA|3,"u.r"
UDOTR:
        rcall   LESSNUM
        rcall   ONEMINUS
        rcall   TOR
        rcall   FALSE_
        bra     UDOTR2
UDOTR1:
        rcall   NUM
UDOTR2: 
        decf    Rrw, F      ;  XNEXT
        bc      UDOTR1
        movwf   Rminus
        movwf   Rminus      ; UNNEXT
        rcall   NUMS
        rcall   NUMGREATER
        rcall   TYPE
        rcall   SPACE_
        return

; .     n --                    display n signed
;   <# DUP ABS #S SWAP SIGN #> TYPE SPACE ;
        dw      L_UDOTR
L_DOT:
        db      NFA|1,"."
DOT:    rcall   LESSNUM
        rcall   DUP
        rcall   ABS
        rcall   FALSE_
        rcall   NUMS
        rcall   ROT
        rcall   SIGN
        rcall   NUMGREATER
        rcall   TYPE
        goto    SPACE_

; DECIMAL  --         set number base to decimal
;   #10 BASE ! ;
        dw      L_DOT
L_DECIMAL:
        db      NFA|7,"decimal"
DECIMAL: 
        rcall   TEN
        rcall   BASE
        goto    STORE

; HEX     --              set number base to hex
;   #16 BASE ! ;
        dw      L_DECIMAL
L_HEX:
        db      NFA|3,"hex"
HEX:
        rcall   LIT_A
        dw      d'16'
        rcall   BASE
        goto    STORE

; BIN     --              set number base to binary
;   #2 BASE ! ;
        dw      L_HEX
L_BIN:
        db      NFA|3,"bin"
BIN:    rcall   CELL
        rcall   BASE
        goto    STORE

; ULINK   -- a-addr     link to next task
        dw      L_BIN
L_ULINK:
        db      NFA|5,"ulink"
ULINK:  rcall   DOUSER
        dw      ulink&h'ffff'


; TASK       -- a-addr              TASK pointer
        dw      L_ULINK
L_TASK:
        db      NFA|4,"task"
TASK:   rcall   DOUSER
        dw      utask&h'ffff'


; HP       -- a-addr                HOLD pointer
        dw      L_TASK
L_HP:
        db      NFA|2,"hp"
HP:     rcall   DOUSER
        dw      uhp&h'ffff'

; PAD     -- a-addr        User Pad buffer
        dw      L_HP
L_PAD:
        db      NFA|3,"pad"
PAD:
        rcall   TIB
        rcall   TIBSIZE
        goto    PLUS

; BASE    -- a-addr       holds conversion radix
        dw      L_PAD
L_BASE
        db      NFA|4,"base"
BASE:
        rcall   DOUSER
        dw      ubase&h'ffff'

; USER   n --        holds conversion radix
; 18 cycles
        dw      L_BASE
L_USER:
        db      NFA|4,"user"
USER:
        call    CONSTANT_
        rcall   XDOES
DOUSER:
        movf    TOSL, W
        movwf   TBLPTRL
        movf    TOSH, W
        movwf   TBLPTRH
        tblrd*+
        movf    TABLAT, W
        addwf   upcurr, W
        movwf   plusS
        tblrd*+
        movf    TABLAT, W
        addwfc  upcurr+1, W
        movwf   plusS
        decf    STKPTR, F, A         ; return to the callers caller
        return  

; SOURCE   -- adr n         current input buffer
;   'SOURCE 2@ ;        length is at higher adrs
        dw      L_USER
L_SOURCE:
        db      NFA|6,"source"
SOURCE:
        rcall   TICKSOURCE
        goto    TWOFETCH

; /STRING  a u n -- a+n u-n          trim string
;   swap over - >r + r>
        dw      L_SOURCE
L_SLASHSTRING:
        db      NFA|7,"/string"
SLASHSTRING:
        rcall   SWOP
        rcall   OVER
        rcall   MINUS
        rcall   TOR
        rcall   PLUS
        rcall   RFROM
        return

; \     Skip the rest of the line
        dw      L_SLASHSTRING
L_BSLASH:
        db      NFA|IMMED|1,h'5c'
BSLASH:
        rcall   SOURCE
        rcall   TOIN
        rcall   STORE_A
        bsf     FLAGS1, noclear ; dont clear flags in case of \
        goto    DROP

; PARSE  char -- c-addr u
        dw      L_BSLASH
L_PARSE:
        db      NFA|5,"parse"
PARSE:
        rcall   DUP             ; c c
        rcall   SOURCE          ; c c a u
        rcall   TOIN            ; c c a u a
        rcall   FETCH_A         ; c c a u n
        rcall   SLASHSTRING     ; c c a u   new tib addr/len
        rcall   DUP             ; c c a u u
        rcall   TOR             ; c c a u                  R: u (new tib len
        rcall   ROT             ; c a u c
        call    SKIP            ; c a u        
        rcall   OVER            ; c a u a
        rcall   TOR             ; c a u                    R: u a (start of word
        rcall   ROT             ; a u c
        call    SCAN            ; a u      end of word, tib left       
        rcall   DUPZEROSENSE
        bz      PARSE1
        rcall   ONEMINUS
PARSE1: rcall   RFROM           ; a u a
        rcall   RFROM           ; a u a u
        rcall   ROT             ; a a u u
        rcall   MINUS           ; a a n  ( addition to toin
        rcall   TOIN
        rcall   PLUSSTORE       ; aend astart
        rcall   TUCK            ; astart aend astart
        goto    MINUS           ; astart wlen
     

; WORD   char -- c-addr        word delimited by char and/or TAB
        dw      L_PARSE
L_WORD:
        db      NFA|4,"word"
WORD:
        rcall   PARSE           ; c-addr wlen
        rcall   SWOP
        rcall   ONEMINUS
        rcall   TUCK
        goto    CSTORE          ; Write the length into the TIB ! 

; CMOVE  src dst u --  copy u bytes from src to dst
; cmove swap !p for c@+ pc! p+ next drop ;
        dw      L_WORD
L_CMOVE:
        db      NFA|5,"cmove"
CMOVE:
        rcall   SWOP
        rcall   STORE_P_TO_R
        rcall   TOR             ; Count to return stack 
        bra     CMOVE2
CMOVE1:
        rcall   CFETCHPP
        rcall   PCSTORE
        rcall   PPLUS
CMOVE2:
        decf    Rminus, F      ;  XNEXT
        movlw   0
        subwfb  Rplus, F
        bc      CMOVE1
        movwf   Rminus
        movwf   Rminus   ; UNNEXT
        rcall   R_TO_P
        goto    DROP


; place  src n dst --     place as counted str
        dw      L_CMOVE
L_PLACE:
        db      NFA|5,"place"
PLACE: 
        rcall   TWODUP
        call    CSTORE
        call    CHARPLUS
        rcall   SWOP
        goto    CMOVE

; :     c@+ ( addr -- addr+1 n ) dup 1+ swap c@ ;
        dw      L_PLACE
L_CFETCHPP:
        db      NFA|3,"c@+"
CFETCHPP:
        rcall   DUP
        rcall   CFETCH_A
        movlw   -3
        incf    SWrw, F
        bnc     CFETCHPP1
        movlw   -2
        incf    SWrw, F
CFETCHPP1:      
        return

; :     @+ ( addr -- addr+2 n ) dup 2+ swap @ ;
        dw      L_CFETCHPP
L_FETCHPP:
        db      NFA|2,"@+"
FETCHPP:
        rcall   DUP
        rcall   TWOPLUS
        rcall   SWOP
        goto    FETCH
;;; ******************************************************
        db      NFA|1,"!"
STORE_A:        
        goto    STORE

; N>C   nfa -- cfa    name adr -> code field
        dw      L_FETCHPP
L_NTOC
        db      NFA|3,"n>c"
NFATOCFA:
        rcall   CFETCHPP
        rcall   LIT_A
        dw      h'0f'
        rcall   AND
        rcall   PLUS
        goto    ALIGNED

; C>N   cfa -- nfa    code field addr -> name field addr
        dw      L_NTOC
L_CTON:
        db      NFA|3,"c>n"
CFATONFA:
        rcall   MINUS_FETCH
        movf    Sminus, W
        movf    Sminus, W
        bnn     CFATONFA
        return

; findi   c-addr nfa -- c-addr 0   if not found
;                          xt  1      if immediate
;                          xt -1      if "normal"
        dw      L_CTON
L_BRACFIND:
        db      NFA|3,"(f)"
findi:
findi1:
FIND_1:
        rcall   TWODUP
        rcall   OVER
        rcall   CFETCH_A
        call    NEQUAL
        rcall   ZEROSENSE
        bz      findi2
        rcall   TWOMINUS ;;;      NFATOLFA
        rcall   FETCH_A
        rcall   DUPZEROSENSE
        bnz     findi1
        bra     findi3
findi2:
        rcall   NIP
        rcall   DUP
        rcall   NFATOCFA
        rcall   SWOP
        rcall   IMMEDQ
        rcall   ZEROEQUAL
        rcall   ONE
        rcall   OR
findi3: 
        return

; IMMED?    nfa -- f        fetch immediate flag
        dw      L_BRACFIND
L_IMMEDQ:
        db      NFA|6,"immed?"
IMMEDQ: 
        rcall   CFETCH_A
        movf    Sminus, W
        movf    Splus, W
        movwf   wflags          ; COMPILE and INLINE flags for the compiler
        rcall   LIT_A
        dw      IMMED
        goto    AND

; FIND   c-addr -- c-addr 0   if not found
;                  xt  1      if immediate
;                  xt -1      if "normal"
        dw      L_IMMEDQ
L_FIND:
        db      NFA|4,"find"
FIND:   
        rcall   LIT_A
        dw      kernellink
        rcall   findi
        rcall   DUPZEROSENSE
        bnz     FIND1
        rcall   DROP
        rcall   LAST
        rcall   findi
FIND1:
        return

; DIGIT?   c -- n -1   if c is a valid digit
        dw      L_FIND
L_DIGITQ:
        db      NFA|6,"digit?"
DIGITQ:
        movf    Sminus, W
        movf    Srw, W
        addlw   -h'3a'
        bn      DIGITQ1
        addlw   -h'27'
DIGITQ1:
        addlw   h'a'
        movwf   Srw
        clrf    plusS
        bnn     DIGITQ2
        bra     FALSE_
DIGITQ2:
        rcall   DUP             ; 1 1
        rcall   BASE            ; 1 1 base
        rcall   FETCH_A         ; 1 1 10
        goto    LESS            ; 1 ffff

SLASHONE:
        call   ONE
        goto   SLASHSTRING


; SIGN?   adr n -- adr' n' f   get optional sign
; + leaves $0000 flag
; - leaves $ffff flag
        dw      L_DIGITQ
L_SIGNQ:
        db      NFA|5,"sign?"
SIGNQ:
        rcall   OVER              ; a n a
        rcall   CFETCH_A          ; a n c
        movf    Sminus, W
        movf    Sminus, W
        addlw   -'-'
        bz      SIGNQMINUS
        addlw   2
        bz      SIGNQPLUS
        bra     SIGNQEND
SIGNQMINUS:
        rcall   SLASHONE
        bra     TRUE_
SIGNQPLUS:
        rcall   SLASHONE
SIGNQEND:
        goto   FALSE_

; UD*  ud u -- ud
        dw      L_SIGNQ
L_UDSTAR:
        db      NFA|3,"ud*"
UDSTAR:
        rcall   DUP
        rcall   TOR
        rcall   UMSTAR
        rcall   DROP
        rcall   SWOP
        rcall   RFROM
        rcall   UMSTAR
        rcall   ROT
        goto    PLUS
        
; UD/MOD  ud u --u(rem) ud(quot)
        dw      L_UDSTAR
L_UDSLASHMOD:
        db      NFA|6,"ud/mod"
UDSLASHMOD:
        rcall   TOR             ; ud.l ud.h 
        rcall   FALSE_          ; ud.l ud.h 0
        rcall   RFETCH          ; ud.l ud.h 0 u
        rcall   UMSLASHMOD      ; ud.l r.h q.h
        rcall   ROT             ; r.h q.h ud.l
        rcall   ROT             ; q.h ud.l r.h
        rcall   RFROM           ; q.h ud.l r.h u
        rcall   UMSLASHMOD      ; q.h r.l q.l
ROT_A:
        goto    ROT             ; r.l q.l q.h

        dw      L_UDSLASHMOD
L_TO_A:
        db      NFA|2,">a"
TO_A:
        movf    Sminus, W
        movwf   areg+1
        movf    Sminus, W
        movwf   areg+0
        return

        dw      L_TO_A
L_A_FROM:
        db      NFA|2,"a>"
A_FROM:
        movf    areg+0, W
        movwf   plusS
        movf    areg+1, W
        movwf   plusS
        return

        
; >NUMBER  0 0 adr u -- ud.l ud.h adr' u'
;                       convert string to number
        dw      L_A_FROM
L_TONUMBER:
        db      NFA|7,">number"
TONUMBER:
        clrf    areg
        clrf    areg+1
        incf    areg, F
TONUM1:
        rcall   DUPZEROSENSE      ; ud.l ud.h adr u
        bz      TONUM3
        rcall   TOR               ; ud.l ud.h adr
        rcall   DUP
        rcall   TOR               ; ud.l ud.h adr
        rcall   CFETCH_A          ; ud.l ud.h c
        movf    Sminus, W
        movf    Splus, W
        sublw   '.'
        bz      TONUM_SKIP
        rcall   DIGITQ          ; ud.l ud.h digit flag
        rcall   ZEROSENSE
        bnz     TONUM2
        rcall   DROP
        rcall   RFROM           ; ud.l ud.h adr
        rcall   RFROM           ; ud.l ud.h adr u
        bra     TONUM3
TONUM2: 
        rcall   TOR             ; ud.l ud.h
        rcall   BASE
        rcall   FETCH_A
        rcall   UDSTAR
        rcall   RFROM           ; ud.l ud.h digit
        rcall   MPLUS           ; ud.l ud.h
        clrf    areg
        bra     TONUM_CONT
TONUM_SKIP:
        rcall   DROP
TONUM_CONT:
        rcall   RFROM           ; ud.l ud.h adr
        rcall   RFROM           ; ud.l ud.h adr u
        rcall   SLASHONE
        bra     TONUM1
TONUM3:
        rcall   A_FROM
        goto    PLUS

BASEQV:   
        dw      DECIMAL
        dw      HEX
        dw      BIN


; NUMBER?  c-addr -- n 1
;                 -- dl dh 2
;                 -- c-addr 0  if convert error
        dw      L_TONUMBER
L_NUMBERQ:
        db      NFA|7,"number?"
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

        movf    Sminus, W
        movlw   '#'
        subwf   Srw, F
        bn      BASEQ1
        movlw   3
        subwf   Srw, W
        bnn     BASEQ1
        rlncf   Srw, F
        movlw   low(BASEQV)
        addwf   Splus, F
        movlw   high(BASEQV)
        addwfc  Srw, F
        call    FEXECUTE

        rcall   SLASHONE
        bra     BASEQ2
BASEQ1:
        movf    Sminus, W
BASEQ2:                         ; a 0 0 a' u
        rcall   TONUMBER        ; a ud.l ud.h  a' u

        rcall   RFROM           ; a ud.l ud.h  a' u oldbase
        rcall   BASE            ; a ud.l ud.h  a' u oldbase addr
        rcall   STORE_A         ; a ud.l ud.h  a' u
                                ; u > 0 -> error
        rcall   ZEROSENSE       ; a ud.l ud.h  a'
        bz      QNUMD           ; u = 0 -> single or double number
QNUM_ERR:                       ; Not a number
        rcall   RFROM           ; a ud.l ud.h a' sign
        call    TWODROP
        call    TWODROP
        bra     FALSE_          ; a 0           Not a number
QNUMD:                          ; Single or double Double number
                                ; a ud.l ud.h a'
        rcall   ONEMINUS
        call    CFETCH          ; a ud.l ud.h c
        rcall   TO_A            ; a ud.l ud.h
        rcall   RFROM           ; a ud.l ud.d sign
        call    QDNEGATE
QNUMD1:
        rcall   ROT_A           ; d.l d.h a
        call    DROP            ; d.l d.h
        call    ONE             ; d.l d.h 1
        movlw   '.'             ; d.l d.h
        subwf   areg, W
        bz      QNUM1
        bra     NIP             ; n 1           Single number
QNUM1:
        goto    ONEPLUS         ; d.l d.h 2     Double number

; TI#  -- n                      size of TIB
; : ti# task @ 8 + @ ;
        dw      L_NUMBERQ
L_TIBSIZE:
        db      NFA|3,"ti#"
TIBSIZE:
        rcall   TASK
        rcall   FETCH_A
        movlw   h'8'
        call    WTOS
        rcall   PLUS
        goto    FETCH

; TIB     -- a-addr        Terminal Input Buffer
        dw      L_TIBSIZE
L_TIB:
        db      NFA|3,"tib"
TIB:
        rcall   TIU
        goto    FETCH
        
; TIU     -- a-addr        Terminal Input Buffer user variable 
        dw      L_TIB
L_TIU:
        db      NFA|3,"tiu"
TIU:
        rcall   DOUSER
        dw      utib&h'ffff'     ; pointer to Terminal input buffer

; >IN     -- a-addr        holds offset into TIB
; In RAM
        dw      L_TIU
L_TOIN:
        db      NFA|3,">in"
TOIN:
        rcall   DOUSER
        dw      utoin&h'ffff'

; 'SOURCE  -- a-addr        two cells: len, adrs
; In RAM ?
        dw      L_TOIN
L_TICKSOURCE:
        db      NFA|7,"'source"
TICKSOURCE:
        rcall   DOUSER
        dw      usource&h'ffff'    ; two cells !!!!!!

        db      NFA|3,"dup"
DUP_A:  goto    DUP

;  INTERPRET  c-addr u --    interpret given buffer
        dw      L_TICKSOURCE
L_INTERPRET:
        db      NFA|9,"interpret"
INTERPRET: 
        rcall   TICKSOURCE
        call    TWOSTORE
        rcall   FALSE_
        rcall   TOIN
        rcall   STORE_A
IPARSEWORD:
        rcall   BL
        rcall   WORD

        rcall   DUP_A
        rcall   CFETCH_A
        rcall   ZEROSENSE
        bz      INOWORD

        rcall   FIND            ; sets also wflags
        movf    Sminus, W
        movf    Sminus, W    ; 0 = not found, -1 = normal, 1 = immediate
        bz      INUMBER
        addlw   1
        bnz     IEXEC           ; Immediate word
        addwf   state, W     ; 0 or ff
        bn      ICOMPILE_1
IEXEC:                          ; Execute a word
                                ; immediate&compiling or interpreting
        btfss   wflags, 4    ; Compile only check
        bra     IEXECUTE        ; Not a compile only word
        rcall   STATE_          ; Compile only word check
        call    XSQUOTE
        db      d'12',"COMPILE ONLY"
        rcall   QABORT
IEXECUTE:
        bcf     FLAGS1, noclear
        call    EXECUTE
        btfsc   FLAGS1, noclear ;  set by \ and by (
        bra     IPARSEWORD
        bcf     FLAGS1, izeroeq ; Clear 0= encountered in compilation
        bcf     FLAGS1, idup    ; Clear DUP encountered in compilation
        bra     IPARSEWORD
ICOMPILE_1:
        bcf     FLAGS1, izeroeq ; Clear 0= encountered in compilation
        rcall   DUP_A
        rcall   LIT_A
        dw      ZEROEQUAL       ; Check for 0=, modifies IF and UNTIL to use bnz
        rcall   EQUAL
        rcall   ZEROSENSE
        bz      ICOMPILE_2
        bsf     FLAGS1, izeroeq ; Mark 0= encountered in compilation
        bra     ICOMMAXT
ICOMPILE_2:
        bcf     FLAGS1, idup    ; Clear DUP encountered in compilation
        rcall   DUP_A
        rcall   LIT_A
        dw      DUP             ; Check for DUP, modies IF and UNTIl to use DUPZEROSENSE
        rcall   EQUAL
        rcall   ZEROSENSE
        bz      ICOMPILE
        bsf     FLAGS1, idup    ; Mark DUP encountered during compilation
ICOMMAXT:
        rcall   COMMAXT_A
        bcf     FLAGS1, fTAILC  ; Allow tailgoto optimisation
        btfsc   wflags, 4       ; Compile only ?
        bsf     FLAGS1, fTAILC  ; Prevent tailgoto optimisation
        bra     IPARSEWORD
ICOMPILE:
        btfss   wflags, 5    ; Inline check
        bra     ICOMMAXT
        rcall   INLINE0
        bra     IPARSEWORD
INUMBER: 
        bcf     FLAGS1, izeroeq ; Clear 0= encountered in compilation
        bcf     FLAGS1, idup    ; Clear DUP encountered in compilation
        rcall   NUMBERQ
        movf    Sminus, W
        movf    Sminus, W
        bz      IUNKNOWN
        movf    state, F
        bz      INUMBER1
        sublw   1
        bz      ISINGLE
IDOUBLE:
        rcall   SWOP_A
        call    LITERAL
ISINGLE:        
        call    LITERAL
INUMBER1:
        bra     IPARSEWORD
IUNKNOWN:
        rcall   DP_TO_RAM
        rcall   CFETCHPP
        call    TYPE
        rcall   FALSE_
        rcall   QABORTQ         ; Never returns & resets the stacks
INOWORD: 
        goto    DROP

        db      NFA|1,"@"
FETCH_A:        
        goto    FETCH

;;;    bitmask -- 
        dw      L_INTERPRET
L_SHB:
        db      NFA|3,"shb"     ; Set header bit
SHB:
        rcall   LAST
        rcall   DUP_A
        rcall   CFETCH_A
        rcall   ROT_A
        call    OR
        rcall   SWOP_A
        goto    CSTORE
        
        dw      L_SHB
L_IMMEDIATE:
        db      NFA|9,"immediate" ; 
IMMEDIATE:
        rcall   LIT_A
        dw      IMMED
        bra     SHB

        db      NFA|4,"swap"
SWOP_A:
        goto    SWOP

;***************************************************************
        dw      L_IMMEDIATE
L_INLINED:
        db      NFA|7,"inlined" ; 
INLINED:
        rcall   LIT_A
        dw      INLINE
        bra     SHB
EMIT_A: goto    EMIT
;; .st ( -- ) output a string with current data section and current base info
;;; : .st base @ dup decimal <#  [char] , hold #s  [char] < hold #> type 
;;;     <# [char] > hold cse @ #s #> type base ! ;
        dw      L_INLINED
L_DOTSTATUS:
        db      NFA|3,".st"
DOTSTATUS:
        rcall   LIT_A
        dw      h'003c'
        rcall    EMIT_A
        call    DOTBASE
        rcall    EMIT_A
        rcall   LIT_A
        dw      h'002C'
        rcall    EMIT_A
        call    MEMQ
        call    TYPE
        rcall   LIT_A
        dw      h'003e'
        rcall    EMIT_A
        goto    DOTS
        
        db      NFA|3,"lit"
LIT_A:
        goto    LIT
        

        db      NFA|2,">r"
TOR_A:  goto    TOR


;;; TEN ( -- n ) Leave decimal 10 on the stack
        db      NFA|1,"a"
TEN:
        movlw   h'a'
        goto    WTOS

; dp> ( -- ) Copy ini, dps and latest from eeprom to ram
;        dw      link
; link    set     $
        db      NFA|3,"dp>"
DP_TO_RAM:
        rcall   LIT_A
        dw      dp_start
        rcall   INI
        rcall   TEN
        goto    CMOVE

; >dp ( -- ) Copy only changed turnkey, dp's and latest from ram to eeprom
;        dw      link
; link    set     $
        db      NFA|3,">dp"
DP_TO_EEPROM:
        rcall   LIT_A
        dw      dp_start
        rcall   STORE_P_TO_R
        rcall   INI
        movlw   5
        movwf   plusR
        bra     DP_TO_EEPROM_3
DP_TO_EEPROM_0: 
        rcall   FETCHPP
        call    DUP
        rcall   PFETCH
        call    XOR; NOTEQUAL
        movf    Sminus, W
        iorwf   Sminus, W
        bz      DP_TO_EEPROM_1
        rcall   PSTORE
        bra     DP_TO_EEPROM_2
DP_TO_EEPROM_1:
        call    DROP
DP_TO_EEPROM_2:
        rcall   PTWOPLUS
DP_TO_EEPROM_3:
        decf    Rrw, F
        bc      DP_TO_EEPROM_0
        movf    Rminus, W
        rcall   R_TO_P
        goto    DROP

;***************************************************************
        dw      L_DOTSTATUS
L_FALSE:
        db      NFA|INLINE|5,"false"
FALSE_:                     ; TOS is 0000 (FALSE)
        clrf    plusS         ; TOS_LO = 00
        clrf    plusS         ; TOS_HI = 00
        return

        dw      L_FALSE
L_TRUE:
        db      NFA|INLINE|4,"true"
TRUE_:                      ; TOS is ffff (TRUE)
        setf    plusS
        setf    plusS
        return
        
; QUIT     --    R: i*x --    interpret from kbd
        dw      L_TRUE
L_QUIT:
        db      NFA|4,"quit"
QUIT:
        call    RPEMPTY
        rcall   LEFTBRACKET
        call    FRAM
QUIT0:  
        ;; Copy INI and DP's from eeprom to ram
        rcall   DP_TO_RAM
QUIT1: 
        call    check_sp
        rcall   CR
        rcall   TIB
        rcall   DUP_A
        rcall   TIBSIZE
        rcall   TEN                 ; Reserve 10 bytes for hold buffer
        call    MINUS
        call    ACCEPT
        call    SPACE_
        rcall   INTERPRET
        movf    state, W
        bnz     QUIT1
        call    IFLUSH
        rcall   DP_TO_EEPROM
        call    XSQUOTE
        db      3," ok"
        call    TYPE
        rcall   PROMPT
        bra     QUIT0
        return

        dw      L_QUIT
L_PROMPT:
        db      NFA|6,"prompt"
PROMPT:
        call    DEFER_DOES
        dw      prompt

; ABORT    i*x --   R: j*x --   clear stk & QUIT
        dw      L_PROMPT
L_ABORT:
        db      NFA|5,"abort"
ABORT:
        rcall   S0
        rcall   FETCH_A
SPSTORE:
        movf    Sminus, W
        movwf   Tp
        movf    Sminus, W 
        movwf   Sp
        movf    Tp, W
        movwf   Sbank
        goto    QUIT

; ?ABORT?   f --       abort & print ?
        dw      L_ABORT
L_QABORTQ:
        db      NFA|7,"?abort?"
QABORTQ:
        call    XSQUOTE
        db      1,"?"
        goto    QABORT


; ?ABORT   f c-addr u --       abort & print msg
        dw      L_QABORTQ
L_QABORT:
        db      NFA|6,"?abort"
QABORT:
        rcall   ROT_A
        rcall   ZEROSENSE
        bnz     QABO1
QABORT1:        
        call    SPACE_
        call    TYPE
        rcall   ABORT  ; ABORT never returns
QABO1:  goto    TWODROP

        db      NFA|3,"(,)"
DOCOMMAXT_A: goto DOCOMMAXT

; ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;         i*x x1 --       R: j*x --      x1<>0
        dw      L_QABORT
L_ABORTQUOTE:
        db      NFA|IMMED|COMPILE|6,"abort\""
ABORTQUOTE:
        call    SQUOTE
        rcall   DOCOMMAXT_A
        dw      QABORT
        return

; '    -- xt             find word in dictionary
        dw      L_ABORTQUOTE
L_TICK:
        db      NFA|1,h'27'    ; 27h = '
TICK:
        rcall   BL
        rcall   WORD
        rcall   FIND
        goto    QABORTQ

; CHAR   -- char           parse ASCII character
        dw      L_TICK
L_CHAR:
        db      NFA|4,"char"
CHAR:
        rcall   BL
        rcall   PARSE
        call    DROP
        goto    CFETCH

; (    --                     skip input until )
        dw      L_CHAR
L_PAREN:
        db      NFA|IMMED|1,"("
PAREN:
        rcall   LIT_A
        dw      h'29'
        rcall   PARSE
        bsf     FLAGS1, noclear ; dont clear flags in case of (
        goto    TWODROP

        db      NFA|3,"(c)"
DOCREATE_A:
        goto    DOCREATE

; IHERE    -- a-addr    return Code dictionary ptr
;   IDP @ ;
;;;         dw      link
;;; link    set     $
        db      NFA|5,"ihere"
IHERE:
        rcall   IDP
        bra     FETCH_A

; [CHAR]   --          compile character literal
        dw      L_PAREN
L_BRACCHAR:
        db      NFA|IMMED|COMPILE|6,"[char]"
BRACCHAR:
        rcall   CHAR
        goto    LITERAL

; COMPILE,  xt --         append codefield
        db      NFA|3,"cf,"
COMMAXT_A:
        goto    COMMAXT

; CR      --                      output newline
        dw      L_BRACCHAR
L_CR:
        db      NFA|2,"cr"
CR:
        call    XSQUOTE
        db      2,"\r\n"
        goto    TYPE

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
        dw      L_CR
L_CREATE:
        db      NFA|6,"create"
CREATE:
        rcall   BL
        rcall   WORD            ; Parse a word

        rcall   DUP_A           ; Remember parsed word at rhere
        rcall   FIND
        call    NIP
        call    ZEROEQUAL
        call    XSQUOTE
        db      d'15',"ALREADY DEFINED"
        rcall   QABORT         ; ABORT if word has already been defined
        rcall   DUP_A           ; Check the word length 
        rcall   CFETCH_A
        call    ONE
        rcall   LIT_A
        dw      h'10'
        call    WITHIN
        rcall   QABORTQ          ; Abort if there is no name for create
        rcall   IHERE
        call    ALIGNED
        rcall   IDP             ; Align the flash DP.
        rcall   STORE_A
        rcall   LAST
        call    ICOMMA          ; Link field
        rcall   CFETCHPP        ; str len
        rcall   IHERE
        rcall   DUP_A             
        rcall   LATEST          ; new 'latest' link
        rcall   STORE_A         ; str len ihere
        rcall   PLACE           ; 
        rcall   IHERE           ; ihere
        call    CFETCH_A
        rcall   LIT_A
        dw      NFA
        rcall   SHB
        call    ONEPLUS
        call    ALIGNED
        rcall   IALLOT          ; The header has now been created
        rcall   DOCOMMAXT_A     ; Append an exeution token
        dw      DOCREATE        ; compiles the runtime routine to fetch the next dictionary cell to the parameter stack
        call    ALIGN
        call    HERE            ; compiles the current dataspace dp into the dictionary
        movf    cse, W
        bnz     CREATE2
        call    TWOPLUS
CREATE2:
        goto    ICOMMA          ; dp now points to a free cell

;***************************************************************
; POSTPONE
        dw     L_CREATE
L_POSTPONE:
        db      NFA|IMMED|COMPILE|8,"postpone"
POSTPONE:
        rcall   BL
        rcall   WORD
        rcall   FIND
        rcall   DUP_A
        rcall   QABORTQ
        call    ZEROLESS
        call    ZEROSENSE
        bz      POSTPONE1
        rcall   DOCOMMAXT_A
        dw      DOCOMMAXT
        goto    ICOMMA
POSTPONE1:
        goto   COMMAXT
;***************************************************************
; IDP    -- a-addr  Dictonary pointer storage        
; Stored in EEPROM
        ;;      dw link
        ;; link set $
        db      NFA|3,"idp"
IDP:
        rcall   DOCREATE_A
        dw      dpFLASH+PRAM


;***************************************************************
; (DOES>)  --      compile the run-time action of DOES>
;        dw     link
;link   set     $
        db      NFA|7,"(does>)"
XDOES:
        movf    TOSL, W
        movwf   plusS
        movf    TOSH, W
        movwf   plusS
        decf    STKPTR, F, A
        rcall   LAST
        rcall   NFATOCFA
        rcall   IDP
        rcall   FETCH_A
        rcall   TOR_A
        rcall   IDP
        rcall   STORE_A
        call    CALL_      ; Always stores a 4 byte call
        call    RFROM
        rcall   IDP
        goto    STORE


; DOES>    --      change action of latest def'n
        dw      L_POSTPONE
L_DOES:
        db      NFA|IMMED|COMPILE|5,"does>"
DOES:   rcall   DOCOMMAXT_A
        dw      XDOES
        rcall   DOCOMMAXT_A
        dw      DODOES
        return

        dw      L_DOES
L_LAST:
        db      NFA|3,"lst"
LAST:
        rcall   LATEST
        goto    FETCH

;*****************************************************************
; [        --      enter interpretive state
        dw      L_LAST
L_LEFTBRACKET:
        db      NFA|IMMED|1,"["
LEFTBRACKET:
        clrf    state
        return


; ]        --      enter compiling state
        dw      L_LEFTBRACKET
L_RIGHTBRACKET:
        db      NFA|1,"]"
RIGHTBRACKET:
        setf    state
        return

; :        --           begin a colon definition
        dw      L_RIGHTBRACKET
L_COLON:
        db      NFA|1,":"
COLON:
        rcall   CREATE
        rcall   RIGHTBRACKET
        goto    STORCOLON

; :noname        -- a          define headerless forth code
        dw      L_COLON
L_NONAME:
        db      NFA|7,":noname"
NONAME:
        rcall   IHERE
        goto    RIGHTBRACKET

; ;        --             end a colon definition
        dw      L_NONAME
L_SEMICOLON:
        db      NFA|IMMED|COMPILE|1,";"
SEMICOLON:
        rcall   LEFTBRACKET
        btfsc   FLAGS1, fTAILC
        bra     ADD_RETURN_1
        rcall   IHERE
        rcall   MINUS_FETCH
        movf    Sminus, W
        movf    Sminus, F
        andlw   0xf8
        sublw   0xd8
        bz      RCALL_TO_GOTO
        rcall   MINUS_FETCH
        movf    Sminus, W
        movf    Sminus, F
        sublw   0xec
        bnz     ADD_RETURN
CALL_TO_GOTO:
        rcall   LIT_A
        dw      0x300
        rcall   SWOP_A
        goto    PLUSSTORE
RCALL_TO_GOTO:
        rcall   FETCH_A
        movlw   h'f8'           ; These rcalls are always negative.
        iorwf   Srw, F
        call    TWOSTAR
        rcall   IHERE
        call    PLUS
        rcall   LIT_A
        dw      0xfffe          ; -2
        rcall   IALLOT
        goto    GOTO_
ADD_RETURN:
        call    DROP
ADD_RETURN_1:
        rcall   LIT_A   ; Compile a return
        dw      0x0012
        goto    ICOMMA


        dw      L_SEMICOLON
L_MINUS_FETCH:
        db      NFA|2,"-@"
MINUS_FETCH:
        rcall   TWOMINUS
        rcall   DUP_A
        goto    FETCH

; [']  --         find word & compile as literal
        dw      L_MINUS_FETCH
L_BRACTICK:
        db      NFA|IMMED|COMPILE|3,"[']"
BRACTICK:
        rcall   TICK       ; get xt of 'xxx'
        goto    LITERAL

; 2-    n -- n-2
        dw      L_BRACTICK
L_TWOMINUS:
        db      NFA|2,"2-"
TWOMINUS:
        swapf   Sminus, W
        movlw   2
        subwf   Splus, F
        movlw   0
        subwfb  Srw, F
        return

        
; BL      -- char                 an ASCII space
        dw      L_TWOMINUS
L_BL:
        db      NFA|2,"bl"
BL:
        movlw   h'20'
        goto    WTOS

; STATE   -- flag                 holds compiler state
        dw      L_BL
L_STATE:
        db      NFA|5,"state"
STATE_:
        movf    state, W
        movwf   plusS
        movwf   plusS
        return

; LATEST    -- a-addr           
        dw      L_STATE
L_LATEST:
        db      NFA|6,"latest"
LATEST:
        rcall   DOCREATE_A
        dw      dpLATEST+PRAM

; S0       -- a-addr      start of parameter stack
        dw      L_LATEST
L_S0:
        db      NFA|2,"s0"
S0:
        call    DOUSER
        dw      us0&h'ffff'
        
; ini -- a-addr       ini variable contains the user-start xt
; In RAM
;        dw      link
;link    set     $
        db      NFA|3,"ini"
INI:
        rcall   DOCREATE_A
        dw      dpSTART+PRAM

; ticks  -- u      system ticks (0-ffff) in milliseconds
        dw      L_S0
L_TICKS:
        db      NFA|5,"ticks"
TICKS:
        bcf     INTCON, GIE
        movf    ms_count, W
        movwf   plusS
        movf    ms_count+1, W
        movwf   plusS
        bsf     INTCON, GIE
        return
        

; ms  +n --      Pause for n millisconds
; : ms ( +n -- )     
;   ticks +
;   begin
;     pause dup ticks - 0<
;   until drop ;
;
        dw      L_TICKS
L_MS:
        db      NFA|2,"ms"
MS:
        rcall   TICKS
        call    PLUS
MS_1:
        call    PAUSE
        swapf   Sminus, W    ; timeout_hi
        movwf   Tp
        movf    Splus, W     ; timeout_lo
        bcf     INTCON, GIE
        subwf   ms_count, W  ; ticks_lo - timeout_lo
        swapf   Tp, W
        subwfb  ms_count+1, W ; ticks_hi - timeout_hi
        bsf     INTCON, GIE
        bn      MS_1
        goto    DROP

CFETCHPP_A: bra CFETCHPP
;  .id ( nfa -- )
        dw      L_MS
L_DOTID:
        db      NFA|3,".id"
DOTID:
        rcall   CFETCHPP
        movf    Sminus, W
        movf    Sminus, W
        andlw   h'f'
        movwf   plusR
        bra     DOTID3
DOTID1:
        rcall   CFETCHPP
        rcall   TO_PRINTABLE
        rcall    EMIT_A
DOTID3:
        decf    Rrw, F
        bc      DOTID1  
        movf    Rminus, W
        goto    DROP

 ; >pr   c -- c      Filter a character to printable 7-bit ASCII
        dw      L_DOTID
L_TO_PRINTABLE:
        db      NFA|3,">pr"
TO_PRINTABLE:   
        movf    Sminus, W
        movf    Srw, W
        bn      TO_PRINTABLE1
        sublw   0x1f
        bn      TO_PRINTABLE2
TO_PRINTABLE1:
        movlw   '.'
        movwf   Srw
TO_PRINTABLE2:
        clrf    plusS
        return
;;;;;;;;;;;;;;
CMP:
        rcall   TOR_A
        bra     CMP2
CMP1:
        rcall   CFETCHPP_A
        rcall   ROT_A
        rcall   CFETCHPP_A
        rcall   ROT_A
        call    MINUS
        call    ZEROSENSE
        bnz     TWODROPZ
CMP2:
        decf    Rrw, F
        bc      CMP1
        bra     TWODROPNZ
;;;;;;;;;;;;;;;;;;;;
LIKEQ:
        rcall   CFETCHPP_A
        rcall   LIT_A
        dw      0xf
        call    AND
        rcall   SWOP_A
        call    STORE_P
        rcall   SWOP_A
        rcall   CFETCHPP_A
        rcall   ROT_A
        call    OVER
        call    MINUS
        call    ONEPLUS
        rcall   FALSE_
        call    MAX
        rcall   TOR_A
        bra     LIKEQ3
LIKEQ1:
        call    TWODUP
        rcall   FETCH_P
        call    PPLUS
        rcall   SWOP_A
        rcall   CMP
        bz      LIKEQ3
TWODROPNZ:
        bcf     STATUS, Z
        bra     LIKEQ4
LIKEQ3:
        decf    Rrw, F
        bc      LIKEQ1
TWODROPZ:
        bsf     STATUS, Z
LIKEQ4:
        movwf   Rminus
        movwf   Rminus
        bra     DOTS2

;;;;;;;;;;;;;;;
LIKES:
        call    TWODUP
        rcall   LIKEQ
        bz      LIKES1
        rcall   DUP_A
        rcall   DOTID
        call    SPACE_
LIKES1:
        rcall   TWOMINUS
        rcall   FETCH_A
        call    DUPZEROSENSE
        bnz     LIKES
        bra     DOTS2

 ; WORDS    --          list all words in dict.
        dw      L_TO_PRINTABLE
L_WORDS:
        db      NFA|5,"words"
        rcall   BL
        call    WORD
        rcall   DUP_A
        rcall   LIT_A
        dw      kernellink
        rcall   WORDS1
        rcall   LAST
WORDS1: 
        rcall   CR
        bra     LIKES

; .S      --           print stack contents
; : .s sp@ s0 @ 1+ begin 2dup < 0= while @+ u. repeat 2drop ;
        dw      L_WORDS
L_DOTS:
        db      NFA|2,".s"
DOTS:
        call    SPFETCH
        rcall   S0
        rcall   FETCH_A
        call    ONEPLUS
DOTS1:
        call    TWODUP
        call    LESS
        call    ZEROSENSE
        bnz     DOTS2
        call    FETCHPP
        call    UDOT
        bra     DOTS1
DOTS2:  
        goto    TWODROP

;   DUMP  ADDR U --       DISPLAY MEMORY
        dw      L_DOTS
L_DUMP:
        db      NFA|4,"dump"
DUMP:
        rcall   LIT_A
        dw      h'10'
        call    USLASH
        rcall   TOR_A
        bra     DUMP7
DUMP1:  
        rcall   CR
        rcall   DUP_A
        rcall   LIT_A
        dw      4
        call    UDOTR
        rcall   LIT_A
        dw      h'3a'
        rcall   EMIT_A
        movlw   h'10'
        movwf   plusR
DUMP2:
        rcall   CFETCHPP_A
        rcall   LIT_A
        dw      2
        call    UDOTR
        decf    Rrw, F
        bnz     DUMP2
        movf    Rminus, W

        rcall   LIT_A
        dw      h'10'
        call    MINUS
        movlw   h'10'
        movwf   plusR
DUMP4:  
        rcall   CFETCHPP_A
        rcall   TO_PRINTABLE
        rcall   EMIT_A
        decf    Rrw, F
        bnz     DUMP4
        movf    Rminus, W
DUMP7:
        decf    Rminus, F
        movlw   0
        subwfb  Rplus, F
        bc      DUMP1
        rcall   RDROP
        goto    DROP

; IALLOT   n --    allocate n bytes in ROM
;       dw      link
;link   set     $
        db      NFA|1," "
IALLOT:
        rcall   IDP
        goto    PLUSSTORE
        
;***************************************************************
; check that the relative address is within reach of conditional branch
; instructions and leave the clipped relative address on the stack
; br?   ( rel-addr limit -- clipped-rel-addr)
;       2dup 2/ swap
;       abs > (qabort)
;       and 2/ ;
        dw     L_DUMP
L_BRQ:
        db      NFA|3,"br?"
BRQ:
        call    TWODUP
        call    TWOSLASH
        rcall   SWOP_A          ; rel-addr limit limit' rel-addr
        call    ABS             ; rel-addr limit limit' rel-addr
        call    GREATER
        call    XSQUOTE
        db      3,"BR?"
        rcall   QABORT         ;  ?RANGE ABORT if TRUE
BRQ1:
        call    AND
        goto    TWOSLASH

; ,?0=    -- addr  Compile ?0= and make make place for a branch instruction
        db      NFA|4,",?0="    ; Just for see to work !
COMMAZEROSENSE:
        btfsc   FLAGS1, idup
        bra     COMMAZEROSENSE1
        rcall   LIT_A
        dw      ZEROSENSE
        bra     COMMAZEROSENSE2
COMMAZEROSENSE1:
        rcall   IDPMINUS
        rcall   LIT_A
        dw      DUPZEROSENSE
COMMAZEROSENSE2:
        bcf     FLAGS1, idup
        bra     INLINE0

; z, ( -- cc)   Zero
        dw      L_BRQ
L_ZC:
        db      NFA|2,"z,"
ZC:
        rcall   DOCREATE_A
        dw      h'e0'

; nz, ( -- cc)  Not Zero
        dw      L_ZC
L_NZC:
        db      NFA|3,"nz,"
NZC:
        rcall   DOCREATE_A
        dw      h'e1'

; cc, ( -- cc) Carry
        dw      L_NZC
L_NCC:
        db      NFA|3,"nc,"
NCC:
        rcall   DOCREATE_A
        dw      h'e3'

; not, ( cc -- opposite-cc)  Reverse the condition code
        dw      L_NCC
L_NOTC:
        db      NFA|4,"not,"
NOTC:
        call    ONE
        goto    XOR


; if, ( cc -- here)  Assembler if
        dw      L_NOTC
L_IFC:
        db      NFA|3,"if,"
IFC:
        rcall   IHERE
        rcall   FALSE_
        call    ICCOMMA
        rcall   SWOP_A
        rcall   NOTC
        goto    ICCOMMA

; then, ( back-addr -- )  Assembler then
        dw      L_IFC
L_THENC:
        db      NFA|5,"then,"
THENC:
        bsf     FLAGS1, fTAILC  ; Disable tail call optimisation
        rcall   DUP_A
        rcall   FETCH_A         ; back-addr oper
        movf    Srw, W
        call    TOR
                              ; bra has longer range
        sublw   h'd0'         ; Also DUP
        bz      THENC1
THENC0: 
        rcall   LIT_A
        dw      h'1ff'          ;  back-addr mask 
        bra     THENC2
THENC1:
        rcall   LIT_A
        dw      h'0fff'         ; back-addr mask
THENC2: 
        rcall   TOR_A           ; back-addr
        rcall   IHERE           ; back-addr ihere
        call    OVER            ; back-addr ihere back-addr
        call    MINUS           ; back-addr rel-addr
        rcall   TWOMINUS        ; back-addr rel-addr
        call    RFROM           ; back-addr rel-addr mask
        rcall   BRQ             ; back-addr rel-addr
        call    RFROM           ; back-addr rel-addr oper
        call    OR              ; back-addr instruction
        rcall   SWOP_A
        goto    STORE
        
; else, ( back-addr -- here )  Assembler else
        dw      L_THENC
L_ELSEC:
        db      NFA|5,"else,"
ELSEC:
        rcall   IHERE
        rcall   FALSE_
        call    BRA_
        rcall   SWOP_A
        goto    THENC

; begin,    -- adrs        target for bwd. branch
        dw      L_ELSEC
L_BEGINC:
        db      NFA|6,"begin,"
BEGINC:
        goto    IHERE
        
; again,    adrs --      uncond'l backward branch
;   unconditional backward branch
        dw      L_BEGINC
L_AGAINC:
        db      NFA|6,"again,"
AGAINC:
        rcall   IHERE
        call    MINUS
        rcall   TWOMINUS
        goto    BRA_

; until,    adrs cc --   Branch bakwards if cc
        dw      L_AGAINC
L_UNTILC:
        db      NFA|6 ,"until,"
UNTILC:
        bsf     FLAGS1, fTAILC  ; Disable tail call optimisation
        rcall   SWOP_A
        rcall   IHERE
        call    MINUS
        rcall   TWOMINUS
        rcall   LIT_A
        dw      h'1ff'
        rcall   BRQ
        call    ICCOMMA
        rcall   NOTC
        goto    ICCOMMA

; IF       -- adrs   conditional forward branch
; Leaves address of branch instruction 
; and compiles the condition byte
        dw      L_UNTILC
L_IF:
        db      NFA|IMMED|COMPILE|2,"if"
IF_:    
        btfsc   FLAGS1, izeroeq
        rcall   IDPMINUS
        rcall   COMMAZEROSENSE
        rcall   NZC
        btfsc   FLAGS1, izeroeq
        rcall   NOTC
        bcf     FLAGS1, izeroeq
        goto    IFC
        

; THEN     adrs  --        resolve forward branch
        dw      L_IF
L_THEN:
        db      NFA|IMMED|COMPILE|4,"then"
THEN_:
        goto    THENC

; ELSE     adrs1 -- adrs2    branch for IF..ELSE
; Leave adrs2 of bra instruction and store bz in adrs1
; Leave adress of branch instruction and FALSE flag on stack
        dw      L_THEN
L_ELSE:
        db      NFA|IMMED|COMPILE|4,"else"
ELSE_:
        goto    ELSEC

; BEGIN    -- adrs        target for bwd. branch
        dw      L_ELSE
L_BEGIN:
        db      NFA|IMMED|COMPILE|5,"begin"
BEGIN:
        goto    IHERE
        
;;; Forget the latest compiled two cell instruction
        db      NFA|1," "
IDPMINUS:
        rcall   LIT_A
        dw      -4
        goto    IALLOT

; UNTIL    adrs --   Branch bakwards if true
        dw      L_BEGIN
L_UNTIL:
        db      NFA|IMMED|COMPILE|5,"until"
UNTIL:
        btfsc   FLAGS1, izeroeq
        rcall   IDPMINUS
        rcall   COMMAZEROSENSE
        rcall   ZC
        btfss   FLAGS1, izeroeq
        rcall   NOTC
        bcf     FLAGS1, izeroeq
        goto    UNTILC
        
; AGAIN    adrs --      uncond'l backward branch
;   unconditional backward branch
        dw      L_UNTIL
L_AGAIN:
        db      NFA|IMMED|COMPILE|5,"again"
AGAIN:
        goto    AGAINC

; WHILE    addr1 -- addr2 addr1         branch for WHILE loop
; addr1 : address of BEGIN
; addr2 : address where to store bz instruction
        dw      L_AGAIN
L_WHILE:
        db      NFA|IMMED|COMPILE|5,"while"
WHILE_:
        rcall   IF_
        goto    SWOP

; REPEAT   addr2 addr1 --     resolve WHILE loop
        dw      L_WHILE
L_REPEAT:
        db      NFA|IMMED|COMPILE|6,"repeat"
REPEAT_:
        rcall   AGAIN
        goto    THEN_

; INLINE  "name" --    Inline following word
        dw      L_REPEAT
L_INLINE:
        db      NFA|IMMED|COMPILE|6,"inline"
        bcf     FLAGS1, izeroeq
        bcf     FLAGS1, idup
        rcall   TICK
        goto    INLINE0

; in, ( addr -- ) begin @+ dup $12 <> while i, repeat 2drop ;
        dw      L_INLINE
L_INLINEC:
        db      NFA|3,"in,"
INLINE0:        
        call    FETCHPP
        movf    Srw, W
        bnz     INLINE2
        movf    Sminus, W
        movf    Splus, W
        sublw   h'12'
        bz      INLINE1
INLINE2:
        call    ICOMMA
        bra     INLINE0
INLINE1:
        goto    TWODROP

; FOR   -- bc-addr bra-addr
; ['] >r cf, ihere ['] (for) cf, ihere
        dw      L_INLINEC
L_FOR:
        db      NFA|IMMED|COMPILE|3,"for"
FOR:
        rcall   DOCOMMAXT_A
        dw      TOR
        rcall   IHERE
        rcall   FALSE_
        call    BRA_
        rcall   IHERE
        goto    SWOP

; NEXT bra-addr bc-addr --
        dw      L_FOR
L_NEXT:
        db      NFA|IMMED|COMPILE|4,"next"
NEXT:
        rcall   THENC
        rcall   LIT_A
        dw      XNEXT
        rcall   INLINE0
        rcall   NCC
        rcall   UNTILC
        rcall   LIT_A
        dw      RDROP
        goto    INLINE0


; (next) decrement top of return stack
XNEXT:  
        decf    Rminus, F
        movlw   h'0'
        subwfb  Rplus, F
        return

; endit clear top of return stack
        dw      L_NEXT
L_ENDIT:
        db      NFA|COMPILE|5,"endit"
ENDIT:
        clrf    Rminus
        clrf    Rplus
        return

; RDROP compile a pop
        dw      L_ENDIT
L_RDROP:
        db      NFA|INLINE|5,"rdrop"
RDROP:
        movwf    Rminus
        movwf    Rminus ;status flags are not affected my movwf
        return

; S>D  n -- d
        dw      L_RDROP
L_STOD:
        db      NFA|3,"s>d"
STOD:
        btfsc   Splus, 7
        goto    test_true        
        goto    test_false
        

; DNEGATE  +d -- -d
        dw      L_STOD
L_DNEGATE:
        db      NFA|7,"dnegate"
DNEGATE:
        rcall   DINVERT
        call    ONE
        goto    MPLUS

; DNEGATE  d -n -- -d
        dw      L_DNEGATE
L_QDNEGATE:
        db      NFA|8,"?dnegate"
QDNEGATE:
        movf    Sminus, W
        movwf   Sminus
        bnn     QDNEGATE1
        rcall   DNEGATE
QDNEGATE1:
        return

; DABS  -d -- d
        dw      L_QDNEGATE
L_DABS:
        db      NFA|4,"dabs"
DABS:
        btfsc   Srw, 7
        rcall   DNEGATE
        return

; D+       d d -- d         add double to double
        dw      L_DABS
L_DPLUS
        db      NFA|2,"d+"
DPLUS:
        movlw   7
        subwf   Sp, F
        movlw   0
        subwfb  Sbank, F
        
        movlw   4
        movf    SWrw, W
        addwf   Splus, F

        movlw   4
        movf    SWrw, W
        addwfc  Splus, F

        movlw   4
        movf    SWrw, W
        addwfc  Splus, F

        movlw   4
        movf    SWrw, W
        addwfc  Srw, F
        
        return

; D-    d1 d2 -- d3        double minus
        dw      L_DPLUS
L_DMINUS:
        db      NFA|2,"d-"
DMINUS:
        rcall   DNEGATE
        goto    DPLUS

; D2/    d1 -- d2        double divide by 2
        dw      L_DMINUS
L_DTWOSLASH:
        db      NFA|3,"d2/"
DTWOSLASH:
        bcf     STATUS, C
        btfsc   Srw, 7
        bsf     STATUS, C
        rrcf    Sminus, F
        rrcf    Sminus, F
        rrcf    Sminus, F
        rrcf    Splus, F
        movf    Splus, W
        movf    Splus, W
        return

; D2*    d1 -- d2        double multiply by 2
        dw      L_DTWOSLASH
L_DTWOSTAR:
        db      NFA|3,"d2*"
DTWOSTAR:
        movf    Sminus, W
        movf    Sminus, W
        movf    Sminus, W
        bcf     STATUS, C
        rlcf    Splus, F
        rlcf    Splus, F
        rlcf    Splus, F
        rlcf    Srw, F
        return
        
; DINVERT    d1 -- d2        double invert
        dw      L_DTWOSTAR
L_DINVERT:
        db      NFA|7,"dinvert"
DINVERT:
        comf    Sminus, F
        comf    Sminus, F
        comf    Sminus, F
        comf    Splus, F
        movf    Splus, W
        movf    Splus, W
        return        
        
; D0=    d1 -- f        double zeroequal
        dw      L_DINVERT
L_DZEROEQUAL:
        db      NFA|3,"d0="
DZEROEQUAL:
        movf    Sminus, W
        iorwf   Sminus, W
        iorwf   Sminus, W
        iorwf   Sminus, W
        bnz     DZEROLESS_FALSE
DZEROEQUAL_TRUE:
        goto    TRUE_

; D0<    d1 -- f        double zeroless
        dw      L_DZEROEQUAL
L_DZEROLESS:
        db      NFA|3,"d0<"
DZEROLESS:
        movf    Sminus, W
        movf    Sminus, F
        movf    Sminus, F
        movf    Sminus, F
        addlw   0
        bn      DZEROEQUAL_TRUE
DZEROLESS_FALSE:
        goto    FALSE_
        
        
; D=    d1 d2 -- f        double equal
        dw      L_DZEROLESS
L_DEQUAL:
        db      NFA|2,"d="
DEQUAL:
        rcall   DMINUS
        goto    DZEROEQUAL
        
; D<    d1 d2 -- f        double less than
        dw      L_DEQUAL
L_DLESS:
        db      NFA|2,"d<"
DLESS:
        rcall   DMINUS
        goto    DZEROLESS

; D>    d1 d2 -- f        double greater than
        dw      L_DLESS
L_DGREATER:
        db      NFA|2,"d>"
DGREATER:
        call    TWOSWAP
        goto    DLESS


; UD.       ud --         unsigned double dot
        dw      L_DGREATER
L_UDDOT:
        db      NFA|3,"ud."
UDDOT:
        call    LESSNUM
        call    NUMS
        call    NUMGREATER
        call    TYPE
        goto    SPACE_
        
; D.       d --         signed double dot
        dw      L_UDDOT
L_DDOT:
        db      NFA|2,"d."
DDOT:
        call    LESSNUM
        call    DUP
        call    TOR
        rcall   DABS
        call    NUMS
        call    RFROM
        call    SIGN
        call    NUMGREATER
        call    TYPE
        goto    SPACE_

        dw      L_DDOT
L_MEMHI:
        db      NFA|2,"hi"
MEMHI:
        call    LIT
        dw      FLASHHI
        call    CSE
        call    PLUS
        goto    FETCH
FLASHHI:
        dw      FLASH_HI
        dw      EEPROM_HI
        dw      RAM_HI
;***************************************************
        dw      L_MEMHI
L_FETCH_P:
        db      NFA|2,"@p"
FETCH_P:
        movf    p_lo, W
        movwf   plusS
        movf    p_hi, W
        movwf   plusS
        return
;***************************************************
        dw      L_FETCH_P
L_PCFETCH:
        db      NFA|3,"pc@" ; ( -- c ) Fetch char from pointer
PCFETCH:
        movf    p_lo, W
        movwf   plusS
        movf    p_hi, W
        movwf   plusS
        goto    CFETCH
;***************************************************
        dw      L_PCFETCH
L_PNPLUS:
        db      NFA|3,"p++" ; ( n -- ) Add n to p
PNPLUS:
        movf    Sminus, W
        movwf   Tp
        movf    Sminus, W
        addwf   p_lo, F
        movf    Tp, W
        addwfc  p_hi, F
        return
;***************************************************
; marker --- name
        dw      0
L_MARKER:
lastword:
        db      NFA|6,"marker"
MARKER:
        call    ROM
        rcall   CREATE
        call    LIT
        dw      dp_start
        call    HERE
        call    TEN
        call    CMOVE
        call    TEN
        call    ALLOT
        call    FRAM
        rcall   XDOES
        call    DODOES
        rcall   INI
        call    TEN
        goto    CMOVE

;        dw      L_RDROP
L_DOTBASE:
        db      NFA|1,"I"
DOTBASE:
        call    BASE
        call    FETCH_A
        movf    Sminus, W
        movf    Srw, W
        xorlw   0x10
        bnz     DOTBASE1
        movlw   '$'
        bra     DOTBASEEND
DOTBASE1:
        xorlw   0x1a
        bnz     DOTBASE2
        movlw   '#'
        bra     DOTBASEEND
DOTBASE2:
        xorlw   0x8
        bnz     DOTBASE3
        movlw   '%'
        bra     DOTBASEEND
DOTBASE3:
        movlw   '?'
DOTBASEEND:
        movwf   Srw
        clrf    plusS
        return

FF_END_CODE code
MEMQADDR_N:
        dw      ROM_N
        dw      EROM_N
        dw      FRAM_N
; M? -- caddr count    current data space string
;        dw      L_DOTBASE
L_MEMQ:
        db      NFA|1,"I"
MEMQ:
        call    CSE
        call    LIT
        dw      MEMQADDR_N
        call    PLUS
        call    FETCH_A
        call    CFETCHPP
        call    LIT
        dw      NFAmask
        goto    AND
end_of_dict:
FF_DP code
dpcode:
;****************************************************
        org EEDATA_LO   ; h'f00000'
        de  h'ff', h'ff'
;        de  dp_user_dictionary&0xff, (dp_user_dictionary>>8)&0xff
;        de  dpeeprom&0xff, (dpeeprom>>8)&0xff
;        de  (dpdata)&0xff, ((dpdata)>>8)&0xff
;        de  lastword_lo, lastword_hi
;        de  DOTSTATUS;&0xff;, (DOTSTATUS>>8)&0xff

       end
;********************************************************** 
