;**********************************************************************
;                                                                     *
;    Filename:      ff-xc8.asm                                        *
;    Date:          01.04.2023                                        *
;    File Version:  5.0                                               *
;    MCU:           Atmega                                            *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2023  Mikael Nordman

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
#include <xc.h>
#include <config-xc8.inc>

; Define the FF version date string
#define DATE "01.04.2023"
#define datelen 10


#if OPERATOR_UART == 1
.equ OP_TX_, TX1_
.equ OP_RX_, RX1_
.equ OP_RXQ, RX1Q
#else
#if OPERATOR_UART == 0
.equ OP_TX_, TX0_
.equ OP_RX_, RX0_
.equ OP_RXQ, RX0Q
#else
#if OPERATOR_UART == 3
.equ OP_TX_, TXU_
.equ OP_RX_, RXU_
.equ OP_RXQ, RXUQ_
#endif
#endif
#endif

#define ubrr0val (FREQ_OSC/16/BAUDRATE0) - 1
#define ubrr1val (FREQ_OSC/16/BAUDRATE1) - 1

#if FREQ_OSC < 16384000
.equ ms_value_tmr0, ((FREQ_OSC/1000/64) - 1)
.equ ms_value_tmr1, ((FREQ_OSC/1000) - 1)
.equ ms_value_tmr2, ((FREQ_OSC/1000/64) - 1)
#ifdef TCCR0B
.equ ms_pre_tmr0, 3
#endif
#ifdef TCCR0
.equ ms_pre_tmr0, 4
#endif
#ifdef TCCR2B
.equ ms_pre_tmr2, 4
#endif
#ifdef TCCR2
.equ ms_pre_tmr2, 3
#endif

#else // FREQ_OSC >= 16384000 Hz

.equ ms_value_tmr0, ((FREQ_OSC/1000/256) - 1)
.equ ms_value_tmr1, ((FREQ_OSC/1000) - 1)
.equ ms_value_tmr2, ((FREQ_OSC/1000/128) - 1)
#ifdef TCCR0B
.equ ms_pre_tmr0, 4
#endif
#ifdef TCCR0
.equ ms_pre_tmr0, 6
#endif
#ifdef TCCR2B
.equ ms_pre_tmr2, 5
#endif
#ifdef TCCR2
.equ ms_pre_tmr2, 4
#endif
#endif
.equ CPU_LOAD_VAL, (FREQ_OSC*255/100000)
;..............................................................................
;Program Specific Constants (literals used in code)
;..............................................................................
; Flash page size
.equ PAGESIZEB, SPM_PAGESIZE    ; Page size in bytes 

; Forth word header flags
.equ NFA, 0x80      ; Name field mask
.equ IMMED, 0x40    ; Immediate mask
.equ INLINE, 0x20   ; Inline mask for 1 and 2 cell code
.equ INLINE4, 0x00  ; Inline mask for 4 cell code
.equ INLINE5, 0x00  ; Inline mask for 5 cell code
.equ COMPILE, 0x10  ; Compile only mask
.equ NFAmask, 0xf   ; Name field length mask

; FLAGS2
.equ fIDLE,     6   ; 0 = busy, 1 = idle
.equ fLOAD,     5   ; Load measurement ready
.equ fLOADled,  4   ; 0 = no load led, 1 = load led on
.equ ixoff_tx1, 1                    
.equ ixoff_tx0, 0

; FLAGS1
.equ fLIT,    7     ; Literal compiled
.equ noclear, 6     ; dont clear optimisation flags 
.equ idup,    5     ; Use dupzeroequal instead of zeroequal
.equ izeroeq, 4     ; Use brne instead of breq if zeroequal
.equ istream, 3
.equ fLOCK,   2
.equ fTAILC,  1
.equ idirty,  0

;;; For Flo8 Control
.equ XON,   0x11
.equ XOFF,  0x13

.equ CR_,0x0d
.equ LF_,0x0a
.equ BS_,0x08
.equ TAB_,0x09

#ifdef USBCON
#define USB_CODE 0x300
#else
#define USB_CODE 0
#endif

;;; Memory mapping prefixes
#define PRAM     0x0000
#define PEEPROM  (RAMEND + 1)
#define SPM_SIZE    0x200
#define OFLASH      (PEEPROM + E2END + 1)
#define PFLASH      OFLASH

#if (FLASHEND == 0x3ffff)             // 128 Kwords flash
#define KERNEL_SIZE 0x2200
#define FLASH_HI    (0xffff)

#elif (FLASHEND == 0x1ffff)            // 64 Kwords flash
#define KERNEL_SIZE 0x2100
#define FLASH_HI    (0xffff)

#elif (FLASHEND == 0xffff)             // 32 Kwords flash
#define KERNEL_SIZE 0x2000
#define FLASH_HI    (0xffff)

#elif (FLASHEND == 0x7fff)            // 16 Kwords flash
#define KERNEL_SIZE (0x2000 + USB_CODE)
#define FLASH_HI    (FLASHEND + PFLASH - SPM_SIZE)

#elif (FLASHEND == 0x3fff)            // 8  Kwords flash
#define KERNEL_SIZE (0x2000 + USB_CODE)
#define FLASH_HI    (FLASHEND + PFLASH - SPM_SIZE)
#endif

#define NRWW_START  (FLASHEND - SPM_SIZE + 1)

;;;  High values for memory areas
.equ FLASH_LOW, KERNEL_SIZE + 0x100
.equ EEPROM_HI , PEEPROM + E2END
.equ RAM_HI , RAMEND

#include <macros-xc8.inc>
        
;;; USER AREA for the OPERATOR task
.equ ursize,       RETURN_STACK_SIZE
.equ ussize,       PARAMETER_STACK_SIZE
.equ utibsize,     TIB_SIZE

;;; User variables and area
.equ us0,          -28         ; Start of parameter stack
.equ ur0,          -26         ; Start of ret stack
.equ uemit,        -24         ; User EMIT vector
.equ ukey,         -22         ; User KEY vector
.equ ukeyq,        -20         ; User KEY? vector
.equ ubase,        -18         ; Number Base
.equ utib,         -16         ; TIB address
.equ utask,        -14         ; Task area pointer
.equ ustatus,      -12
.equ uflg,         -11
.equ usource,      -10         ; Two cells
.equ utoin,        -6          ; Input stream
.equ ulink,        -4          ; Task link (up0)
.equ ursave,       -2          ; Saved ret stack pointer
.equ uhp,           0          ; Hold pointer


;;; Variables in EEPROM
.equ eeprom,       PEEPROM
.equ dp_start,     eeprom + 0x0000 ; TURNKEY
.equ dp_flash,     eeprom + 0x0002 ; FLASH dictionary pointer
.equ dp_eeprom,    eeprom + 0x0004 ; EEPROM dictionary pointer
.equ dp_ram,       eeprom + 0x0006 ; RAM dictionary pointer
.equ latest,       eeprom + 0x0008 ; Pointer to latest dictionary word
.equ prompt,       eeprom + 0x000a ; Deferred prompt
.equ ehere,        eeprom + 0x000c

;****************************************************
.section .bss
#ifdef USBCON
usb_config_status:  .space 1
bmRequestType:      .space 1
bRequest:           .space 1
wValue:             .space 2
wIndex:             .space 2
wLength:            .space 2
line_coding:        .space 7
#endif

ibuf:         .space PAGESIZEB
ivec:         .space SPACE_VECTORS_SIZE

rxqueue0:
rbuf0_wr:    .space 1
rbuf0_rd:    .space 1
rbuf0_lv:    .space 1
rbuf0:       .space RX0_BUF_SIZE

#if UARTS == 2
rxqueue1:
rbuf1_wr:    .space 1
rbuf1_rd:    .space 1
rbuf1_lv:    .space 1
rbuf1:       .space RX1_BUF_SIZE
#endif

litbuf0:    .space 1
litbuf1:    .space 1

dpSTART:    .space 2
dpFLASH:    .space 2 ; DP's and LATEST in RAM
dpEEPROM:   .space 2
dpRAM:      .space 2
dpLATEST:   .space 2

iaddrl:     .space 1
iaddrh:     .space 1
#if FLASHEND > 0xffff
iaddru:	    .space 1
ibaseu:	    .space 1
#endif

#if IDLE_MODE == 1
#if CPU_LOAD == 1       
load_acc:   .space 3 ; Load measurement accumulator
load_res:   .space 3 ; Load result
#endif
#endif

cse:        .space 1 ; Current data section 0=flash, 1=eeprom, 2=ram
state:      .space 1 ; Compilation state
uvars:      .space   (-us0)
up0:        .space   2
urbuf:      .space   ursize
usbuf:      .space   ussize
utibbuf:    .space   utibsize
dpdata:     .space   2

;*******************************************************************
; Start of kernel
;*******************************************************************
#define INT_VECTORS_SIZE (_VECTORS_SIZE/2 + 4)
.equ  SPACE_VECTORS_SIZE, INT_VECTORS_SIZE

.section .vectors
RESET_:   jmp  WARM_0
ISR2:     rcall FF_ISR
          nop
ISR3:     rcall FF_ISR
          nop
ISR4:     rcall FF_ISR
          nop
ISR5:
#if MS_VECTOR == 5
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR6:     rcall FF_ISR
          nop
ISR7:     rcall FF_ISR
          nop
ISR8:
#if MS_VECTOR == 8
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR9:     rcall FF_ISR
          nop
ISR10:
#if MS_VECTOR == 10
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR11:
#if MS_VECTOR == 11
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR12:
#if MS_VECTOR == 12
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR13:
#if MS_VECTOR == 13
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR14:
#if MS_VECTOR == 14
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR15:
#if MS_VECTOR == 15
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR16:
#if MS_VECTOR == 16
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR17:
#if MS_VECTOR == 17
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR18:
#if MS_VECTOR == 18
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
ISR19:    rcall FF_ISR
          nop
#if 0x26 < INT_VECTORS_SIZE
ISR20:    rcall FF_ISR
          nop
#endif
#if 0x28 < INT_VECTORS_SIZE
ISR21:    rcall FF_ISR
          nop
#endif
#if 0x2a < INT_VECTORS_SIZE
ISR22:
#if MS_VECTOR == 22
          rjmp  MS_TIMER_ISR
#else
          rcall FF_ISR
#endif
          nop
#endif
#if 0x2c < INT_VECTORS_SIZE
ISR23:    rcall FF_ISR
          nop
#endif
#if 0x2e < INT_VECTORS_SIZE
ISR24:    rcall FF_ISR
          nop
#endif
#if 0x30 < INT_VECTORS_SIZE
ISR25:    rcall FF_ISR
          nop
#endif
#if 0x32 < INT_VECTORS_SIZE
ISR26:    rcall FF_ISR
          nop
#endif
#if 0x34 < INT_VECTORS_SIZE
ISR27:    rcall FF_ISR
          nop
#endif
#if 0x36 < INT_VECTORS_SIZE
ISR28:    rcall FF_ISR
          nop
#endif
#if 0x38 < INT_VECTORS_SIZE
ISRx38:   rcall FF_ISR
          nop
#endif
#if 0x3a < INT_VECTORS_SIZE
ISRx3a:   rcall FF_ISR
          nop
#endif
#if 0x3c < INT_VECTORS_SIZE
ISRx3c:   rcall FF_ISR
          nop
#endif
#if 0x3e < INT_VECTORS_SIZE
ISRx3e:   rcall FF_ISR
          nop
#endif
#if 0x40 < INT_VECTORS_SIZE
ISRx40:   rcall FF_ISR
          nop
#endif
#if 0x42 < INT_VECTORS_SIZE
ISRx42:   rcall FF_ISR
          nop
#endif
#if 0x44 < INT_VECTORS_SIZE
ISRx44:   rcall FF_ISR
          nop
#endif
#if 0x46 < INT_VECTORS_SIZE
ISRx46:   rcall FF_ISR
          nop
#endif
#if 0x48 < INT_VECTORS_SIZE
ISRx48:   rcall FF_ISR
          nop
#endif
#if 0x4a < INT_VECTORS_SIZE
ISRx4a:   rcall FF_ISR
          nop
#endif
#if 0x4c < INT_VECTORS_SIZE
ISRx4c:   rcall FF_ISR
          nop
#endif
#if 0x4e < INT_VECTORS_SIZE
ISRx4e:   rcall FF_ISR
          nop
#endif
#if 0x50 < INT_VECTORS_SIZE
ISRx50:   rcall FF_ISR
          nop
#endif
#if 0x52 < INT_VECTORS_SIZE
ISRx52:   rcall FF_ISR
          nop
#endif
#if 0x54 < INT_VECTORS_SIZE
ISRx54:   rcall FF_ISR
          nop
#endif
#if 0x56 < INT_VECTORS_SIZE
ISRx56:   rcall FF_ISR
          nop
#endif
#if 0x58 < INT_VECTORS_SIZE
ISRx58:   rcall FF_ISR
          nop
#endif
#if 0x5a < INT_VECTORS_SIZE
ISRx5a:   rcall FF_ISR
          nop
#endif
#if 0x5c < INT_VECTORS_SIZE
ISRx5c:   rcall FF_ISR
          nop
#endif
#if 0x5e < INT_VECTORS_SIZE
ISRx5e:   rcall FF_ISR
          nop
#endif
#if 0x60 < INT_VECTORS_SIZE
ISRx60:   rcall FF_ISR
          nop
#endif
#if 0x62 < INT_VECTORS_SIZE
ISRx62:   rcall FF_ISR
          nop
#endif
#if 0x64 < INT_VECTORS_SIZE
ISRx64:   rcall FF_ISR
          nop
#endif
#if 0x66 < INT_VECTORS_SIZE
ISRx66:   rcall FF_ISR
          nop
#endif
#if 0x68 < INT_VECTORS_SIZE
ISRx68:   rcall FF_ISR
          nop
#endif
#if 0x6a < INT_VECTORS_SIZE
ISRx6a:   rcall FF_ISR
          nop
#endif
#if 0x6c < INT_VECTORS_SIZE
ISRx6c:   rcall FF_ISR
          nop
#endif
#if 0x6e < INT_VECTORS_SIZE
ISRx6e:   rcall FF_ISR
          nop
#endif
#if 0x70 < INT_VECTORS_SIZE
ISRx70:   rcall FF_ISR
          nop
#endif

FF_ISR_EXIT:
        pop     tosh
        pop     tosl
        pop     t1
        pop     t0
        pop     zh
        pop     zl
MS_TIMER_ISR_EXIT_LOAD:
        ld      xl, y+
        ld      xh, y+
MS_TIMER_ISR_EXIT:
        m_out    SREG, xh
        ld      xh, y+
        reti
        
FF_ISR:
#if IDLE_MODE == 1
#if CPU_LOAD == 1
        m_out    TCCR1B, r_one   ; Start load counter
#endif
#endif
        st      -y, xh
        m_in     xh, SREG
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
        clr     r_zero
        ldi     t0, lo8(ivec+1)
        add     xl, t0
        ldi     xh, hi8(ivec)
        ld      zl, x+
        ld      zh, x+
        m_ijmp   ;(z)

;;; *************************************************
MS_TIMER_ISR:
#if IDLE_MODE == 1
#if CPU_LOAD == 1
        m_out    TCCR1B, r_one   ; Start load counter
#endif
#endif
        st      -y, xh
        m_in     xh, SREG
        add     ms_count,  r_one
        adc     ms_count1, r_zero
#if CPU_LOAD == 1
LOAD_ADD:
        st      -y, xh
        st      -y, xl
        m_in     xl, TCNT1L
        m_in     xh, TCNT1H
        m_out    TCNT1H, r_zero
        m_out    TCNT1L, r_two

        add     loadreg0, xl
        adc     loadreg1, xh
        adc     loadreg2, r_zero

        tst     ms_count
        brne    LOAD_ADD_END
        sbr     FLAGS2, (1<<fLOAD)
LOAD_ADD_END:
        rjmp    MS_TIMER_ISR_EXIT_LOAD
#else
        rjmp    MS_TIMER_ISR_EXIT
#endif
;;; ***************************************************
RX0_ISR:
        m_in     xh, UDR0_
#if OPERATOR_UART == 0
#if CTRL_O_WARM_RESET == 1
        cpi     xh, 0xf
        brne    CTRL_O_WARM_RESET_1
        jmp     RESET_
CTRL_O_WARM_RESET_1:
#endif
#endif
        lds     xl, rbuf0_lv
        inc     xl
        sts     rbuf0_lv, xl

#if U0FC_TYPE == 1
        cpi     xl, RX0_OFF_FILL
        brmi    RX0_ISR_SKIP_XOFF
        rcall   XXOFF_TX0_1
#endif
#if U0FC_TYPE == 2
        cpi     xl, RX0_OFF_FILL
        brmi    RX0_ISR_SKIP_XOFF
        m_sbi    U0RTS_PORT, U0RTS_BIT
#endif
RX0_ISR_SKIP_XOFF:
        ldi     zl, lo8(rbuf0)
        ldi     zh, hi8(rbuf0)

        lds     xl, rbuf0_wr
        add     zl, xl
        adc     zh, r_zero
        st      z, xh
        inc     xl
        andi    xl, (RX0_BUF_SIZE-1)
        sts     rbuf0_wr, xl
        rjmp    FF_ISR_EXIT
;*******************************************************************
#if UARTS == 2
RX1_ISR:
        m_in     xh, UDR1
#if OPERATOR_UART == 1
#if CTRL_O_WARM_RESET == 1
        cpi     xh, 0xf
        brne    CTRL_O_WARM_RESET_1
        jmp     RESET_
CTRL_O_WARM_RESET_1:
#endif
#endif
        lds     xl, rbuf1_lv
        inc     xl
        sts     rbuf1_lv, xl

#if U1FC_TYPE == 1
        cpi     xl, RX0_OFF_FILL
        brmi    RX1_ISR_SKIP_XOFF
        rcall   XXOFF_TX1_1
#endif
#if U1FC_TYPE == 2
        cpi     xl, RX0_OFF_FILL
        brmi    RX1_ISR_SKIP_XOFF
        m_sbi    U1RTS_PORT, U1RTS_BIT
#endif
RX1_ISR_SKIP_XOFF:
        ldi     zl, lo8(rbuf1)
        ldi     zh, hi8(rbuf1)
        lds     xl, rbuf1_wr
        add     zl, xl
        adc     zh, r_zero
        st      z, xh
        inc     xl
        andi    xl, (RX1_BUF_SIZE-1)
        sts     rbuf1_wr, xl
        rjmp    FF_ISR_EXIT
#endif
#ifdef USBCON
#include  "usbcdc-xc8.asm" 
#endif

;;; *************************************
;;; EMPTY dictionary data
; *******************************************************************
.equ coldlitsize, 12
COLDLIT:
STARTV: .word      0
DPC:    .word      (KERNEL_END+0x100+PFLASH)
DPE:    .word      ehere
DPD:    .word      dpdata
LW:     fdw      lastword
STAT:   fdw      DOTSTATUS
;;; *************************************************
;;; WARM user area data
.equ warmlitsize, 28
WARMLIT:
        .word      0x0200                ; cse, state
        .word      utibbuf-4             ; S0
        .word      usbuf-1               ; R0
        fdw      OP_TX_
        fdw      OP_RX_
        fdw      OP_RXQ
        .word      BASE_DEFAULT          ; BASE
        .word      utibbuf               ; TIB
        fdw      OPERATOR_AREA           ; TASK
        .word      0                     ; ustatus & uflg
        .word      0                     ; source
        .word      0                     ; source
        .word      0                     ; TOIN
        .word      up0                   ; Task link

MEMQADDR_N:
        fdw     ROM_N
        fdw     EROM_N
        fdw     FRAM_N
BASEQV:
        fdw     DECIMAL
        fdw     HEX
        fdw     BIN

#if (FLASHEND == 0x1ffff)
        fdw     PAUSE_L
WDON_L:
        .byte    NFA|3
        .ascii   "wd+"
        .align  1
WDON:
        cli
        wdr
        lds     tosh, WDTCR
        ori     tosh, (1<<WDCE)|(1<<WDE)
        sts     WDTCR, tosh
        andi    tosl, 7
        ori     tosl, (1<<WDE)
        sts     WDTCR, tosl
        sei
        jmp     DROP

; WD- ( -- )    stop the watchdog
        fdw     WDON_L
WDOFF_L:
        .byte   NFA|3
        .ascii  "wd-"
        .align  1
WDOFF:
        cli
        wdr
#ifdef MCUCSR
        m_out     MCUCSR, r_zero
#else
        m_out     MCUSR, r_zero
#endif
        ldi     t0, (1<<WDCE)|(1<<WDE)
        sts     WDTCR, t0
        sts     WDTCR, r_zero
        sei
        ret

#else

; WD+ ( n -- )  n < 8 start watchdog timer
        fdw     PAUSE_L
WDON_L:
        .byte     NFA|3
        .ascii  "wd+"
        .align  1
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
        .byte     NFA|3
        .ascii  "wd-"
        .align  1
WDOFF:
        cli
        wdr
#ifdef MCUCSR
        sts     MCUCSR, r_zero
#else
        sts     MCUSR, r_zero
#endif
        ldi     t0, (1<<WDCE)|(1<<WDE)
        sts     WDTCSR, t0
        sts     WDTCSR, r_zero
        sei
        ret

#endif

; CWD ( -- )    kick the dog
        fdw     WDOFF_L
CWD_L:
        .byte     NFA|INLINE|3
        .ascii  "cwd"
        .align  1
CWD:
        wdr
        ret

;*********************************************************************
; EXIT --   Compile a return
;        variable link
        .word     0
EXIT_L:
        .byte     NFA|4
        .ascii    "exit"
        .align  1
EXIT:
        m_pop_t0
        pop     t0
        pop     t0
        ret

        fdw     IFLUSH_L
OPERATOR_L:
        .byte     NFA|8
        .ascii  "operator"
        .align  1
OPERATOR:
        call    DOCREATE
        fdw     OPERATOR_AREA
OPERATOR_AREA:
        .word     up0
        .word     0, ursize
        .word     ussize, utibsize

; idle
        fdw(EXIT_L)
IDLE_L:
        .byte     NFA|4
        .ascii    "idle"
        .align  1
IDLE:
        sbr     FLAGS2, (1<<fIDLE)
        ret
        
; busy
        fdw(IDLE_L)
BUSY_L:
        .byte     NFA|4
        .ascii  "busy"
        .align  1
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
        .byte     NFA|4
        .ascii  "mset"
        .align  1
MSET:
        movw    zl, tosl
        m_drop
        ld      t0, z+
        or      t0, tosl
        st      -z, t0
        m_drop
        ret
      
; : mclr  ( mask addr -- )
;  dup >r c@ swap invert and r> c!
; ;
        fdw     MSET_L
MCLR_L:
        .byte     NFA|4
        .ascii  "mclr"
        .align  1
MCLR_:
        movw    zl, tosl
        m_drop
        ld      t0, z+
        com     tosl
        and     t0, tosl
        st      -z, t0
        m_drop
        ret

;   LSHIFT      x1 a -- x2
        fdw     MCLR_L
LSHIFT_L:
        .byte     NFA|6
        .ascii  "lshift"
        .align  1
LSHIFT:
        movw    zl, tosl
        m_drop
LSHIFT1:
        sbiw    zl, 1
        brmi    LSHIFT2
        lsl     tosl
        rol     tosh
        rjmp    LSHIFT1
LSHIFT2:
        ret

;   RSHIFT      x1 a -- x2
        fdw     LSHIFT_L
RSHIFT_L:
        .byte     NFA|6
        .ascii  "rshift"
        .align  1
RSHIFT:
        movw    zl, tosl
        m_drop
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
        .byte     NFA|2
        .ascii  "n="
        .align  1
NEQUAL:
        movw    z, tosl
        subi    zh, hi8(PFLASH)
        ld      xl, y+
        ld      xh, y+
        m_lpm   tosh
        andi    tosh, 0xf
        ld      tosl, x+
        sub     tosl, tosh
        brne    NEQUAL3
        dec     tosh
NEQUAL2:
        m_lpm   tosl
        ld      t0, x+
        sub     tosl, t0
        brne    NEQUAL3
        dec     tosh
        brpl    NEQUAL2
NEQUAL3:
        mov     tosh, tosl
        ret

; SKIP   c-addr u c -- c-addr' u'
;                          skip matching chars
; u (count) must be smaller than 256
        fdw     NEQUAL_L
SKIP_L:
        .byte     NFA|4
        .ascii  "skip"
        .align  1
SKIP:
        ld      tosh, y+    ; count
        ld      t0, y+      ; dummy
        ld      xl, y+      ; c-addr
        ld      xh, y+      ; c-addr
SKIP0:
        cpi     tosh, 0
        breq    SKIP2
        ld      t0, x
        
        cpi     tosl, 32
        brne    SKIP3
        cpi     t0,  32
        brcs    SKIP1
SKIP3:
        cp      t0, tosl
        brne    SKIP2
SKIP1:
        adiw    xl, 1
        dec     tosh
        rjmp    SKIP0
SKIP2:
        st      -y, xh
        st      -y, xl
        mov     tosl, tosh
        clr     tosh
        ret


; SCAN   c-addr u c -- c-addr' u'
;                          find matching chars
        fdw     SKIP_L
SCAN_L:
        .byte     NFA|4
        .ascii  "scan"
        .align  1
SCAN:
        ;tosl               ; delimiter
        ld      tosh, y+    ; count
        ld      xl, y+      ; dummy
        ld      xl, y+      ; c-addr
        ld      xh, y+      ; c-addr
SCAN1:
        cpi     tosh, 0
        breq    SCAN4
        ld      t0, x
        
        cpi     tosl, 32
        brne    SCAN2
        cpi     t0, 32
        brcs    SCAN4
SCAN2:
        cp      tosl, t0
        breq    SCAN4
SCAN3:
        adiw    xl, 1
        dec     tosh
        rjmp    SCAN1
SCAN4:
        st      -y, xh
        st      -y, xl
        mov     tosl, tosh
        ldi     tosh, 0
        ret

; : mtst ( mask addr -- flag )
;   c@ and 
; ;
        fdw     SCAN_L
MTST_L:
        .byte     NFA|4
        .ascii  "mtst"
        .align  1
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
        .byte     NFA|3
        .ascii  "Fcy"
        .align  1
        rcall   DOCREATE
        .word     FREQ_OSC / 1000

;;; Check parameter stack pointer
        .byte     NFA|3
        .ascii  "sp?"
        .align  1
check_sp:
        rcall   SPFETCH
        call    R0_
        rcall   FETCH_A
        call    S0
        rcall   FETCH_A
        rcall   ONEPLUS
        rcall   WITHIN
        rcall   XSQUOTE
        .byte     3
        .ascii  "SP?"
        .align  1
        rcall   QABORT
        ret
;***************************************************
; EMIT  c --    output character to the emit vector
        fdw     FCY_L
EMIT_L:
        .byte     NFA|4
        .ascii  "emit"
        .align  1
EMIT:
        rcall   UEMIT_
        jmp     FEXECUTE

;***************************************************
; KEY   -- c    get char from UKEY vector
        fdw     EMIT_L
KEY_L:
        .byte     NFA|3
        .ascii  "key"
        .align  1
KEY:
        rcall   UKEY_
        jmp     FEXECUTE
;***************************************************
; KEY   -- c    get char from UKEY vector
        fdw     KEY_L
KEYQ_L:
        .byte     NFA|4
        .ascii  "key?"
        .align  1
KEYQ:
        rcall   UKEYQ_
        jmp     FEXECUTE

        fdw     KEYQ_L
EXECUTE_L:
        .byte     NFA|7
        .ascii  "execute"
        .align  1
EXECUTE:
        movw    zl, tosl
        sub_pflash_z
        m_drop
        lsr     zh
        ror     zl
        m_ijmp

        fdw     EXECUTE_L
FEXECUTE_L:
        .byte     NFA|3
        .ascii  "@ex"
        .align  1
FEXECUTE:
        rcall   FETCH_A
        jmp     EXECUTE

        fdw     FEXECUTE_L
VARIABLE_L:
        .byte     NFA|8
        .ascii  "variable"
        .align  1
VARIABLE_:
        rcall   HERE
        rcall   CELL
        rcall   ALLOT
        jmp     CONSTANT_

        fdw     VARIABLE_L
TWOVARIABLE_L:
        .byte     NFA|9
        .ascii  "2variable"
        .align  1
TWOVARIABLE_:
        rcall   HERE
        rcall   DOLIT
        .word     0x4
        rcall   ALLOT
        jmp     CONSTANT_

        fdw     TWOVARIABLE_L
CONSTANT_L:
        .byte     NFA|8
        .ascii  "constant"
        .align  1
CONSTANT_:
        call   COLON
        call    LITERAL
        jmp     SEMICOLON

        fdw     CONSTANT_L
TWOCONSTANT_L:
        .byte     NFA|9
        .ascii  "2constant"
        .align  1
TWOCONSTANT_:
        rcall   SWOP
        call   COLON
        call    LITERAL
        call    LITERAL
        jmp     SEMICOLON

; DOCREATE, code action of CREATE
; Fetch the next cell from program memory to the parameter stack
DOCREATE_L:
        .byte     NFA|3
        .ascii  "(c)"
        .align  1
DOCREATE:
        m_pop_zh
        pop     zh
        pop     zl
        rcall   FETCHLIT
        m_pop_zh
        pop     zh
        pop     zl
        m_ijmp

;;; Resolve the runtime action of the word created by using does>
DODOES_L:
        .byte     NFA|3
        .ascii  "(d)"
        .align  1
DODOES:
        m_pop_xh
        pop     xh
        pop     xl
        m_pop_zh
        pop     zh
        pop     zl
        rcall   FETCHLIT
        movw    z, x
        m_ijmp    ; (z)

FETCHLIT:
        m_dup
        lsl     zl
        rol     zh
        m_lpm    tosl
        m_lpm    tosh
        ret

        .byte     NFA|3
        .ascii  "(,)"
        .align  1
DOCOMMAXT:
        m_pop_t0
        pop     zh
        pop     zl
        rcall   FETCHLIT
        lsr     zh
        ror     zl
        push    zl
        push    zh
        m_push_t0
        rjmp     COMMAXT

;   SP@     -- addr         get parameter stack pointer
        fdw     TWOCONSTANT_L
SPFETCH_L:
        .byte     NFA|3
        .ascii  "sp@"
        .align  1
SPFETCH:
        movw    z, y
        m_dup
        movw    tosl, z
        ret

;   SP!     addr --         store stack pointer
        fdw     SPFETCH_L
SPSTORE_L:
        .byte     NFA|3
        .ascii  "sp!"
        .align  1
SPSTORE:
        movw    y, tosl
        ret

;   RPEMPTY     -- EMPTY THE RETURN STACK       
        .byte     NFA|3
        .ascii  "rp0"
        .align  1
RPEMPTY:
        m_pop_xh
        pop     xh
        pop     xl
        call    R0_
        rcall   FETCH_A
        out     spl, tosl
        out     sph, tosh
        m_drop
        movw    zl, xl
        m_ijmp

;   RP@ Fetch the return stack pointer        
        fdw     SPSTORE_L
RPFETCH_L:
        .byte     NFA|INLINE|COMPILE|3
        .ascii  "rp@"
        .align  1
RPFETCH:
        m_dup
        in      tosl, spl
        in      tosh, sph
        ret

;   ><  Swap bytes        
        fdw     RPFETCH_L
SWAPB_L:
        .byte     NFA|INLINE|2
        .ascii  "><"
        .align  1
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
        .byte     NFA|5
        .ascii  "flash"
        .align  1
ROM_:
        sts     cse, r_zero
        ret

; EEPROM -- sets the data section to EEPROM data memory
        fdw     FLASH_L
EEPROM_L:
EROM_N: 
        .byte     NFA|6
        .ascii    "eeprom"
        .align  1
EROM:
        sts     cse, r_two
        ret
        
; RAM -- sets the data section to RAM memory
        fdw     EEPROM_L
RAM_L:
FRAM_N: 
        .byte     NFA|3
        .ascii  "ram"
        .align  1
FRAM:
        ldi     t0, 4
        sts     cse, t0
        ret

; DP    -- a-addr          
; Fetched from EEPROM
        fdw     RAM_L
DP_L:
        .byte     NFA|2
        .ascii  "dp"
        .align  1
DP:
        rcall   IDP
        rcall   CSE_
        jmp     PLUS


;;; 
        .byte     NFA|3
        .ascii  "cse"
        .align  1
CSE_:
        m_dup
        lds     tosl, cse
        clr     tosh
        ret

; HERE    -- addr    get current data space ptr
;   DP @ ;
        fdw     DP_L
HERE_L:
        .byte     NFA|4
        .ascii  "here"
        .align  1
HERE:
        rcall   DP
        jmp     FETCH

; ,   x --             append cell to current data space
;   HERE ! CELL ALLOT ;
        fdw     HERE_L
COMMA_L:
        .byte     NFA|1
        .ascii  ","
        .align  1
COMMA:
        rcall   HERE
        rcall   STORE_A
        rcall   CELL
        jmp     ALLOT

; C,  c --             append char to current data space
;   HERE C! 1 ALLOT ;
        fdw     COMMA_L 
CCOMMA_L:
        .byte     NFA|2
        .ascii  "c,"
        .align  1
CCOMMA:
        rcall   HERE
        rcall   CSTORE_A
        rcall   ONE
        jmp     ALLOT


; CELL     -- n                 size of one cell
        fdw     CCOMMA_L
CELL_L:
        .byte     NFA|4
        .ascii  "cell"
        .align  1
CELL:
        m_dup
        ldi     tosl, 2
        ldi     tosh, 0
        ret

; ALIGN    --                         align DP
        fdw     CELL_L
ALIGN_L:
        .byte     NFA|5
        .ascii  "align"
        .align  1
ALIGN:
        rcall   HERE
        rcall   ALIGNED
        rcall   DP
        jmp     STORE

; ALIGNED  addr -- a-addr       align given addr
        fdw     ALIGN_L
ALIGNED_L:
        .byte     NFA|7
        .ascii  "aligned"
        .align  1
ALIGNED:
        adiw    tosl, 1
        cbr     tosl, 1
        ret

; CELL+    a-addr1 -- a-addr2      add cell size
;   2 + ;
        fdw     ALIGNED_L
CELLPLUS_L:
        .byte     NFA|INLINE|5
        .ascii  "cell+"
        .align  1
CELLPLUS:
        adiw    tosl, 2
        ret

; CELLS    n1 -- n2            cells->adrs units
        fdw     CELLPLUS_L
CELLS_L:
        .byte     NFA|INLINE|5
        .ascii  "cells"
        .align  1
CELLS:
        lsl     tosl
        rol     tosh
        ret

; CHAR+    c-addr1 -- c-addr2   add char size
        fdw     CELLS_L
CHARPLUS_L:
        .byte     NFA|INLINE|5
        .ascii  "char+"
        .align  1
CHARPLUS:
        adiw    tosl, 1
        ret

; CHARS    n1 -- n2            chars->adrs units
        fdw     CHARPLUS_L
CHARS_L:
        .byte     NFA|INLINE|5
        .ascii  "chars"
        .align  1
CHARS:  ret

        fdw     CHARS_L
COMMAXT_L:
        .byte     NFA|3
        .ascii  "cf,"
        .align  1
COMMAXT:
        rcall   DUP
        rcall   IHERE
        rcall   MINUS
        rcall   ABS_ 
        rcall   DOLIT
        .word     0xff0
        rcall   GREATER
        rcall   ZEROSENSE
        breq    STORECF1
STORECFF1: 
        rcall   DOLIT
        .word     0x940E  ; call jmp:0x940d
        call    ICOMMA
        sub_pflash_tos
        lsr     tosh
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
;       .word    link
;link   set     $
        .byte     NFA|2
        .ascii  "!:"
        .align  1
STORCOLON:
        rcall   DOLIT
        .word     0xfffa         ;  -6
        jmp     IALLOT


; 2@    a-addr -- x1 x2            fetch 2 cells
;   DUP @ SWAP CELL+ @ ;
;   the higher address will appear on top of stack
        fdw     COMMAXT_L
TWOFETCH_L:
        .byte     NFA|2
        .ascii  "2@"
        .align  1
TWOFETCH:
        rcall   DUP
        rcall   FETCH_A
        rcall   SWOP
        rcall   CELLPLUS
        jmp     FETCH_A

; 2!    x1 x2 a-addr --            store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the higher address
        fdw     TWOFETCH_L
TWOSTORE_L:
        .byte     NFA|2
        .ascii  "2!"
        .align  1
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
        .byte     NFA|5
        .ascii  "2drop"
        .align  1
TWODROP:
        rcall   DROP
        jmp     DROP

; 2DUP   x1 x2 -- x1 x2 x1 x2    dup top 2 cells
;   OVER OVER ;
        fdw     TWODROP_L
TWODUP_L:
        .byte     NFA|4
        .ascii  "2dup"
        .align  1
TWODUP:
        rcall   OVER
        jmp     OVER

; 2SWAP   x1 x2 x3 x4 -- x3 x4 x1 x2    dup top 2 cells
        fdw     TWODUP_L
TWOSWAP_L:
        .byte     NFA|5
        .ascii  "2swap"
        .align  1
TWOSWAP:
        rcall   ROT
        rcall   TOR
        rcall   ROT
        rcall   RFROM
        ret

	fdw     TWOSWAP_L
TWOOVER_L:
        .byte     NFA|5
        .ascii  "2over"
        .align  1
TWOOVER:
	m_dup
        ldd     tosl, y+6
        ldd     tosh, y+7
        m_dup
        ldd     tosl, y+6
        ldd     tosh, y+7
	ret

; INPUT/OUTPUT ==================================

; SPACE   --                      output a space
;   BL EMIT ;
        fdw     TWOOVER_L
SPACE_L:
        .byte     NFA|5
        .ascii  "space"
        .align  1
SPACE_:  
        rcall   BL
        jmp     EMIT

; SPACES   n --                  output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
        fdw     SPACE_L
SPACES_L:
        .byte     NFA|6
        .ascii  "spaces"
        .align  1
SPACES:
SPCS1:
        sbiw    tosl, 0
        breq    SPCS2
        rcall   SPACE_
        rcall   ONEMINUS
        rjmp    SPCS1
SPCS2:  jmp     DROP


; umin     u1 u2 -- u           unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
        fdw     SPACES_L
UMIN_L:
        .byte     NFA|4
        .ascii  "umin"
        .align  1
UMIN:
        rcall   TWODUP
        rcall   UGREATER
        rjmp    MINMAX

; umax    u1 u2 -- u            unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
        fdw     UMIN_L
UMAX_L:
        .byte     NFA|4
        .ascii  "umax"
        .align  1
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
        .byte     NFA|INLINE4|1
        .ascii  "1"
        .align  1
ONE:
        m_dup
        ldi     tosl, 1
        ldi     tosh, 0
        ret

; ACCEPT  c-addr +n -- +n'  get line from terminal
        fdw     ONE_L
ACCEPT_L:
        .byte     NFA|6
        .ascii  "accept"
        .align  1
ACCEPT:
        rcall   OVER
        rcall   PLUS
        rcall   TOR
        rcall   DUP
ACC1:
        rcall   KEY
        cpi     tosl, CR_
        breq    ACC_CR
        cpi     tosl, LF_
        breq    ACC_LF
        cpi     tosl, BS_
        breq    ACC_BS_DEL
        cpi     tosl, 127
        breq    ACC_BS_DEL
        rjmp    ACC3
ACC_CR:
        rcall   FCR
        rcall   CSTORE_A
        rjmp    ACC6
ACC_LF:
        rcall   DROP

        rcall   FCR
        rcall   CFETCH_A
        rcall   ZEROSENSE
        breq    ACC6
        rcall   FALSE_
        rcall   FCR
        rcall   CSTORE_A
        rjmp    ACC1
ACC_BS_DEL:
        rcall   FALSE_
        rcall   FCR
        rcall   CSTORE_A
        rcall   DROP
        rcall   TWODUP
        rcall   EQUAL
        rcall   ZEROSENSE
        brne    ACC1
        rcall   ONEMINUS
        rcall   XSQUOTE
        .byte   3,8,0x20,8
        rcall   TYPE
        rjmp    ACC1
ACC3:
        rcall   DUP
        rcall   EMIT
        rcall   OVER
        rcall   CSTORE_A
        rcall   ONEPLUS
        rcall   RFETCH
        rcall   OVER
        rcall   EQUAL
        rcall   ZEROSENSE
        breq    ACC1
ACC6:
        pop     t0
        pop     t0

        rcall   SWOP
        jmp     MINUS

        .byte     NFA|3
        .ascii  "fcr"
        .align  1
FCR:
        rcall   DOUSER
        .word     uflg


; TYPE    c-addr u --   type line to terminal u < $100
; : type for c@+ emit next drop ;

        fdw      ACCEPT_L
TYPE_L:
        .byte     NFA|4
        .ascii  "type"
        .align  1
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
        fdw     TYPE_L
XSQUOTE_L:
        .byte      NFA|3
        .ascii  "(s"
        .byte   0x22
        .align  1
XSQUOTE:
        m_pop_zh
        pop     zh
        pop     zl
        lsl     zl
        rol     zh
        m_lpm    t0
        m_dup
        movw    tosl, zl
        add_pflash_tos
        m_dup
        mov     tosl, t0
        clr     tosh
        add     zl, t0
        adc     zh, tosh
        adiw    zl, 1
        lsr     zh
        ror     zl
        m_ijmp

        fdw     XSQUOTE_L
SQUOTE_L:
        .byte      NFA|IMMED|COMPILE|2
        .ascii  "s"
        .byte   0x22
        .align  1
SQUOTE:
        rcall   DOCOMMAXT
        fdw     XSQUOTE
        rcall   ROM_
        rcall   CQUOTE
        jmp     FRAM

        fdw     SQUOTE_L
CQUOTE_L:
        .byte     NFA|2
        .ascii  ","
        .byte   0x22
        .align  1
CQUOTE: 
        rcall   DOLIT
        .word     0x22
        rcall   PARSE
        rcall   HERE
        rcall   OVER
        rcall   ONEPLUS
        rcall   ALIGNED
        rcall   ALLOT
        jmp     PLACE


        fdw     CQUOTE_L
DOTQUOTE_L:
        .byte      NFA|IMMED|COMPILE|2
        .ascii  "."
        .byte   0x22
        .align  1
DOTQUOTE:
        rcall   SQUOTE
        rcall   DOCOMMAXT
        fdw     TYPE
        ret

        fdw     DOTQUOTE_L
ALLOT_L:
        .byte     NFA|5
        .ascii  "allot"
        .align  1
ALLOT:
        rcall   DP
        jmp     PLUSSTORE

        fdw     ALLOT_L
DROP_L:
        .byte     NFA|INLINE|4
        .ascii  "drop"
        .align  1
DROP:
        m_drop
        ret

        fdw     DROP_L
SWOP_L:
        .byte     NFA|INLINE5|4
        .ascii  "swap"
        .align  1
SWOP:
        ld      t0, y+
        ld      t1, y+
        m_dup
        movw    tosl, t0
        ret

        fdw     SWOP_L
OVER_L:
        .byte     NFA|INLINE4|4
        .ascii  "over"
        .align  1
OVER:
        m_dup
        ldd     tosl, y+2
        ldd     tosh, y+3
        ret

        fdw     OVER_L
ROT_L:
        .byte     NFA|3
        .ascii  "rot"
        .align  1
ROT:
        rcall   TOR
        rcall   SWOP
        rcall   RFROM
        jmp     SWOP

        fdw     ROT_L
TOR_L:
        .byte     NFA|COMPILE|2
        .ascii  ">r"
        .align  1
TOR:
        m_pop_zh
        pop     zh
        pop     zl
        push    tosl
        push    tosh
        m_drop
        m_ijmp

        fdw     TOR_L
RFROM_L:
        .byte     NFA|COMPILE|2
        .ascii  "r>"
        .align  1
RFROM:
        m_pop_zh
        pop     zh
        pop     zl
        m_dup
        pop     tosh
        pop     tosl
        m_ijmp

        fdw     RFROM_L
RFETCH_L:
        .byte     NFA|COMPILE|2
        .ascii  "r@"
        .align  1
RFETCH:
        m_pop_zh
        pop     zh
        pop     zl
        m_dup
        pop     tosh
        pop     tosl
        push    tosl
        push    tosh
        m_ijmp

;   ABS     n   --- n1      absolute value of n
        fdw     DUP_L
ABS_L:
        .byte     NFA|3
        .ascii  "abs"
        .align  1
ABS_:
        rcall   DUP
        jmp     QNEGATE

        fdw     ABS_L
PLUS_L:
        .byte     NFA|INLINE4|1
        .ascii  "+"
        .align  1
PLUS:
        ld      t0, Y+        
        ld      t1, Y+
        add     tosl, t0
        adc     tosh, t1
        ret

; m+  ( d n -- d1 )
        fdw     PLUS_L
MPLUS_L:
        .byte     NFA|2
        .ascii  "m+"
        .align  1
MPLUS:
        call   STOD
        jmp     DPLUS

        fdw     MPLUS_L
MINUS_L:
        .byte     NFA|INLINE5|1
        .ascii  "-"
        .align  1
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
        .byte     NFA|INLINE4|3
        .ascii  "and"
        .align  1
AND_:
        ld      t0, Y+
        ld      t1, Y+
        and     tosl, t0
        and     tosh, t1
        ret

        fdw     AND_L
OR_L:
        .byte     NFA|INLINE4|2
        .ascii  "or"
        .align  1
OR_:
        ld      t0, Y+
        ld      t1, Y+
        or      tosl, t0
        or      tosh, t1
        ret

        fdw     OR_L
XOR_L:
        .byte     NFA|INLINE4|3
        .ascii  "xor"
        .align  1
XOR_:
        ld      t0, Y+
        ld      t1, Y+
        eor     tosl, t0
        eor     tosh, t1
        ret

        fdw     XOR_L
INVERT_L:
        .byte     NFA|INLINE|6
        .ascii  "invert"
        .align  1
INVERT:
        com     tosl
        com     tosh
        ret

        fdw     INVERT_L
NEGATE_L:
        .byte     NFA|6
        .ascii  "negate"
        .align  1
NEGATE:
        com     tosl
        com     tosh
        adiw    tosl, 1
        ret

        fdw     NEGATE_L
ONEPLUS_L:
        .byte     NFA|INLINE|2
        .ascii  "1+"
        .align  1
ONEPLUS:
        adiw    tosl, 1
        ret

        fdw     ONEPLUS_L
ONEMINUS_L:
        .byte     NFA|INLINE|2
        .ascii  "1-"
        .align  1
ONEMINUS:
        sbiw    tosl, 1
        ret

        fdw     ONEMINUS_L
TWOPLUS_L:
        .byte     NFA|INLINE|2
        .ascii  "2+"
        .align  1
TWOPLUS:
        adiw    tosl, 2
        ret

        fdw     TWOPLUS_L
TOBODY_L:
        .byte     NFA|5
        .ascii  ">body"
        .align  1
TOBODY:
        adiw    tosl, 4
        jmp     FETCH

        fdw     TOBODY_L
TWOSTAR_L:
        .byte     NFA|INLINE|2
        .ascii   "2*"
        .align  1
TWOSTAR:
        lsl     tosl
        rol     tosh
        ret

        fdw     TWOSTAR_L
TWOSLASH_L:
        .byte     NFA|INLINE|2
        .ascii  "2/"
        .align  1
TWOSLASH:
        asr     tosh
        ror     tosl
        ret

        fdw     TWOSLASH_L
PLUSSTORE_L:
        .byte     NFA|2
        .ascii  "+!"
        .align  1
PLUSSTORE:
        rcall   SWOP
        rcall   OVER
        rcall   FETCH_A
        rcall   PLUS
        rcall   SWOP
        jmp     STORE

        fdw     PLUSSTORE_L
WITHIN_L:
        .byte     NFA|6
        .ascii  "within"
        .align  1
WITHIN:
        rcall   OVER
        rcall   MINUS
        rcall   TOR
        rcall   MINUS
        rcall   RFROM
        jmp     ULESS

        fdw     WITHIN_L
NOTEQUAL_L:
        .byte     NFA|2
        .ascii  "<>"
        .align  1
NOTEQUAL:
        rcall   EQUAL
        jmp     ZEROEQUAL

        fdw     ZEROLESS_L
EQUAL_L:
        .byte     NFA|1
        .ascii  "="
        .align  1
EQUAL:
        rcall   MINUS
        jmp     ZEROEQUAL


        fdw     EQUAL_L
LESS_L:
        .byte     NFA|1
        .ascii  "<"
        .align  1
LESS:
        rcall   MINUS       ; Status flags are valid after MINUS
        ldi     tosl, 0xff
        ldi     tosh, 0xff
        brlt    LESS_1
        com     tosl
        com     tosh
LESS_1:
        ret

        fdw     LESS_L
GREATER_L:
        .byte     NFA|1
        .ascii  ">"
        .align  1
GREATER:
        rcall   SWOP
        jmp     LESS

        fdw     GREATER_L
ULESS_L:
        .byte     NFA|2
        .ascii  "u<"
        .align  1
ULESS:
        rcall   MINUS       ; Carry is valid after MINUS
        sbc     tosl, tosl
        sbc     tosh, tosh
        ret

        fdw     ULESS_L
UGREATER_L:
        .byte     NFA|2
        .ascii  "u>"
        .align  1
UGREATER:
        rcall   SWOP
        jmp     ULESS

        fdw     UGREATER_L
STORE_P_L:
        .byte     NFA|2
        .ascii  "!p"
        .align  1
STORE_P:
        movw    pl, tosl
        m_drop
        ret

        fdw     STORE_P_L
STORE_P_TO_R_L:
        .byte     NFA|COMPILE|4
        .ascii  "!p>r"
        .align  1
STORE_P_TO_R:
        m_pop_zh
        pop     zh
        pop     zl
        push    pl
        push    ph
        movw    pl, tosl
        m_drop
        m_ijmp

        fdw     STORE_P_TO_R_L
R_TO_P_L:
        .byte     NFA|COMPILE|3
        .ascii  "r>p"
        .align  1
R_TO_P:
        m_pop_zh
        pop     zh
        pop     zl
        pop     ph
        pop     pl
        m_ijmp

        fdw     R_TO_P_L
PFETCH_L:
        .byte     NFA|2
        .ascii  "p@"
        .align  1
PFETCH:
        m_dup
        movw    tosl, pl
        jmp     FETCH

        fdw     PFETCH_L
PSTORE_L:
        .byte     NFA|2
        .ascii  "p!"
        .align  1
PSTORE:
        m_dup
        movw    tosl, pl
        jmp     STORE

        fdw     PSTORE_L
PCSTORE_L:
        .byte     NFA|3
        .ascii  "pc!"
        .align  1
PCSTORE:
        m_dup
        movw    tosl, pl
        jmp     CSTORE

        fdw     PCSTORE_L
PPLUS_L:
        .byte     NFA|INLINE|2
        .ascii  "p+"
        .align  1
PPLUS:
        add     pl, r_one
        adc     ph, r_zero
        ret   

        fdw     PPLUS_L
PNPLUS_L:
        .byte     NFA|3
        .ascii  "p++"
        .align  1
PNPLUS:
        add     pl, tosl
        adc     ph, tosh
        m_drop
        ret

        fdw     PNPLUS_L
UEMIT_L:
        .byte     NFA|5
        .ascii  "'emit"
        .align  1
UEMIT_:
        rcall   DOUSER
        .word     uemit
        
        fdw     UEMIT_L
UKEY_L:
        .byte     NFA|4
        .ascii  "'key"
        .align  1
UKEY_:
        rcall   DOUSER
        .word     ukey

        fdw     UKEY_L
UKEYQ_L:
        .byte     NFA|5
        .ascii  "'key?"
        .align  1
UKEYQ_:
        rcall   DOUSER
        .word     ukeyq

        .byte     NFA|3
        .ascii  "?0="
        .align  1
ZEROSENSE:
        sbiw    tosl, 0
        m_drop
        ret

        .byte     NFA|3
        .ascii  "d0="
        .align  1
DUPZEROSENSE:
        sbiw    tosl, 0
        ret

        fdw     UKEYQ_L
UMSTAR_L:
        .byte     NFA|3
        .ascii  "um*"
        .align  1
UMSTAR:
        jmp     umstar0

        fdw     UMSTAR_L
UMSLASHMOD_L:
        .byte     NFA|6
        .ascii  "um/mod"
        .align  1
UMSLASHMOD:
        jmp     umslashmod0


        fdw     UMSLASHMOD_L
USLASHMOD_L:
        .byte     NFA|5
        .ascii  "u/mod"
        .align  1
USLASHMOD:
        rcall   FALSE_
        rcall   SWOP
        jmp     umslashmod0

        fdw     USLASHMOD_L
STAR_L:
        .byte     NFA|1
        .ascii  "*"
        .align  1
STAR: 
        rcall   UMSTAR
        jmp     DROP

        fdw     STAR_L
USLASH_L:
        .byte     NFA|2
        .ascii  "u/"
        .align  1
USLASH:
        rcall   USLASHMOD
        jmp     NIP

        fdw     USLASH_L
USSMOD_L:
        .byte     NFA|6
        .ascii  "u*/mod"
        .align  1
USSMOD:
        rcall   TOR
        rcall   UMSTAR
        rcall   RFROM
        jmp     UMSLASHMOD


        fdw     USSMOD_L
SLASH_L:
        .byte     NFA|1
        .ascii  "/"
        .align  1
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
        .byte     NFA|INLINE|3
        .ascii  "nip"
        .align  1
NIP:
        ld      t0, y+
        ld      t0, y+
        ret
    
        fdw     NIP_L
TUCK_L:
        .byte     NFA|4
        .ascii  "tuck"
        .align  1
TUCK:
        rcall   SWOP
        jmp     OVER

        fdw     TUCK_L
QNEGATE_L:
        .byte     NFA|7
        .ascii  "?negate"
        .align  1
QNEGATE:
        rcall   ZEROLESS
        rcall   ZEROSENSE
        breq    QNEGATE1
        rcall   NEGATE
QNEGATE1:
        ret

        fdw     QNEGATE_L
MAX_L:
        .byte     NFA|3
        .ascii  "max"
        .align  1
MAX:    rcall   TWODUP
        rcall   LESS
        rjmp    MINMAX

        fdw     MAX_L
MIN_L:
        .byte     NFA|3
        .ascii  "min"
        .align  1
MIN:    rcall   TWODUP
        rcall   GREATER
        rjmp    MINMAX

        .byte     NFA|2
        .ascii  "c@"
        .align  1
CFETCH_A:       
        jmp     CFETCH

        .byte     NFA|2
        .ascii  "c!"
        .align  1
CSTORE_A:       
        jmp     CSTORE

        fdw     MIN_L
UPTR_L:
        .byte     NFA|2
        .ascii  "up"
        .align  1
UPTR:   rcall   DOCREATE
        .word     2 ; upl

        fdw     UPTR_L
HOLD_L:
        .byte     NFA|4
        .ascii  "hold"
        .align  1
HOLD:   rcall   TRUE_
        rcall   HP
        rcall   PLUSSTORE
        rcall   HP
        rcall   FETCH_A
        jmp     CSTORE

; <#    --              begin numeric conversion
;   HB HP ! ;          (initialize Hold Pointer)
        fdw     HOLD_L
LESSNUM_L:
        .byte     NFA|2
        .ascii  "<#"
        .align  1
LESSNUM: 
        rcall   HB
        rcall   HP
        jmp     STORE

; digit   n -- c            convert to 0..9a..z
        fdw     LESSNUM_L
TODIGIT_L:
        .byte     NFA|5
        .ascii  "digit"
        .align  1
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
        .byte     NFA|1
        .ascii  "#"
        .align  1
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
        .byte     NFA|2
        .ascii  "#s"
        .align  1
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
        .byte     NFA|2
        .ascii  "#>"
        .align  1
NUMGREATER:
        rcall   TWODROP
        rcall   HP
        rcall   FETCH_A
        rcall   HB
        rcall   OVER
        jmp     MINUS

; SIGN  n --               add minus sign if n<0
;   0< IF 2D HOLD THEN ; 
        fdw     NUMGREATER_L
SIGN_L:
        .byte     NFA|4
        .ascii  "sign"
        .align  1
SIGN:
        cpi     tosh, 0
        brpl    SIGN1
        rcall   DOLIT
        .word     0x2D
        rcall   HOLD
SIGN1:
        jmp     DROP

; U.    u --                  display u unsigned
;   <# 0 #S #> TYPE SPACE ;
        fdw     SIGN_L
UDOT_L:
        .byte     NFA|2
        .ascii  "u."
        .align  1
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
        .byte     NFA|3
        .ascii  "u.r"
        .align  1
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
        .byte     NFA|1
        .ascii  "."
        .align  1
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
        .byte     NFA|7
        .ascii  "decimal"
        .align  1
DECIMAL: 
        rcall   TEN
        rcall   BASE
        jmp     STORE

; HEX     --              set number base to hex
;   #16 BASE ! ;
        Fdw     DECIMAL_L
HEX_L:
        .byte     NFA|3
        .ascii  "hex"
        .align  1
HEX:
        rcall   DOLIT
        .word     16
        rcall   BASE
        jmp     STORE

; BIN     --              set number base to binary
;   #2 BASE ! ;
        Fdw     HEX_L
BIN_L:
        .byte     NFA|3
        .ascii  "bin"
        .align  1
BIN:    rcall   CELL
        rcall   BASE
        jmp     STORE

; RSAVE   -- a-addr     Saved return stack pointer
        fdw     BIN_L
RSAVE_L:
        .byte     NFA|5
        .ascii  "rsave"
        .align  1
RSAVE_: rcall   DOUSER
        .word     ursave


; ULINK   -- a-addr     link to next task
        fdw     RSAVE_L
ULINK_L:
        .byte     NFA|5
        .ascii  "ulink"
        .align  1
ULINK_: rcall   DOUSER
        .word     ulink


; TASK       -- a-addr              TASK pointer
        fdw     ULINK_L
TASK_L:
        .byte     NFA|4
        .ascii  "task"
        .align  1
TASK:   rcall   DOUSER
        .word     utask


; HP       -- a-addr                HOLD pointer
        fdw     TASK_L
HP_L:
        .byte     NFA|2
        .ascii  "hp"
        .align  1
HP:     rcall   DOUSER
        .word     uhp

; HB     -- a-addr        Number formatting buffer
        fdw     HP_L
HB_L:
        .byte     NFA|2
        .ascii  "hb"
        .align  1
HB:
        rcall   TIB
        rcall   TIBSIZE
        jmp     PLUS

; BASE    -- a-addr       holds conversion radix
        fdw     HB_L
BASE_L:
        .byte     NFA|4
        .ascii  "base"
        .align  1
BASE:
        rcall   DOUSER
        .word     ubase

; USER   n --
        fdw     BASE_L
USER_L:
        .byte     NFA|4
        .ascii  "user"
        .align  1
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


        fdw     NOTEQUAL_L
ZEROEQUAL_L:
        .byte     NFA|2
        .ascii  "0="
        .align  1
ZEROEQUAL:
        sbiw    tosl, 1
        sbc     tosl, tosl
        sbc     tosh, tosh
        ret

; SOURCE   -- adr n         current input buffer
;   'SOURCE 2@ ;        length is at hi8er adrs
        fdw     USER_L
SOURCE_L:
        .byte     NFA|6
        .ascii  "source"
        .align  1
SOURCE:
        rcall   TICKSOURCE
        jmp     TWOFETCH


; /STRING  a u n -- a+n u-n          trim string
;   swap over - >r + r>
        fdw      SOURCE_L
SLASHSTRING_L:
        .byte     NFA|7
        .ascii  "/string"
        .align  1
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
        .byte     NFA|IMMED|1,0x5c
        .align  1
BSLASH:
        rcall   SOURCE
        rcall   TOIN
        rcall   STORE_A
        sbr     FLAGS1, (1<<noclear)  ; dont clear flags in case of backslash
        jmp     DROP

; PARSE  char -- c-addr u
        fdw     BSLASH_L
PARSE_L:
        .byte     NFA|5
        .ascii  "parse"
        .align  1
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
        sbiw    tosl, 0
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
        .byte     NFA|4
        .ascii  "word"
        .align  1
WORD:
        rcall   PARSE           ; c-addr wlen
        rcall   PAD
        rcall   PLACE
        jmp     PAD

; CMOVE  src dst u --  copy u bytes from src to dst
; cmove swap !p>r for c@+ pc! p+ next r>p drop ;
        fdw     WORD_L
CMOVE_L:
        .byte     NFA|5
        .ascii  "cmove"
        .align  1
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
        .byte     NFA|5
        .ascii  "place"
        .align  1
PLACE: 
        rcall   TWODUP
        rcall   CSTORE_A
        rcall   CHARPLUS
        rcall   SWOP
        jmp     CMOVE

; :     c@+ ( addr -- addr+1 n ) dup 1+ swap c@ ;
        fdw     PLACE_L
CFETCHPP_L:
        .byte     NFA|3
        .ascii  "c@+"
        .align  1
CFETCHPP:
        m_dup
        adiw    tosl, 1
        rcall   SWOP
        jmp     CFETCH

; :     @+ ( addr -- addr+2 n ) dup 2+ swap @ ;
        fdw     CFETCHPP_L
FETCHPP_L:
        .byte     NFA|2
        .ascii  "@+"
        .align  1
FETCHPP:
        m_dup
        adiw    tosl, 2
        rcall   SWOP
        jmp     FETCH

        .byte     NFA|1
        .ascii  "!"
STORE_A:        
        jmp     STORE

; N>C   nfa -- cfa    name adr -> code field
        fdw    FETCHPP_L
NTOC_L:
        .byte     NFA|3
        .ascii  "n>c"
        .align  1
NFATOCFA:
        rcall   CFETCHPP
        andi    tosl, 0x0f
        rcall   PLUS
        jmp     ALIGNED

; C>N   cfa -- nfa    code field addr -> name field addr
        fdw    NTOC_L
CTON_L:
        .byte     NFA|3
        .ascii  "c>n"
        .align  1
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
        .byte     NFA|3
        .ascii  "(f)"
        .align  1
findi:
findi1:
        movw    al, tosl    ; c-addr nfa
        movw    z, tosl
        subi    zh, hi8(PFLASH)
        ldd     xl, y+0
        ldd     xh, y+1
        m_lpm   tosh
        andi    tosh, 0xf
        ld      tosl, x+
        sub     tosl, tosh
        brne    FEQUAL3
        dec     tosh
FEQUAL2:
        m_lpm   tosl
        ld      t0, x+
        sub     tosl, t0
        brne    FEQUAL3
        dec     tosh
        brpl    FEQUAL2
FEQUAL3:
        cpi     tosl, 0
        movw    tosl, al    ; c-addr nfa
        breq    findi4      ; ( c-addr 0 )
        sbiw    tosl, 2     ; c-addr lfa
        call    FETCH      ; c-addr nfa
findi2:
        sbiw    tosl, 0     ; c-addr nfa
        brne    findi1
        rjmp    findi3
findi4:
        std     y+0, tosl
        std     y+1, tosh    ; nfa nfa
        rcall   NFATOCFA     ; nfa xt
        rcall   SWOP
        rcall   IMMEDQ       ; xt flag
        rcall   ZEROEQUAL
        ori     tosl, 1
findi3: 
        ret

; IMMED?    nfa -- f        fetch immediate flag
        fdw     BRACFIND_L
IMMEDQ_L:
        .byte     NFA|6
        .ascii  "immed?"
        .align  1
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
        .byte     NFA|4
        .ascii  "find"
        .align  1
FIND:   
        rcall   DOLIT
        fdw     kernellink
        rcall   findi
        sbiw    tosl, 0
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
        .byte     NFA|6
        .ascii  "digit?"
        .align  1
DIGITQ:
                                ; 1 = 0x31    a = 0x61
        cpi     tosl, 0x3a
        brlt    DIGITQ1
        cpi     tosl, 0x61
        brmi    DIGITQ2
        sbiw    tosl, 0x27
DIGITQ1:        
        sbiw    tosl, 0x30      ; 1
        brpl    DIGITQ3
DIGITQ2:
        rjmp    FALSE_
DIGITQ3:
        rcall   DUP             ; 1 1
        rcall   BASE            ; 1 1 base
        rcall   FETCH_A         ; 1 1 10
        jmp     LESS            ; 1 ffff

; SIGN?   adr n -- adr' n' f   get optional sign
; + leaves $0000 flag
; - leaves $0002 flag
        fdw     DIGITQ_L
SIGNQ_L:
        .byte     NFA|5
        .ascii  "sign?"
        .align  1
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
        .byte     NFA|3
        .ascii  "ud*"
        .align  1
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
        .byte     NFA|6
        .ascii  "ud/mod"
        .align  1
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
        .byte     NFA|7
        .ascii  ">number"
        .align  1
TONUMBER:
        ldi     al, 1
TONUM1:
        sbiw    tosl, 0          ; ud.l ud.h adr u
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
        .byte     NFA|7
        .ascii  "number?"
        .align  1
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


        .byte     NFA|4
        .ascii  "swap"
        .align  1
SWOP_A:
        jmp     SWOP

; TI#  -- n                      size of TIB
; : ti# task @ 8 + @ ;
        fdw     NUMBERQ_L
TIBSIZE_L:
        .byte     NFA|3
        .ascii  "ti#"
        .align  1
TIBSIZE:
        rcall   TASK
        rcall   FETCH_A
        adiw    tosl, 8
        jmp     FETCH

; TIB     -- a-addr        Terminal Input Buffer
        fdw     TIBSIZE_L
TIB_L:
        .byte     NFA|3
        .ascii  "tib"
        .align  1
TIB:
        rcall   TIU
        jmp     FETCH
        
; TIU     -- a-addr        Terminal Input Buffer user variable 
        fdw     TIB_L
TIU_L:
        .byte     NFA|3
        .ascii  "tiu"
        .align  1
TIU:
        rcall   DOUSER
        .word     utib       ; pointer to Terminal input buffer

; >IN     -- a-addr        holds offset into TIB
; In RAM
        fdw     TIU_L
TOIN_L:
        .byte     NFA|3
        .ascii  ">in"
        .align  1
TOIN:
        rcall   DOUSER
        .word   utoin

; 'SOURCE  -- a-addr        two cells: len, adrs
; In RAM ?
        fdw     TOIN_L
TICKSOURCE_L:
        .byte     NFA|7
        .ascii  "'source"
        .align  1
TICKSOURCE:
        rcall   DOUSER
        .word     usource       ; two cells !!!!!!

WORDQ:
        rcall   DUP
        m_pop_t0
        pop     zh
        pop     zl
        rcall   FETCHLIT
        lsr     zh
        ror     zl
        rcall   EQUAL
        rcall   ZEROSENSE
        m_ijmp

;  INTERPRET  c-addr u --    interpret given buffer
        fdw     TICKSOURCE_L
INTERPRET_L:
        .byte     NFA|9
        .ascii  "interpret"
        .align  1
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
        sbiw    tosl, 0         ; 0 = not found, -1 = normal, 1 = immediate
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
        .byte     12
        .ascii  "COMPILE ONLY"
        .align  1
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
        sbiw    tosl, 0
        breq    IUNKNOWN
        rcall   STATE_
        rcall   ZEROSENSE
        breq    INUMBER1
        mov     t0, tosl
        m_drop
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

        .byte     NFA|1
        .ascii  "@"
        .align  1
FETCH_A:        
        jmp     FETCH

;;;    bitmask -- 
        fdw     INTERPRET_L
SHB_L:
        .byte     NFA|3
        .ascii  "shb"     ; Set header bit
        .align  1
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
        .byte     NFA|9
        .ascii  "immediate" ; 
        .align  1
IMMEDIATE:
        rcall   DOLIT
        .word     IMMED
        jmp     SHB

;***************************************************************
        fdw     IMMEDIATE_L
INLINED_L:
        .byte     NFA|7
        .ascii  "inlined" ; 
        .align  1
INLINED:
        rcall   DOLIT
        .word     INLINE
        jmp     SHB

;; .st ( -- ) output a string with current data section and current base info
;;; : .st base @ dup decimal <#  [char] , hold #s  [char] < hold #> type 
;;;     <# [char] > hold cse @ #s #> type base ! ;
        fdw     INLINED_L
DOTSTATUS_L:
        .byte     NFA|3
        .ascii  ".st"
        .align  1
DOTSTATUS:
        rcall   DOLIT
        .word     '<'
        rcall   EMIT
        call    DOTBASE
        rcall   EMIT
        rcall   DOLIT
        .word     ','
        rcall   EMIT
        call    MEMQ
        rcall   TYPE
        rcall   DOLIT
        .word     '>'
        rcall   EMIT
        jmp     DOTS

        .byte     NFA|2
        .ascii  ">r"
        .align  1
TOR_A:  jmp     TOR


;;; TEN ( -- n ) Leave decimal 10 on the stack
;        .byte     NFA|1,"a"
TEN:
        rcall   DOCREATE
        .word     10

; dp> ( -- ) Copy ini, dps and latest from eeprom to ram
;        .word     link
; link    set     $
        .byte     NFA|3
        .ascii  "dp>"
        .align  1
DP_TO_RAM:
        rcall   DOLIT
        .word     dp_start
        rcall   INI
        rcall   TEN
        jmp     CMOVE

; >dp ( -- ) Copy only changed turnkey, dp's and latest from ram to eeprom
;        .word     link
; link    set     $
        .byte     NFA|3
        .ascii  ">dp"
        .align  1
DP_TO_EEPROM:
        rcall   DOLIT
        .word   dp_start
        rcall   STORE_P_TO_R
        rcall   INI
        rcall   DOLIT
        .word   9
        rcall   TOR
DP_TO_EEPROM_0: 
        rcall   CFETCHPP
        rcall   DUP
        rcall   PCFETCH
        rcall   NOTEQUAL
        rcall   ZEROSENSE
        breq    DP_TO_EEPROM_1
        rcall   PCSTORE
        rjmp    DP_TO_EEPROM_2
DP_TO_EEPROM_1:
        rcall   DROP
DP_TO_EEPROM_2:
        rcall   PPLUS
DP_TO_EEPROM_3:
        rcall   XNEXT
        brcc    DP_TO_EEPROM_0
        pop     t1
        pop     t0
        rcall   R_TO_P
        jmp     DROP

        fdw     DOTSTATUS_L
FALSE_L:
        .byte     NFA|5
        .ascii  "false"
        .align  1
FALSE_:                     ; TOS is 0000 (FALSE)
        m_dup
        clr     tosl
        clr     tosh
        ret

        fdw     FALSE_L
TRUE_L:
        .byte     NFA|4
        .ascii  "true"
        .align  1
TRUE_:                      ; TOS is ffff (TRUE)
        m_dup
        ser     tosl
        ser     tosh
        ret

; QUIT     --    R: i*x --    interpret from kbd
        fdw     TRUE_L
QUIT_L:
        .byte     NFA|4
        .ascii  "quit"
        .align  1
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
        .byte     3
        .ascii  " ok"
        .align  1
        rcall    TYPE
        rcall   PROMPT_
        jmp     QUIT0


        fdw     QUIT_L
PROMPT_L:
        .byte     NFA|6
        .ascii    "prompt"
        .align  1
PROMPT_:
        call    DEFER_DOES
        .word     prompt

; ABORT    i*x --   R: j*x --   clear stk & QUIT
        fdw     PROMPT_L
ABORT_L:
        .byte     NFA|5
        .ascii  "abort"
        .align  1
ABORT:
        rcall   S0
        rcall   FETCH_A
        rcall   SPSTORE
        jmp     QUIT            ; QUIT never rets

; ?ABORT   f --       abort & print ?
        fdw     ABORT_L
QABORTQ_L:
        .byte     NFA|7
        .ascii  "?abort?"
        .align  1
QABORTQ:
        rcall   XSQUOTE
        .byte     1
        .ascii  "?"
        .align  1
        jmp     QABORT


; ?ABORT   f c-addr u --       abort & print msg if flag is false
        fdw     QABORTQ_L
QABORT_L:
        .byte     NFA|6
        .ascii  "?abort"
        .align  1
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
        .byte     NFA|IMMED|COMPILE|6
        .ascii  "abort"
        .byte 0x22
        .align  1
ABORTQUOTE:
        rcall   SQUOTE
        rcall   DOCOMMAXT
        fdw     QABORT
        ret

;***************************************************
; LIT   -- x    fetch inline 16 bit literal to the stack
        fdw     ABORTQUOTE_L
DOLIT_L:
        .byte     NFA|3
        .ascii  "lit"
        .align  1
DOLIT:
        m_pop_zh
        pop     zh
        pop     zl
        rcall   FETCHLIT
        lsr     zh
        ror     zl
        m_ijmp    ; (z)

; DUP must not be reachable from user code with rcall
        fdw     RFETCH_L
DUP_L:
        .byte     NFA|INLINE|3
        .ascii  "dup"
        .align  1
DUP:
        m_dup
        ret

        fdw     ZEROEQUAL_L
ZEROLESS_L:
        .byte     NFA|2
        .ascii  "0<"
        .align  1
ZEROLESS:
        lsl     tosh
        sbc     tosl, tosl
        sbc     tosh, tosh
        ret


; '    -- xt             find word in dictionary
        fdw     DOLIT_L
TICK_L:
        .byte     NFA|1,0x27    ; 27h = '
        .align  1
TICK:
        rcall   BL
        rcall   WORD
        rcall   FIND
        jmp     QABORTQ

; CHAR   -- char           parse ASCII character
        fdw     TICK_L
CHAR_L:
        .byte     NFA|4
        .ascii  "char"
        .align  1
CHAR:
        rcall   BL
        rcall   PARSE
        rcall   DROP
        jmp     CFETCH

; (    --                     skip input until )
        fdw     CHAR_L
PAREN_L:
        .byte     NFA|IMMED|1
        .ascii  "("
        .align  1
PAREN:
        rcall   DOLIT
        .word     ')'
        rcall   PARSE
        sbr     FLAGS1, (1<<noclear) ; dont clear flags in case of (
        jmp     TWODROP

; IHERE    -- a-addr    ret Code dictionary ptr
;   IDP @ ;
        fdw     PAREN_L
IHERE_L:
        .byte     NFA|5
        .ascii  "ihere"
        .align  1
IHERE:
        rcall   IDP
        rjmp    FETCH_A

; [CHAR]   --          compile character DOLITeral
        fdw     IHERE_L
BRACCHAR_L:
        .byte     NFA|IMMED|COMPILE|6
        .ascii  "[char]"
        .align  1
BRACCHAR:
        rcall   CHAR
        jmp     LITERAL

; COMPILE,  xt --         append codefield
        .byte     NFA|3
        .ascii  "cf,"
        .align  1
COMMAXT_A:
        jmp     COMMAXT

; CR      --                      output newline
        fdw     BRACCHAR_L
CR_L:
        .byte     NFA|2
        .ascii  "cr"
        .align  1
CR:
        rcall   DOLIT
        .word     0x0d       ; CR \r
        call    EMIT
        rcall   DOLIT
        .word     0x0a       ; LF \n
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
        .byte     NFA|6
        .ascii  "create"
        .align  1
CREATE:
        rcall   BL
        rcall   WORD            ; Parse a word

        rcall   DUP             ; Remember parsed word at rhere
        rcall   FIND
        rcall   NIP
        rcall   ZEROEQUAL
        rcall   XSQUOTE
        .byte     15
        .ascii  "ALREADY DEFINED"
        .align  1
        rcall   QABORT         ; ABORT if word has already been defined
        rcall   DUP             ; Check the word length 
        rcall   CFETCH_A
        rcall   ONE
        rcall   DOLIT
        .word     16
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

        rcall   OVER
        ori     tosl, NFA
        rcall   OVER
        rcall   CSTORE_A
        rcall   CHARPLUS
        rcall   SWOP
        rcall   CMOVE          ; Store the name to flash

        rcall   IHERE
        rcall   CFETCH_A
        andi    tosl, ~NFA
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
        .byte     NFA|IMMED|COMPILE|8
        .ascii  "postpone"
        .align  1
POSTPONE:
        rcall   BL
        rcall   WORD
        rcall   FIND
        rcall   DUP
        rcall   QABORTQ
        rcall   ZEROLESS
        rcall   ZEROSENSE
        breq    POSTPONE1
        call    DOCOMMAXT
        fdw     DOCOMMAXT
        rjmp    ICOMMA_
POSTPONE1:
        jmp     COMMAXT


IDP_L:
        .byte     NFA|3
        .ascii  "idp"
        .align  1
IDP:
        call    DOCREATE
        .word     dpFLASH

;***************************************************************
; (DOES>)  --      run-time action of DOES>
;        .word    link
;link   set     $
        .byte     NFA|7
        .ascii  "(does>)"
        .align  1
XDOES:
        m_pop_zh
        rcall   RFROM       ; dodoes-xa
        rcall   LATEST_
        rcall   FETCH_A
        rcall   NFATOCFA    ; dodoes-xa markerword-cfa
        rcall   IDP
        rcall   FETCH_A     ;  dodoes-xa markerword-cfa idp
        rcall   TOR_A       ;  dodoes-xa markerword-cfa
        rcall   IDP
        rcall   STORE_A     ;  dodoes-xa
        rcall   DOLIT
        .word     0x940E    ; call
        call    ICOMMA
        call    ICOMMA
        rcall   RFROM
        rcall   IDP
        jmp     STORE


; DOES>    --      change action of latest def'n
        fdw     POSTPONE_L
DOES_L:
        .byte     NFA|IMMED|COMPILE|5
        .ascii  "does>"
        .align  1
DOES:   call    DOCOMMAXT
        fdw     XDOES
        call    DOCOMMAXT
        fdw     DODOES
        ret


;*****************************************************************
; [        --      enter interpretive state
        fdw     DOES_L
LEFTBRACKET_L:
        .byte     NFA|IMMED|1
        .ascii  "["
        .align  1
LEFTBRACKET:
        sts     state, r_zero
        ret


; ]        --      enter compiling state
        fdw     LEFTBRACKET_L
RIGHTBRACKET_L:
        .byte     NFA|1
        .ascii  "]"
        .align  1
RIGHTBRACKET:
        sts     state, r_one
        ret

; :        --           begin a colon definition
        fdw     RIGHTBRACKET_L
COLON_L:
        .byte     NFA|1
        .ascii  ":"
        .align  1
COLON:
        rcall   CREATE
        rcall   RIGHTBRACKET
        jmp     STORCOLON

; :noname        -- a          define headerless forth code
        fdw     COLON_L
NONAME_L:
        .byte     NFA|7
        .ascii  ":noname"
        .align  1
NONAME:
        rcall   IHERE
        jmp     RIGHTBRACKET

; ;        --             end a colon definition
        fdw     NONAME_L
SEMICOLON_L:
        .byte     NFA|IMMED|COMPILE|1
        .ascii  ";"
        .align  1
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
        m_drop
        rcall   MINUS_FETCH
        subi    tosl, 0x0e
        sbci    tosh, 0x94
        brne    ADD_RETURN
CALL_TO_JMP:
        ldi     tosl, 0x0c
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
        .word     -2
        rcall   IALLOT
        rcall   DOLIT
        .word     0x940c      ; jmp:0x940c
        rcall   ICOMMA__
        sub_pflash_tos
        lsr     tosh
        ror     tosl
        rjmp    ICOMMA__
ADD_RETURN:
        rcall   TWODROP
ADD_RETURN_1:
        rcall   DOLIT   ; Compile a ret
        .word     0x9508
ICOMMA__:
        jmp    ICOMMA



        fdw     SEMICOLON_L
MINUS_FETCH_L:
        .byte     NFA|2
        .ascii  "-@"
        .align  1
MINUS_FETCH:
        rcall   TWOMINUS
        rcall   DUP
        jmp     FETCH

; [']  --         find word & compile as DOLITeral
        fdw     MINUS_FETCH_L
BRACTICK_L:
        .byte     NFA|IMMED|COMPILE|3
        .ascii  "[']"
        .align  1
BRACTICK:
        rcall   TICK       ; get xt of 'xxx'
        jmp     LITERAL

; 2-    n -- n-2
        fdw     BRACTICK_L
TWOMINUS_L:
        .byte     NFA|INLINE|2
        .ascii  "2-"
        .align  1
TWOMINUS:
        sbiw    tosl, 2
        ret

        
; BL      -- char                 an ASCII space
        fdw     TWOMINUS_L
BL_L:
        .byte     NFA|2
        .ascii  "bl"
        .align  1
BL:
        call    DOCREATE
        .word     ' '

; STATE   -- flag                 holds compiler state
        fdw     BL_L
STATE_L:
        .byte     NFA|5
        .ascii  "state"
        .align  1
STATE_:
        m_dup
        lds     tosl, state
        lds     tosh, state
        ret

; LATEST    -- a-addr           
        fdw     STATE_L
LATEST_L:
        .byte   NFA|6
        .ascii  "latest"
        .align  1
LATEST_:
        call    DOCREATE
        .word     dpLATEST

; S0       -- a-addr      start of parameter stack
        fdw     LATEST_L
S0_L:
        .byte     NFA|2
        .ascii  "s0"
        .align  1
S0:
        rcall   DOUSER
        .word     us0
        
; R0       -- a-addr      start of parameter stack
        fdw     S0_L
R0_L:
        .byte     NFA|2
        .ascii  "r0"
        .align  1
R0_:
        rcall   DOUSER
        .word     ur0
        
; ini -- a-addr       ini variable contains the user-start xt
; In RAM
;        .word     link
;link    set     $
        .byte     NFA|3
        .ascii  "ini"
        .align  1
INI:
         call   DOCREATE
        .word     dpSTART

; ticks  -- u      system ticks (0-ffff) in milliseconds
        fdw     R0_L
TICKS_L:
        .byte     NFA|5
        .ascii  "ticks"
        .align  1
TICKS:  
        m_dup
        movw     tosl, ms_count
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
        .byte     NFA|2
        .ascii  "ms"
        .align  1
MS:
        rcall   TICKS
        rcall   PLUS
MS1:    
        rcall   PAUSE
        movw    t0, tosl
        cli
        sub     t0, ms_count      ; tos mscount -
        sbc     t1, ms_count1
        sei
        brpl    MS1
        jmp     DROP

;  .id ( nfa -- ) 
        fdw     MS_L
DOTID_L:
        .byte     NFA|3
        .ascii  ".id"
        .align  1
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
        .byte     NFA|3
        .ascii  ">pr"
        .align  1
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
        .word     0x0f
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
        call    TWODUP
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
        call    TWODUP
        rcall   LIKEQ
        breq    LIKES1
        rcall   DUP
        rcall   DOTID
        call    SPACE_
LIKES1:
        rcall   TWOMINUS
        rcall   FETCH_A
        sbiw    tosl, 0
        brne    LIKES
TWODROP__:
        jmp     TWODROP

 ; WORDS    -- filter
        fdw     TO_PRINTABLE_L
WORDS_L:
        .byte     NFA|5
        .ascii  "words"
        .align  1
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
        .byte     NFA|2
        .ascii  ".s"
        .align  1
DOTS:
        call    SPACE_
        rcall   DUP
        call    SPFETCH
        rcall   S0
        rcall   FETCH_A
        rcall   TWOMINUS
DOTS1:
        call   TWODUP
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
        .byte     NFA|4
        .ascii  "dump"
        .align  1
DUMP:
        adiw    tosl, 15
        rcall   DOLIT
        .word     16
        rcall   USLASH
        rcall   TOR
        rjmp    DUMP7
DUMP1:  
        rcall   CR
        rcall   DUP
        rcall   DOLIT
        .word     4
        rcall   UDOTR
        rcall   DOLIT
        .word     ':'
        rcall   EMIT_A
        rcall   DOLIT
        .word     15
        rcall   TOR
DUMP2:
        rcall   CFETCHPP
        rcall   DOLIT
        .word     2
        rcall   UDOTR
        rcall   XNEXT
        brcc    DUMP2
        pop     t1
        pop     t0

        rcall   DOLIT
        .word     16
        rcall   MINUS
        rcall   DOLIT
        .word     15
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
;       .word     link
;link   set     $
        .byte     NFA|1
        .ascii  " "
        .align  1
IALLOT:
        rcall   IDP
        jmp     PLUSSTORE
    

;***************************************************************
;  Store the execution vector addr to the return stack
; leave the updated return stack pointer on the data stack
; x>r ( addr rsp -- rsp' )
        fdw     DUMP_L
X_TO_R_L:
        .byte     NFA|3
        .ascii  "x>r"
        .align  1
X_TO_R:
        movw    zl, tosl
        m_drop
        rcall   TO_XA
        adiw    zl, 1
        st      -z, tosl
        st      -z, tosh
#if (FLASHEND == 0x3ffff)
        st      -z, r_zero
#endif
        st      -z, r_zero
        movw    tosl, zl
        ret
;***************************************************************
        fdw     X_TO_R_L
TO_XA_L:
        .byte NFA|3
        .ascii  ">xa"
        .align  1
TO_XA:
         sub_pflash_tos
         lsr tosh
         ror tosl
         ret

         fdw     TO_XA_L
XA_FROM_L:
        .byte NFA|3
        .ascii  "xa>"
        .align  1
XA_FROM:
         lsl     tosl
         rol     tosh
         add_pflash_tos
         ret
;***************************************************************
         fdw    XA_FROM_L
PFL_L:
        .byte     NFA|3
        .ascii  "pfl"
        .align  1
PFL:
         call   DOCREATE
        .word     OFLASH
;***************************************************************
        fdw    PFL_L
ZFL_L:
        .byte     NFA|3
        .ascii   "zfl"
        .align  1
ZFL:
         call   DOCREATE
        .word   0
;***************************************************************
; ,?0=    -- addr  Compile ?0= and make make place for a branch instruction
        .byte     NFA|4
        .ascii   ",?0="    ; Just for see to work !
        .align  1
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
        .word     -4
        rjmp    IALLOT

;       rjmp, ( rel-addr -- )
RJMPC:
        rcall   TWOSLASH
        andi    tosh, 0x0f
        ori     tosh, 0xc0
        rjmp    ICOMMA__


BRCCC:
        rcall   DOLIT
        .word     0xf008      ; brcc pc+2
        rjmp    ICOMMA__
;BREQC:
;        rcall   DOLIT
;        .word     0xf009      ; breq pc+2
;        sbrc    FLAGS1, izeroeq
;        ori     tosh, 4     ; brne pc+2
;        jmp     ICOMMA
BRNEC:
        rcall   DOLIT
        .word     0xf409      ; brne pc+2
        sbrc    FLAGS1, izeroeq
        andi    tosh, ~4
        rjmp    ICOMMA__

; IF       -- adrs   conditional forward branch
; Leaves address of branch instruction 
; and compiles the condition byte
        fdw     ZFL_L
IF_L:
        .byte     NFA|IMMED|COMPILE|2
        .ascii  "if"
        .align  1
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
        .byte     NFA|IMMED|COMPILE|4
        .ascii  "else"
        .align  1
ELSE_:
        rcall   IHERE
        rcall   FALSE_
        rcall   RJMPC
        rcall   SWOP_A      ; else-addr  if-addr 
        jmp     THEN_

; THEN     adrs  --        resolve forward branch
        fdw     ELSE_L
THEN_L:
        .byte     NFA|IMMED|COMPILE|4
        .ascii  "then"
        .align  1
THEN_:
        sbr     FLAGS1, (1<<fTAILC)  ; Prevent tailjmp  optimisation
        rcall   IHERE
        call    OVER
        rcall   MINUS
        rcall   TWOMINUS
        rcall   TWOSLASH
        rcall   DOLIT
        .word     0xc000      ;  back-addr mask 
        rcall   OR_
        rcall   SWOP_A
        jmp     STORE

; BEGIN    -- adrs        target for bwd. branch
        fdw     THEN_L
BEGIN_L:
        .byte     NFA|IMMED|COMPILE|5
        .ascii  "begin"
        .align  1
BEGIN:
        jmp     IHERE

; UNTIL    adrs --   Branch bakwards if true
        fdw     BEGIN_L
UNTIL_L:
        .byte     NFA|IMMED|COMPILE|5
        .ascii  "until"
        .align  1
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
        .byte     NFA|IMMED|COMPILE|5
        .ascii  "again"
        .align  1
AGAIN_:
        sbr     FLAGS1, (1<<fTAILC)  ; Prevent tailjmp  optimisation
        rcall   IHERE
        call    MINUS
        rcall   TWOMINUS
        jmp     RJMPC

; WHILE    addr1 -- addr2 addr1         branch for WHILE loop
; addr1 : address of BEGIN
; addr2 : address where to store bz instruction
        fdw     AGAIN_L
WHILE_L:
        .byte     NFA|IMMED|COMPILE|5
        .ascii  "while"
        .align  1
WHILE_:
        rcall   IF_
        jmp     SWOP

; REPEAT   addr2 addr1 --     resolve WHILE loop
        fdw     WHILE_L
REPEAT_L:
        .byte     NFA|IMMED|COMPILE|6
        .ascii  "repeat"
        .align  1
REPEAT_:
        rcall   AGAIN_
        jmp     THEN_

        fdw     REPEAT_L
INLINE_L:
        .byte      NFA|IMMED|COMPILE|6
        .ascii  "inline"
        .align  1
        cbr      FLAGS1, (1<<izeroeq)
        cbr      FLAGS1, (1<<idup)
        rcall    TICK
        jmp      INLINE0
; in, ( addr -- ) begin @+ dup $9508 <> while i, repeat 2drop ;
        fdw      INLINE_L
INLINEC_L:
        .byte      NFA|3
        .ascii  "in,"
        .align  1
INLINE0:        
        rcall   FETCHPP
        rcall   DUP
        rcall   DOLIT
        .word     0x9508
        rcall   NOTEQUAL
        rcall   ZEROSENSE
        breq    INLINE1
        call   ICOMMA
        rjmp    INLINE0
INLINE1:
        jmp     TWODROP

; FOR   -- bc-addr bra-addr
        fdw     INLINEC_L
FOR_L:
        .byte     NFA|IMMED|COMPILE|3
        .ascii  "for"
        .align  1
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
        .byte     NFA|IMMED|COMPILE|4
        .ascii  "next"
        .align  1
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
        .byte     NFA|7
        .ascii  "(next) "
        .align  1
XNEXT:  
        m_pop_zh
        pop     zh
        pop     zl
        pop     xh
        pop     xl
        sbiw    xl, 1
        push    xl
        push    xh
        m_ijmp
        ret
XNEXT1:
        pop     t1
        pop     t0
        ret

; leave clear top of return stack
        fdw     NEXT_L
LEAVE_L:
        .byte     NFA|COMPILE|5
        .ascii  "endit"
        .align  1
LEAVE:
        m_pop_zh
        pop     zh
        pop     zl
        pop     t1
        pop     t0
        push    r_zero
        push    r_zero
        m_ijmp
;***************************************************
; RDROP compile a pop
        fdw      LEAVE_L
RDROP_L:
        .byte      NFA|COMPILE|INLINE|5
        .ascii  "rdrop"
        .align  1
RDROP:
        pop     t0
        pop     t0
        ret
;***************************************************
        fdw     RDROP_L
STOD_L:
        .byte     NFA|3
        .ascii  "s>d"
        .align  1
STOD:
        sbrs    tosh, 7
        rjmp    FALSE_
        rjmp    TRUE_
;***************************************************
        fdw     STOD_L
DNEGATE_L:
        .byte     NFA|7
        .ascii  "dnegate"
        .align  1
DNEGATE:
        rcall   DINVERT
        call    ONE
        jmp     MPLUS
;***************************************************
        fdw     DNEGATE_L
QDNEGATE_L:
        .byte     NFA|8
        .ascii  "?dnegate"
        .align  1
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
        .byte     NFA|4
        .ascii  "dabs"
        .align  1
DABS:
        rcall   DUP
        jmp     QDNEGATE
;***************************************************
        fdw     DABS_L
DPLUS_L:
        .byte     NFA|2
        .ascii  "d+"
        .align  1
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
        .byte     NFA|2
        .ascii  "d-"
        .align  1
DMINUS:
        rcall   DNEGATE
        jmp     DPLUS
;***************************************************
        fdw     DMINUS_L
DTWOSLASH_L:
        .byte     NFA|3
        .ascii  "d2/"
        .align  1
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
        .byte     NFA|3
        .ascii  "d2*"
        .align  1
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
        .byte     NFA|7
        .ascii  "dinvert"
        .align  1
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
        .byte     NFA|3
        .ascii  "d0="
        .align  1
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
        .byte     NFA|3
        .ascii  "d0<"
        .align  1
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
        .byte     NFA|2
        .ascii  "d="
        .align  1
        rcall   DMINUS
        jmp     DZEROEQUAL
;***************************************************
        fdw     DEQUAL_L
DLESS_L:
        .byte     NFA|2
        .ascii  "d<"
        .align  1
DLESS:
        rcall   DMINUS
        ld      tosl, y+
        ld      tosh, y+
        ldi     tosl, 0xff
        ldi     tosh, 0xff
        brlt    DLESS_1
        com     tosl
        com     tosh
DLESS_1:
        ret
;***************************************************
        fdw     DLESS_L
DGREATER_L:
        .byte     NFA|2
        .ascii  "d>"
        .align  1
DGREATER:
        call    TWOSWAP
        jmp     DLESS
;***************************************************
        fdw     DGREATER_L
UDDOT_L:
        .byte     NFA|3
        .ascii  "ud."
        .align  1
        rcall   LESSNUM
        rcall   NUMS
        rcall   NUMGREATER
        call    TYPE
        jmp     SPACE_
;***************************************************
        fdw     UDDOT_L
DDOT_L:
        .byte     NFA|2
        .ascii  "d."
        .align  1
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
9:
        .byte   NFA|6
        .ascii  "unused"
        .align  1
UNUSED:
        rcall   MEMHI
        call    HERE
        call    MINUS
        jmp     ONEPLUS

        fdw      9b
9:
        .byte     NFA|2
        .ascii  "hi"
        .align  1
MEMHI:
        rcall   DOLIT
        fdw     FLASHHI
        call    CSE_
        call    PLUS
        jmp     FETCH
FLASHHI:
        .word      FLASH_HI
        .word      EEPROM_HI
        .word      RAM_HI

#if FLASHEND > 0xffff
;;; x@ ( addrl addru -- x )
        fdw     A_FROM_L
XFETCH_L:
        .byte     NFA|2
        .ascii  "x@"
        .align  1
#if FLASHEND > 0xffff
        m_out    RAMPZ, tosl
#endif
        m_drop
        movw    z, tosl
        m_lpm    tosl     ; Fetch from Flash directly
        m_lpm    tosh
#if FLASHEND > 0xffff
        m_out    RAMPZ, r_zero
#endif
	ret
	
;;; x! ( x addrl addru -- )
        fdw     XFETCH_L
XSTORE_L:
        .byte     NFA|2
        .ascii  "x!"
        .align  1
        mov     t0, tosl
        m_drop
        call   XUPDATEBUF
        jmp    ISTORE1
#endif

;***************************************************
#include <registers.inc>
;***************************************************
        fdw      9b
L_FETCH_P:
        .byte      NFA|INLINE|2
        .ascii  "@p"
        .align  1
FETCH_P:
        m_dup
        movw    tosl, pl
        ret
;***************************************************
        fdw     L_FETCH_P
L_PCFETCH:
        .byte     NFA|3
        .ascii  "pc@" ; ( -- c ) Fetch char from pointer
        .align  1
PCFETCH:
        m_dup
        movw    tosl, pl
        jmp     CFETCH
;***************************************************
        fdw      L_PCFETCH
L_PAD:
        .byte   NFA|3
        .ascii  "pad"
        .align  1
PAD:
        m_dup
        lds     tosl, dpRAM
        lds     tosh, dpRAM+1
        ret
;***************************************************
        fdw      L_PAD
L_PTWOPLUS:
kernellink:
        .byte     NFA|INLINE|3
        .ascii  "p2+" ; ( n -- ) Add 2 to p
        .align  1
PTWOPLUS:
        add     pl, r_two
        adc     ph, r_zero
        ret

;***************************************************
; marker --- name
        .word     0
L_MARKER:
lastword:
        .byte     NFA|6
        .ascii  "marker"
        .align  1
MARKER:
        call    ROM_
        rcall   CREATE
        rcall   DOLIT
        .word   dp_start
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

#if IDLE_MODE == 1
#if CPU_LOAD_LED == 1
;;; Enable load led
        fdw     BUSY_L
LOADON_L:
        .byte     NFA|5
        .ascii  "load+"
        .align  1
LOAD_ON:
        sbr     FLAGS2, (1<<fLOADled)
        ret

;;; Disable load led
        fdw     LOADON_L
LOADOFF_L:
        .byte     NFA|5
        .ascii  "load-"
        .align  1
        cbr     FLAGS2, (1<<fLOADled)
#if CPU_LOAD_LED == 1
#if CPU_LOAD_LED_POLARITY == 1
        m_cbi    CPU_LOAD_PORT, CPU_LOAD_BIT
#else
        m_sbi    CPU_LOAD_PORT, CPU_LOAD_BIT
#endif
#endif
        ret
#endif
;;; 
#if CPU_LOAD == 1
#if CPU_LOAD_LED == 1
        fdw     LOADOFF_L
#else
        fdw     BUSY_L
#endif
LOAD_L:
        .byte     NFA|4
        .ascii  "load"
        .align  1
        rcall DUP
        lds     tosl, load_res
        lds     tosh, load_res+1
        rcall DUP
        lds     tosl, load_res+2
        clr     tosh
        rcall DUP
        ldi     tosl, lo8(CPU_LOAD_VAL)
        ldi     tosh, hi8(CPU_LOAD_VAL)
        call    UMSLASHMOD
        jmp     NIP 
#endif
#endif

#if UARTS == 2
;***************************************************
; TX1   c --    output character to UART 1
        fdw     RX0Q_L
TX1_L:
        .byte     NFA|3
        .ascii  "tx1"
        .align  1
TX1_:
        cpi     tosl, XON
        breq    XXON_TX1_TOS
        cpi     tosl, XOFF
        breq    XXOFF_TX1_TOS
TX1_LOOP:
        rcall   PAUSE
        m_in     t0, UCSR1A
        sbrs    t0, UDRE1
        rjmp    TX1_LOOP
        m_out    UDR1, tosl
        m_drop
        ret

XXON_TX1_TOS:
        m_drop
        rjmp    XXON_TX1_1
XXON_TX1:
        sbrs    FLAGS2, ixoff_tx1
        ret
XXON_TX1_1:
        cbr     FLAGS2, (1<<ixoff_tx1)
        ldi     zh, XON
        rjmp    TX1_SEND

XXOFF_TX1_TOS:
        m_drop
        rjmp    XXOFF_TX1_1
XXOFF_TX1:
        sbrc    FLAGS2, ixoff_tx1
        ret     
XXOFF_TX1_1:
        sbr     FLAGS2, (1<<ixoff_tx1)
        ldi     zh, XOFF
TX1_SEND:
        m_in     zl, UCSR1A
        sbrs    zl, UDRE1
        rjmp    TX1_SEND
        m_out    UDR1, zh
        ret
;***************************************************
; RX1    -- c    get character from the serial line
        fdw     TX1_L
RX1_L:
        .byte     NFA|3
        .ascii  "rx1"
        .align  1
RX1_:
        rcall   PAUSE
        rcall   RX1Q
        call    ZEROSENSE
        breq    RX1_
        m_dup
        ldi     zl, lo8(rbuf1)
        ldi     zh, hi8(rbuf1)
        lds     xl, rbuf1_rd
        add     zl, xl
        adc     zh, r_zero
        ld      tosl, z
        clr     tosh
        m_in     t0, SREG
        cli
        inc     xl
        andi    xl, (RX1_BUF_SIZE-1)
        sts     rbuf1_rd, xl
        lds     xl, rbuf1_lv
        dec     xl
        sts     rbuf1_lv, xl
        m_out    SREG, t0
        ret
;***************************************************
; RX1?  -- n    return the number of characters in queue
        fdw     RX1_L
RX1Q_L:
        .byte     NFA|4
        .ascii  "rx1?"
        .align  1
RX1Q:
        lds     xl, rbuf1_lv
        cpse    xl, r_zero
        jmp     TRUE_
#if U1FC_TYPE == 1
        rcall   XXON_TX1
#endif
#if U1FC_TYPE == 2
        m_cbi    U1RTS_PORT, U1RTS_BIT
#endif
        jmp     FALSE_
#endif
;****************************************************
;***************************************************
RQ_EMIT:
        sbrs    t2, PORF
        rjmp    RQ_EXTR
        rcall   DOLIT
        .word     'P'
        rcall   EMIT_A
RQ_EXTR:
        sbrs    t2, EXTRF
        rjmp    RQ_BORF
        rcall   DOLIT
        .word     'E'
        rcall   EMIT_A
RQ_BORF:
        sbrs    t2, BORF
        rjmp    RQ_WDRF
        rcall   DOLIT
        .word     'B'
        rcall   EMIT_A
RQ_WDRF:
        sbrs    t2, WDRF
        rjmp    RQ_DIVZERO
        rcall   DOLIT
        .word     'W'
        rcall   EMIT_A
RQ_DIVZERO:
        sbrs    t3, 6 ; T bit MATH error
        rjmp    RQ_END
        rcall   DOLIT
        .word     'M'
        rcall   EMIT_A
RQ_END: 
        jmp    SPACE_

;*****************************************************
#if IDLE_MODE == 1
IDLE_LOAD:
#if CPU_LOAD == 1       
        sbrs    FLAGS2, fLOAD
        rjmp    CPU_LOAD_END
        m_in    t0, SREG
        cli
        cbr     FLAGS2, (1<<fLOAD)
        sts     load_res, loadreg0
        sts     load_res+1,loadreg1
        sts     load_res+2, loadreg2
        clr     loadreg0
        clr     loadreg1
        clr     loadreg2
        m_out   SREG, t0
CPU_LOAD_END:
#endif
#if CPU_LOAD_LED == 1
        sbrs    FLAGS2, fLOADled
        rjmp    LOAD_LED_END
#if CPU_LOAD_LED_POLARITY == 1
        m_cbi   CPU_LOAD_PORT, CPU_LOAD_BIT
#else
        m_sbi   CPU_LOAD_PORT, CPU_LOAD_BIT
#endif
LOAD_LED_END:
#endif
        sbrs    FLAGS2, fIDLE
        rjmp    IDLE_LOAD1
        ldi     t0, lo8(up0)
        cp      upl, t0
        brne    IDLE_LOAD1
#ifdef SMCR
        lds     zl, rbuf0_lv
#ifdef rbuf1_lv
        lds     zh, rbuf1_lv
        or      zl, zh
#else
        cpi     zl, 0
#endif
        brne    IDLE_LOAD1
        m_out    SMCR, r_one
#else
        m_in     t0, MCUCR
        sbr     t0, (1<<SE)
        m_out    MCUCR, t0
#endif
#if CPU_LOAD == 1
        m_out    TCCR1B, r_zero    ; Stop load counter
#endif
        sleep               ; IDLE mode
#ifdef SMCR
        m_out    SMCR, r_zero
#else
        m_in     t0, MCUCR
        cbr     t0, (1<<SE)
        m_out    MCUCR, t0
#endif
IDLE_LOAD1:
#if CPU_LOAD_LED == 1
        sbrc    FLAGS2, fLOADled
#if CPU_LOAD_LED_POLARITY == 1
        m_sbi    CPU_LOAD_PORT, CPU_LOAD_BIT
#else
        m_cbi    CPU_LOAD_PORT, CPU_LOAD_BIT
#endif
#endif
        ret
#endif

;***************************************************
; TX0   c --    output character to UART 0
#if IDLE_MODE == 1
#if CPU_LOAD == 1
        fdw(LOAD_L)
#else
#if CPU_LOAD_LED == 1
        fdw(LOADOFF_L)
#else
        fdw(BUSY_L)
#endif
#endif
#else
        fdw(EXIT_L)
#endif
TX0_L:
        .byte     NFA|3
        .ascii  "tx0"
TX0_:
#if U0FC_TYPE == 1
        cpi     tosl, XON
        breq    XXON_TX0_TOS
        cpi     tosl, XOFF
        breq    XXOFF_TX0_TOS
#endif
TX0_LOOP:
        rcall   PAUSE
        m_in     t0, UCSR0A
        sbrs    t0, 5        ; UDRE0, UDRE USART Data Register Empty
        rjmp    TX0_LOOP
        m_out   UDR0_, tosl
        m_drop
        ret

#if U0FC_TYPE == 1
XXON_TX0_TOS:
        m_drop
        rjmp    XXON_TX0_1
XXON_TX0:
        sbrs    FLAGS2, ixoff_tx0
        ret
XXON_TX0_1:
        cbr     FLAGS2, (1<<ixoff_tx0)
        ldi     zh, XON
        rjmp    TX0_SEND

XXOFF_TX0_TOS:
        m_drop
        rjmp    XXOFF_TX0_1
XXOFF_TX0:
        sbrc    FLAGS2, ixoff_tx0
        ret     
XXOFF_TX0_1:
        sbr     FLAGS2, (1<<ixoff_tx0)
        ldi     zh, XOFF
#endif
TX0_SEND:
        m_in    zl, UCSR0A
        sbrs    zl, 5        ; UDRE0, UDRE USART Data Register Empty
        rjmp    TX0_SEND
        m_out   UDR0_, zh
        ret
;***************************************************
; RX0    -- c    get character from the UART 0 buffer
        fdw(TX0_L)
RX0_L:
        .byte     NFA|3
        .ascii  "rx0"
        .align  1
RX0_:
        rcall   PAUSE
        rcall   RX0Q
        call    ZEROSENSE
        breq    RX0_
        m_dup
        ldi     zl, lo8(rbuf0)
        ldi     zh, hi8(rbuf0)
        lds     xl, rbuf0_rd
        add     zl, xl
        adc     zh, r_zero
        ld      tosl, z
        clr     tosh
        m_in    t0, SREG
        cli
        inc     xl
        andi    xl, (RX0_BUF_SIZE-1)
        sts     rbuf0_rd, xl
        lds     xl, rbuf0_lv
        dec     xl
        sts     rbuf0_lv, xl
        m_out    SREG, t0
        ret
;***************************************************
; RX0?  -- n    return the number of characters in queue
        fdw     RX0_L
RX0Q_L:
        .byte     NFA|4
        .ascii  "rx0?"
        .align  1
RX0Q:
        lds     xl, rbuf0_lv
        cpse    xl, r_zero
        jmp     TRUE_
#if U0FC_TYPE == 1
        rcall   XXON_TX0
#endif
#if U0FC_TYPE == 2
        m_cbi    U0RTS_PORT, U0RTS_BIT
#endif
        jmp     FALSE_


        fdw     CWD_L
IFLUSH_L:
        .byte     NFA|6
        .ascii  "iflush"
        .align  1
IFLUSH:
        sbrc    FLAGS1, idirty
        jmp     IWRITE_BUFFER
        ret

;***************************************************
#if UARTS == 2
        fdw     RX1Q_L
#else
        fdw     RX0Q_L
#endif
EMPTY_L:
        .byte     NFA|5
        .ascii  "empty"
        .align  1
EMPTY:
        rcall   DOLIT
        fdw     COLDLIT
        rcall   DOLIT
        .word   dp_start
        rcall   DOLIT
        .word   coldlitsize
        call    CMOVE
        call    FALSE_
        rcall   DOLIT
        .word   dp_flash
        call    CSTORE
        jmp     DP_TO_RAM

; Init constant registers
INIT_012:
        clr     r_zero
        ldi     zl, 1
        ldi     zh, 2
        movw    r_one, zl
        ret
;*******************************************************
#ifdef USBCON
        fdw     TXU_L
#else
        fdw     EMPTY_L
#endif
WARM_L:
        .byte     NFA|4
        .ascii  "warm"
        .align  1
WARM_0:
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

        m_in     t3, SREG
#ifdef MCUCSR
        m_in     t2, MCUCSR
        sts     MCUCSR, r_zero
#elif defined(MCUSR)
        m_in     t2, MCUSR
        sts     MCUSR, r_zero
#endif
        ldi     xh, hi8(RAMSTART)

#ifdef USBCON
  	    m_sbi	USBCON, OTGPADE			; Enable USB power pads
        ldi   xl, lo8(RAMSTART + 0x10)
#else
        ldi   xl, lo8(RAMSTART)
#endif
WARM_2:
        st      x+, r_zero
        cpi     xh, hi8(RAMEND) 
        brne    WARM_2
        cpi     xl, lo8(RAMEND)
        brne    WARM_2
        st      x, r_zero

; Init empty flash buffer
        dec     ibaseh
#if FLASHEND > 0xffff
        sts     ibaseu, ibaseh
#endif

; Init Stack pointer
        ldi     yl, lo8(utibbuf-4)
        ldi     yh, hi8(utibbuf-4)

; Init Return stack pointer
        ldi     t0, lo8(usbuf-1)
        ldi     t1, hi8(usbuf-1)
        out     spl, t0
        out     sph, t1

        rcall   INIT_012
        call    WDOFF

; Init user pointer
        ldi     t0, lo8(up0)
        ldi     t1, hi8(up0)
        movw    upl, t0
; Set RAMPZ for correct flash addressing
#if FLASHEND > 0xffff
        m_out    RAMPZ, r_zero
#endif
; init warm literals
        rcall   DOLIT
        fdw     WARMLIT
        rcall   DOLIT
        .word   cse
        rcall   DOLIT
        .word   warmlitsize
        call    CMOVE
; init cold data to eeprom
        rcall   DOLIT
        .word   dp_start
        rcall   FETCH
        rcall   TRUE_
        call    EQUAL
        call    ZEROSENSE
        breq    WARM_3  
        rcall   EMPTY
WARM_3:
; Start watchdog timer
#if MS_TIMER == 0
#ifdef TIMSK0
        m_out    TCCR0A, r_two  ; CTC
        ldi     t0, ms_pre_tmr0
        m_out    TCCR0B, t0
        ldi     t0, ms_value_tmr0
        m_out    OCR0A, t0
        m_out    TIMSK0, r_two ; (1<<OCIE0A)
#endif
#ifdef TIMSK
        ldi     t0, (ms_pre_tmr0 | ( 1<<WGM01 ))
        m_out    TCCR0, t0
        ldi     t0, ms_value_tmr0
        m_out    OCR0, t0
        ldi     t0, (1<<OCIE0)
        m_out    TIMSK, t0
#endif
#endif
#if MS_TIMER == 1
; Init ms timer
        ldi     t0, 9      ; CTC, clk/1
        m_out    TCCR1B, t0
        ldi     t0, hi8(ms_value_tmr1)
        m_out    OCR1AH, t0
        ldi     t0, lo8(ms_value_tmr1)
        m_out    OCR1AL, t0
#ifdef TIMSK
        ldi     t0, (1<<OCIE1A)
        m_out    TIMSK, t0
#endif
#ifdef TIMSK1
        m_out    TIMSK1, r_two ; (1<<OCIE1A)
#endif
#endif
#if MS_TIMER == 2
; Init ms timer
#ifdef TIMSK2
        m_out    TCCR2A, r_two   ; CTC
        ldi     t0, ms_pre_tmr2
        m_out    TCCR2B, t0
        ldi     t0, ms_value_tmr2
        m_out    OCR2A, t0
        m_out    TIMSK2, r_two ; t0, (1<<OCIE2A)
#endif
#ifdef TIMSK
        ldi     t0, (ms_pre_tmr2 | ( 1<<WGM21 ))
        m_out    TCCR2, t0
        ldi     t0, ms_value_tmr2
        m_out    OCR2, t0
        ldi     t0, (1<<OCIE2)
        m_out    TIMSK, t0
#endif
#endif

; Init UART 0
#if UARTS >= 1
        rcall   DOLIT
        .word   RX0_ISR + PFLASH
        rcall   DOLIT
#ifdef USART_RX_vect_num
        .word   USART_RX_vect_num + 1    ; m328
#else
#ifdef  USART0_RX_vect_num
        .word   USART0_RX_vect_num + 1   ; m128 m2560
#else
        .word   USART1_RX_vect_num + 1   ; m32u2 m32u4
#endif
#endif
        rcall   IRQ_V
;;;     Set baud rate
;        m_out    UBRR0H, r_zero
        ldi     t0, ubrr0val
        m_out   UBRR0L, t0
        ; Enable receiver and transmitter, rx1 interrupts
        ldi     t0, (1<<RXEN0)|(1<<TXEN0)|(1<<RXCIE0)
        m_out   UCSR0B,t0
        ; Set frame format: 8data, 1stop bit
        ldi     t0, (3<<UCSZ00)|URSEL_
        m_out   UCSR0C,t0
#if U0FC_TYPE == 1
        sbr     FLAGS2, (1<<ixoff_tx0)
#endif
#if U0FC_TYPE == 2
        m_sbi   U0RTS_DDR, U0RTS_BIT
#endif
#endif
; Init UART 1
#if UARTS == 2
        rcall   DOLIT
        .word   RX1_ISR + PFLASH
        rcall   DOLIT
        .word   USART1_RX_vect_num + 1
        rcall   IRQ_V
        ; Set baud rate
;        m_out    UBRR1H, r_zero
        ldi     t0, ubrr1val
        m_out    UBRR1L, t0
        ; Enable receiver and transmitter, rx1 interrupts
        ldi     t0, (1<<RXEN1)|(1<<TXEN1)|(1<<RXCIE1)
        m_out    UCSR1B,t0
        ; Set frame format: 8data, 1stop bit
        ldi     t0, (3<<UCSZ10)
        m_out    UCSR1C,t0
#if U1FC_TYPE == 1
        sbr     FLAGS2, (1<<ixoff_tx1)
#endif
#if U1FC_TYPE == 2
        m_sbi    U1RTS_DDR, U1RTS_BIT
#endif
#endif
        rcall   DP_TO_RAM
#ifdef USBCON
#if OPERATOR_UART == 3
		    call	USB_ON
#endif
#endif
        sts     rbuf0_lv, r_zero
        sts     rbuf0_wr, r_zero
#ifdef rbuf1_lv
        sts     rbuf1_lv, r_zero
        sts     rbuf1_wr, r_zero
#endif
        sei
        call    RQ_EMIT
        rcall   VER
#if CPU_LOAD_LED == 1
        m_sbi    CPU_LOAD_DDR, CPU_LOAD_BIT
#endif
; Turnkey ?
        rcall   TURNKEY
        call    ZEROSENSE
        breq    STARTQ2
        call    XSQUOTE
        .byte     3
        .ascii  "ESC"
        .align  1
        call    TYPE
        rcall   DOLIT
        .word     TURNKEY_DELAY
        call   MS
        call    KEYQ
        call    ZEROSENSE
        breq    STARTQ1
        call    KEY
        rcall   DOLIT
        .word     0x1b
        call    EQUAL
        call    ZEROSENSE
        brne    STARTQ2
STARTQ1:
        rcall   TURNKEY
        call    EXECUTE
STARTQ2:
        jmp     ABORT

        fdw     WARM_L
VER_L:
        .byte     NFA|3
        .ascii  "ver"
        .align  1
VER:
        call    XSQUOTE
         ;      1234567890123456789012345678901234567890
        ;.byte 34,"FlashForth Atmega 5.0 ",DATE,0xd,0xa,0
        .byte     partlen+datelen+16
        .ascii    "FlashForth 5 "
        .ascii    partstring
        .ascii    " "
        .ascii    DATE
        .byte     0xd,0xa
        .align  1
        jmp     TYPE

//; ei  ( -- )    Enable interrupts
        fdw     VER_L
EI_L:
        .byte     NFA|INLINE|2
        .ascii  "ei"
        .align  1
        sei
        ret
        
//; di  ( -- )    Disable interrupts
        fdw     EI_L
DI_L:
        .byte     NFA|INLINE|2
        .ascii  "di"
        .align  1
        cli
        ret
;*******************************************************
; ;i  ( -- )    End definition of user interrupt routine
        fdw     DI_L
IRQ_SEMI_L:
        .byte     NFA|IMMED|2
        .ascii  ";i"
        .align  1
IRQ_SEMI:
        rcall   DOLIT
        .word   0x940C     ; jmp
        rcall   ICOMMA
        rcall   DOLIT
        .word   gs(FF_ISR_EXIT) ; gs get the word address instead of byte address.
        rcall   ICOMMA
        jmp     LEFTBRACKET


; int!  ( addr n  --  )   store to interrupt vector number 
        fdw     IRQ_SEMI_L
IRQ_V_L:
        .byte     NFA|4
        .ascii  "int!"
        .align  1
IRQ_V:
        movw    zl, tosl
        lsl     zl
        ldi     tosl, lo8(ivec)
        add     zl, tosl
        ldi     zh, hi8(ivec)
        m_drop
        call    TO_XA
        jmp     STORE_RAM_2

; DOLITERAL  x --           compile DOLITeral x as native code
        fdw     IRQ_V_L
LITERAL_L:
        .byte     NFA|IMMED|7
        .ascii  "literal"
        .align  1
LITERAL:
        rcall   DOLIT
        fdw     DUP
        call   INLINE0
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
;*****************************************************************
ISTORE:
        call    IUPDATEBUF
ISTORE1:
        m_drop
        ldi     xl, lo8(ibuf)
        ldi     xh, hi8(ibuf)
        lds     t0, iaddrl
        sbrc    t0, 0
        jmp     ISTORERR0
        andi    t0, (PAGESIZEB-1)
        add     xl, t0
        st      x+, tosl
        st      x+, tosh
        rjmp    ICSTORE_POP

        fdw     LITERAL_L
TO_A_L:
        .byte     NFA|2
        .ascii  ">a"
        .align  1
TO_A:
        mov     al, tosl
        mov     ah, tosh
        m_drop
        ret

        fdw     TO_A_L
STORE_L:
        .byte     NFA|1
        .ascii  "!"
        .align  1
STORE:
        cpi     tosh, hi8(PEEPROM)
        brcc    STORE1
STORE_RAM:
        movw    zl, tosl
        m_drop
STORE_RAM_2:
        std     Z+1, tosh
        std     Z+0, tosl
        m_drop
        ret
STORE1:
        rcall   LOCKEDQ
        cpi     tosh, hi8(OFLASH)
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
        call   DOTS
        call    XSQUOTE
        .byte     3
        .ascii  "AD?"
        .align  1
        call    TYPE
        rjmp    STARTQ2        ; goto    ABORT
        
;***********************************************************
IFETCH:
        movw    z, tosl
        sub_pflash_z
        cpse    zh, ibaseh
        rjmp    IIFETCH
        mov     t0, zl
        andi    t0, (~(PAGESIZEB-1)&0xff)
        cp      t0, ibasel
        brne    IIFETCH
        ldi     xl, lo8(ibuf)
        ldi     xh, hi8(ibuf)
        andi    zl, (PAGESIZEB-1)
        add     xl, zl
        ld      tosl, x+
        ld      tosh, x+
        ret
IIFETCH:
        m_lpm    tosl     ; Fetch from Flash directly
        m_lpm    tosh
        ret
                
        fdw     STORE_L
A_FROM_L:
        .byte     NFA|2
        .ascii  "a>"
        .align  1
A_FROM:
        m_dup
        mov     tosl, al
        mov     tosh, ah
        ret

#if FLASHEND > 0xffff
        fdw     XSTORE_L
#else
        fdw     A_FROM_L
#endif
FETCH_L:
        .byte     NFA|1
        .ascii  "@"
        .align  1
FETCH:
        cpi     tosh, hi8(PEEPROM)
        brcc    FETCH1
FETCH_RAM:
        movw    zl, tosl
FETCH_RAM_2:
        ld      tosl, z+
        ld      tosh, z+
        ret
FETCH1:
        cpi     tosh, hi8(OFLASH)
        brcc    IFETCH
EFETCH:
#ifndef EEPE
#define EEPE EEWE
#endif
#ifndef EEMPE
#define EEMPE EEMWE
#endif

        sbic    _SFR_IO_ADDR(EECR), EEPE
        rjmp    EFETCH
        subi    tosh, hi8(PEEPROM)
        out     _SFR_IO_ADDR(EEARL), tosl
        out     _SFR_IO_ADDR(EEARH), tosh
        sbi     _SFR_IO_ADDR(EECR), EERE
        in      t0, _SFR_IO_ADDR(EEDR)
        inc     tosl
        out     _SFR_IO_ADDR(EEARL), tosl
        sbi     _SFR_IO_ADDR(EECR), EERE
        in      tosh,_SFR_IO_ADDR(EEDR)
        mov     tosl, t0
        ret

ICFETCH:
        rcall   IFETCH
        clr     tosh
        ret

        fdw     FETCH_L
CFETCH_L:
        .byte     NFA|2
        .ascii  "c@"
        .align  1
CFETCH:
        cpi     tosh, hi8(PEEPROM)
        brcc    CFETCH1
CFETCH_RAM:
        movw    zl, tosl
        ld      tosl, z+
        clr     tosh
        ret
CFETCH1:
        cpi     tosh, hi8(OFLASH)
        brcc    ICFETCH
ECFETCH:
        rcall   EFETCH
        clr     tosh
        ret

ICSTORE:
        call    IUPDATEBUF
        m_drop
        ldi     xl, lo8(ibuf)
        ldi     xh, hi8(ibuf)
        lds     t0, iaddrl
        andi    t0, (PAGESIZEB-1)
        add     xl, t0
        st      x+, tosl
ICSTORE_POP:
        sbr     FLAGS1, (1<<idirty)
        rjmp    CSTORE_POP

        fdw     CFETCH_L
CSTORE_L:
        .byte     NFA|2
        .ascii  "c!"
        .align  1
CSTORE:
        cpi     tosh, hi8(PEEPROM)
        brcc    CSTORE1
CSTORE_RAM:
        movw zl, tosl
        m_drop
        st      Z, tosl
CSTORE_POP:
        m_drop
        ret
CSTORE1:
        rcall   LOCKEDQ
        cpi     tosh, hi8(OFLASH)
        brcc    ICSTORE
ECSTORE:
        cli
        sbic    _SFR_IO_ADDR(EECR), EEPE
        rjmp    ECSTORE
        subi    tosh, hi8(PEEPROM)
        out     _SFR_IO_ADDR(EEAR), tosl
        out     _SFR_IO_ADDR(EEAR+1), tosh
        m_drop
        out     _SFR_IO_ADDR(EEDR), tosl
        sbi     _SFR_IO_ADDR(EECR), EEMPE
        sbi     _SFR_IO_ADDR(EECR), EEPE
ECSTORE1:
        sbic    _SFR_IO_ADDR(EECR), EEPE
        rjmp    ECSTORE1
        sei
#if DEBUG_FLASH == 1
        rcall   DOLIT
        .word     'E'
        call    EMIT
#endif
        rjmp    CSTORE_POP
;;; Disable writes to flash and eeprom
        fdw     CSTORE_L

FLOCK_L:
        .byte     NFA|3
        .ascii  "fl-"
        .align  1
        sbr     FLAGS1, (1<<fLOCK)
        ret

;;; Enable writes to flash and eeprom
        fdw     FLOCK_L
FUNLOCK_L:
        .byte     NFA|3
        .ascii  "fl+"
        .align  1
        cbr     FLAGS1, (1<<fLOCK)
        ret

        fdw     FUNLOCK_L
VALUE_L:
        .byte     NFA|5
        .ascii  "value"
        .align  1
VALUE:
        rcall   CREATE
        call    COMMA
        rcall   XDOES
VALUE_DOES:
        call    DODOES
        jmp     FETCH

        fdw     VALUE_L
DEFER_L:
        .byte     NFA|5
        .ascii  "defer"
        .align  1
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
        .byte     NFA|IMMED|2
        .ascii  "is"
        .align  1
IS:
        call    TICK
        call    TOBODY
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
        .byte     NFA|IMMED|2
        .ascii  "to"
        .align  1
TO:
        jmp     IS

        fdw     TO_L
TURNKEY_L:
        .byte     NFA|7
        .ascii  "turnkey"
        .align  1
TURNKEY:
        call    VALUE_DOES      ; Must be call for IS to work.
        .word     dpSTART


;;; *******************************************************
; PAUSE  --     switch task
        fdw     TURNKEY_L
PAUSE_L:
        .byte     NFA|5
        .ascii  "pause"
        .align  1
PAUSE:
#ifdef USBCON
        call    USB_device_service
        call    USB_ep_service
#endif
#if IDLE_MODE == 1
        rcall   IDLE_LOAD
#endif
        wdr               ; watchdog reset
        push    yh        ; SP
        push    yl
        push    tosh      ; TOS
        push    tosl
        push    ph        ; P
        push    pl
        movw    zl, upl
        cli
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
        sei
        ret


        fdw     OPERATOR_L
ICOMMA_L:
        .byte     NFA|2
        .ascii  "i,"
        .align  1
ICOMMA:
        call    IHERE
        rcall   STORE
        call    CELL
        jmp     IALLOT


;   IHERE ! 1 CHARS IALLOT ;
        fdw     ICOMMA_L
ICCOMMA_L:
        .byte     NFA|3
        .ascii  "ic,"
        .align  1
ICCOMMA:
        call    IHERE
        rcall   CSTORE
        call    ONE
        jmp     IALLOT

L_DOTBASE:
        .global L_DOTBASE
        .byte      NFA|1
        .ascii  " "
        .align  1
DOTBASE:
        .global DOTBASE
        call    BASE
        call    FETCH
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
;********************************************************************
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

; M? -- caddr count    current data space string
;        dw      L_DOTBASE
L_MEMQ:
        .byte     NFA|1,0x20
        .align  1
MEMQ:
        call    CSE_
        call    DOLIT
        fdw     MEMQADDR_N
        call    PLUS
        call    FETCH_A
        call    CFETCHPP
        call    DOLIT
        .word     NFAmask
        jmp     AND_
; *******************************************************************
ISTORERR0:
        m_dup
        lds   tosl, iaddrl
        lds   tosh, iaddrh
ISTORERR:
        call   DOTS
        call    XSQUOTE
        .byte     3
        .ascii  "AD?"
        .align  1
        call    TYPE
        jmp    ABORT
        
IWRITE_FC_START:
#if OPERATOR_UART == 0
#if U0FC_TYPE == 1
        rcall   DOLIT
        .word     XOFF
        call    EMIT
#endif
#if U0FC_TYPE == 2
        m_sbi    U0RTS_PORT, U0RTS_BIT
#endif
#else
#if U1FC_TYPE == 1
        rcall   DOLIT
        .word     XOFF
        call    EMIT
#endif
#if U1FC_TYPE == 2
        m_sbi    U1RTS_PORT, U1RTS_BIT
#endif
#endif
#if ((U0FC_TYPE == 1) || (U1FC_TYPE == 1))
        call   DOLIT
        .word  10
        call   MS
#endif
        ret

ISTORE_FC_END:
#if OPERATOR_UART == 0
#if U0FC_TYPE == 1
        rcall   DOLIT
        .word     XON
        call    EMIT
#endif
#if U0FC_TYPE == 2
        m_cbi    U0RTS_PORT, U0RTS_BIT
#endif
#else
#if U1FC_TYPE == 1
        rcall   DOLIT
        .word     XON
        call    EMIT
#endif
#if U1FC_TYPE == 2
        m_cbi    U1RTS_PORT, U1RTS_BIT
#endif
#endif
#if DEBUG_FLASH == 1
        call   DOLIT
        .word     'F'
        call    EMIT
#endif
        ret
;*******************************************************************
KERNEL_END:
;***********************************************************
.section .nrww, code, address(NRWW_START)
;*************************************************************
; In case of boot loader reset vector is active
; this jmp will ensure correct warm start anyway.
        jmp  WARM_0
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
        jmp  WARM_0
umslashmodstart:
        movw t4, tosl

        ld t3, Y+
        ld t6, Y+

        ld tosl, Y+
        ld tosh, Y+

; unsigned 32/16 -> 16/16 division
        ; set loop counter
        ldi t0, 0x10 ;6

umslashmod1:
        ; shift left, saving hi8 bit
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

ISTORERR3:
        jmp  ISTORERR
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
	      mov     t0, r_zero
        rjmp    XUPDATEBUF2
XUPDATEBUF:
#if FLASHEND > 0xffff
        sts     iaddru, t0
#endif
XUPDATEBUF2:
        sts     iaddrl, tosl
        sts     iaddrh, tosh

        cpi     t0, (FLASHEND>>16)
        brne    UPDATEBUF3
        cpi     tosh, hi8(NRWW_START&0xff00)  ; Don't allow kernel writes
        brcc    ISTORERR3
UPDATEBUF3:
        cpi     t0, 0
        brne    UPDATEBUF4
        cpi     tosh, hi8(KERNEL_END+0x100) ; Protect the  kernel
        brcs    ISTORERR3
UPDATEBUF4:
        lds     t0, iaddrl
        andi    t0, (~(PAGESIZEB-1)&0xff)
        cpse    t0, ibasel
        rjmp    IFILL_BUFFER
        lds     t0, iaddrh
        cpse    t0, ibaseh
        rjmp    IFILL_BUFFER
#if FLASHEND > 0xffff
        lds     t0, iaddru
        lds     t1, ibaseu
        cpse    t0, t1
        rjmp    IFILL_BUFFER
#endif
        ret

IFILL_BUFFER:
        call    IFLUSH
        lds     t0, iaddrl
        andi    t0, (~(PAGESIZEB-1)&0xff)
        mov     ibasel, t0
        lds     ibaseh, iaddrh
#if FLASHEND > 0xffff
        lds     t0, iaddru
        sts     ibaseu, t0
        m_out    RAMPZ, t0
#endif
IFILL_BUFFER_1:
        ldi     t0, PAGESIZEB&0xff ; 0x100 max PAGESIZEB
        movw    zl, ibasel
        ldi     xl, lo8(ibuf)
        ldi     xh, hi8(ibuf)
IFILL_BUFFER_2:
        m_lpm    t1
        st      x+, t1
        dec     t0
        brne    IFILL_BUFFER_2
#if FLASHEND > 0xffff
        m_out    RAMPZ, r_zero
#endif
        ret

IWRITE_BUFFER:
        call    IWRITE_FC_START
        ; Disable interrupts
        cli
#if FLASHEND > 0xffff
        lds     t0, ibaseu
        m_out   RAMPZ, t0
#else
        clr     t0
#endif
        movw    zl, ibasel

        cpi     t0, (FLASHEND>>16)
        brne    IWRITE_CHECK1
        cpi     zh, hi8(NRWW_START)  ; Don't allow kernel writes
        brcc    ISTORERR2
IWRITE_CHECK1:
        cpi     t0, 0
        brne    IWRITE_CHECK2
        cpi     zh, hi8(KERNEL_END+0x100) ; Protect the kernel
        brcs    ISTORERR2
IWRITE_CHECK2:

        ldi     t1, (1<<PGERS) | (1<<SPMEN) ; Page erase
        rcall   DO_SPM
        ldi     t1, (1<<RWWSRE) | (1<<SPMEN); re-enable the RWW section
        rcall   DO_SPM

        ; transfer data from RAM to Flash page buffer
        ldi     t0, lo8(PAGESIZEB/2);init loop variable
        ldi     xl, lo8(ibuf)
        ldi     xh, hi8(ibuf)
        push    r0
        push    r1
IWRITE_BUFFER1:
        ld      r0, x+
        ld      r1, x+
        ldi     t1, (1<<SPMEN)
        call    DO_SPM
        adiw    zl, 2
        subi    t0, 1
        brne    IWRITE_BUFFER1

        ; execute page write
        subi    zl, lo8(PAGESIZEB) ;restore pointer
        sbci    zh, hi8(PAGESIZEB)
        ldi     t1, (1<<PGWRT) | (1<<SPMEN)
        call    DO_SPM
        ; re-enable the RWW section
        rcall   IWRITE_BUFFER3

        ; read back and check, optional
        ldi     t0, lo8(PAGESIZEB);init loop variable
        subi    xl, lo8(PAGESIZEB) ;restore pointer
        sbci    xh, hi8(PAGESIZEB)
IWRITE_BUFFER2:
        m_lpm    r0
        ld      r1, x+
        cpse    r0, r1
        jmp     WARM_0     ; reset
        subi    t0, 1
        brne    IWRITE_BUFFER2
        pop     r1
        pop     r0
ISTORERR2:
        ser     t0
        mov     ibaseh, t0
#if FLASHEND > 0xffff
        sts     ibaseu, t0
        m_out    RAMPZ, r_zero
#endif
        cbr     FLAGS1, (1<<idirty)
        // reenable interrupts
        sei
        call    ISTORE_FC_END
        ret
        ; ret to RWW section
        ; verify that RWW section is safe to read
IWRITE_BUFFER3:
        m_in     t8, SPMCSR
        sbrs    t8, RWWSB ; If RWWSB is set, the RWW section is not ready yet
        ret
        ; re-enable the RWW section
        ldi     t1, (1<<RWWSRE) | (1<<SPMEN)
        call    DO_SPM
        rjmp    IWRITE_BUFFER3
DO_SPM:
        m_in    t8, SPMCSR
        sbrc    t8, SPMEN
        rjmp    DO_SPM       ; Wait for previous write to complete
        m_out   SPMCSR, t1
        spm
DO_SPM2:
        m_in    t8, SPMCSR
        sbrc    t8, SPMEN
        rjmp    DO_SPM2       ; Wait the current write to complete
        ret
.end
