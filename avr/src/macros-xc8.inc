#include <xc.h>
; Register definitions
  .equ upl, 2         ; not in interrupt 
  .equ uph, 3         ; not in interrupt
  .equ r_zero, 5      ; read only zero
  .equ r_one, 6       ; read only one
  .equ r_two, 7       ; read only two
  .equ t8, 8          ; Not in interrupt
  .equ wflags , 9     ; not in interrupt

  .equ loadreg0, 4    ;
  .equ loadreg1, 12
  .equ loadreg2, 13


  .equ ibasel, 10       ; Not in interrupt
  .equ ibaseh, 11       ; Not in interrupt
  .equ ms_count , 14    ; Not in interrupt
  .equ ms_count1, 15    ; Not in interrupt
  .equ t0, 16
  .equ t1, 17
  .equ t2, 0            ; Not in interrupt
  .equ t3, 1            ; Not in interrupt

  .equ al, 18
  .equ ah, 19
  .equ pl, 20         ; P Register and DO..LOOP INDEX variable
  .equ ph, 21

  .equ FLAGS1, 22     ; Not in interrupt
  .equ FLAGS2, 23     ; Not in interrupt
  .equ tosl, 24
  .equ tosh, 25
;  xl, r26
;  xh, r27
;  yl, r28  ; StackPointer Ylo
;  yh, r29  ; StackPointer Yhi
;  zl, r30
;  zh, r31
  .equ t4, 26
  .equ t5, 27
  .equ t6, 30
  .equ t7, 31
  .equ spl, 0x3d
  .equ sph, 0x3e

; Macros
.macro m_drop
    ld tosl, Y+
    ld tosh, Y+
.endm

.macro m_dup
    st -y, tosh
    st -y, tosl
.endm

.macro m_in reg, addr
.if (\addr < 0x20)
  in _SFR_IO_ADDR(\reg), \addr
.else
  lds \reg, \addr
.endif
.endm

.macro m_out addr, reg
.if (\addr < 0x20)
  out _SFR_IO_ADDR(\addr),\reg
.else
  sts \addr,\reg
.endif
.endm

.macro m_sbi addr, bit
.if (\addr < 0x40)
  sbi _SFR_IO_ADDR(\addr),\bit
.else
  m_in r16, \addr
  ori r16, (1<<\bit)
  m_out \addr, r16
.endif 
.endm

.macro m_cbi addr, bit
.if (\addr < 0x40)
  cbi _SFR_IO_ADDR(\addr),\bit
.else
  m_in r16, \addr
  andi r16, ~(1<<\bit)
  m_out \addr,r16
.endif
.endm

.macro m_lpm reg
.if (FLASHEND < 0x8000)
        lpm \reg, z+
.else
        elpm \reg, z+
.endif
.endm

.macro sub_pflash_z
.if (PFLASH > 0)
        subi    zh, hi8(PFLASH)
.endif
.endm

.macro add_pflash_z
.if (PFLASH > 0)
        subi    zh, hi8(0x10000-PFLASH)
.endif
.endm


.macro sub_pflash_tos
.if (PFLASH > 0)
        subi    tosh, hi8(PFLASH)
.endif
.endm

.macro add_pflash_tos
.if (PFLASH > 0)
        subi    tosh, hi8(0x10000-PFLASH)
.endif        
.endm

.macro fdw addr
	.word (\addr+PFLASH)
.endm

.macro m_pop_zh
.if (FLASHEND > 0x1ffff)
        pop     zh
.endif
.endm

.macro m_pop_xh
.if (FLASHEND > 0x1ffff)
        pop     xh
.endif
.endm

.macro m_pop_t0
.if (FLASHEND > 0x1ffff)
        pop     t0
.endif
.endm

.macro m_push_t0
.if (FLASHEND > 0x1ffff)
        push    t0
.endif
.endm

.macro m_ijmp
.if (FLASHEND > 0x1ffff)
        eijmp
.else
        ijmp
.endif
.endm

.macro m_header name, flags
        fdw     9b
.equ name_len, 2f-1f
9:
        .byte   NFA|\flags|name_len
1:
        .ascii  "\name"
2:
        .align  1
.endm

; Macros for constants
.macro m_const value, name, flags
        m_header \name, \flags
        m_dup
        ldi     tosl,lo8(\value)
        ldi     tosh,hi8(\value)
        ret
.endm



; Symbol naming compatilibity
; UART1 symbols for Atmega32u4
#if defined(XXU4)
.equ UCSR0A,UCSR1A
.equ UDR0_,UDR1
.equ UCSR0B,UCSR1B
.equ UCSR0C,UCSR1C
.equ URXC0addr,URXC1addr
.equ RXEN0,RXEN1
.equ TXEN0,TXEN1
.equ RXCIE0,RXCIE1
.equ UCSZ00,UCSZ10
.equ USBS0,USBS1
.equ UBRR0H,UBRR1H
.equ UBRR0L,UBRR1L
.equ URSEL_,0x00
.equ U2X0, U2X1
#else
; UART0 symbols for Atmega32
#ifndef UCSR0A
.equ UCSR0A,UCSRA
.equ UDR0_,UDR
.equ UCSR0B,UCSRB
.equ UCSR0C,UCSRC
.equ RXEN0,RXEN
.equ TXEN0,TXEN
.equ RXCIE0,RXCIE
.equ UCSZ00,UCSZ0
.equ USBS0,USBS
.equ UBRR0H,UBRRH
.equ UBRR0L,UBRRL
.equ URSEL_,0x80
#else
.equ UDR0_,UDR0
.equ URSEL_,0
#endif
#endif
