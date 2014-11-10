\ *********************************************************************
\    Interrupts example for FlashForth                                *
\    Filename:      irq.txt                                           *
\    Date:          04.10.2013                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega128                                         *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Disable interrupt before removing the interrupt code
irqOvf3Dis
-irqOvf3
marker -irqOvf3
\ Timer 3 definitions from m128def.inc
$8a constant tccr3b
$7d constant etimsk
#30 constant ovf3Ivec

\ Counter for timer overflows
variable counter

\ The interrupt routine
: t3OverflowIsr
  1 counter +!
;i

: irqOvf3Init
  \ Store the interrupt vector
  ['] t3OverflowIsr ovf3Ivec int!
  \ Activate counter 3
  1 tccr3b mset
  \ Activate timer3 overflow interrupt
  4 etimsk mset
;
: irqOvf3Dis
  4 etimsk mclr
;

irqOvf3Init

counter @ u.
#1000 ms
counter @ u.
