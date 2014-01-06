\ *********************************************************************
\    Interrupts example for FlashForth                                *
\    Filename:      irqAtmega2560.txt                                 *
\    Date:          04.10.2013                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega2560                                        *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Disable interrupt before removing the interrupt code
irqOvf3Dis
-irqOvf3
marker -irqOvf3
\ Timer 3 definitions from m2560def.inc
$91 constant tccr3b
$71 constant timsk3
#36 constant ovf3Ivec

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
  1 timsk3 mset
;
: irqOvf3Dis
  1 timsk3 mclr
;

irqOvf3Init

counter @ .
#1000 ms
counter @ .
