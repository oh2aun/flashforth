\ *********************************************************************
\    Interrupts example for FlashForth                                *
\    Filename:      irqAtmega328.txt                                  *
\    Date:          10.11.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega328                                         *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Disable interrupt before removing the interrupt code
irqOvf2Dis
-irqOvf2
marker -irqOvf2
\ Timer 2 definitions from m328pdef.inc
$b1 constant tccr2b
$70 constant timsk2
#10 constant ovf2Ivec

\ Counter for timer overflows
variable counter

\ The interrupt routine
: t2OverflowIsr
  1 counter +!
;i

: irqOvf2Init
  \ Store the interrupt vector
  ['] t2OverflowIsr ovf2Ivec int!
  \ Activate counter 2
  3 tccr2b c!
  \ Activate timer2 overflow interrupt
  1 timsk2 mset
;
: irqOvf2Dis
  1 timsk2 mclr
;

\ irqOvf2Init

