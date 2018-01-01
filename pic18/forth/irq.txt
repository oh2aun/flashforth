\ *********************************************************************
\    Interrupts example for FlashForth                                *
\    Filename:      irq.txt                                           *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ NOTE: Always deactivate user interrupts before
\ the interrupt word is removed.
\ You must also clear any related interrupt enable bits 
\ and interrupt flags before zeroing the interrupt vector
\ NOTE: Interrupt vector 0 is the high prority interrupt vector.
\ NOTE: The low priority interrupt vector is not implemented.

0 0 int!
-uirq
marker -uirq
ram hex

$28 as3 incf,           ( f d a -- )  
$48 as3 infsnz,         ( f d a -- )
: lfsr,    ( k f -- )
  4 lshift over 8 rshift $f and or $ee00 or i, $ff and $f000 or i, ;  
1 constant f,     \ Destination File

1     constant tp
$ffe6 constant (tp+)  \ Treg (FSR1) is interrupt safe
ram variable icnt

\ Interrupt routine written in assembly
: irq_asm
  [ icnt tp lfsr,     ]
  [ (tp+) f, a, infsnz, ]
  [ (tp+) f, a, incf,   ]
;i

' irq_asm 0 int!

icnt @ icnt @ u. u.
\
\ Interrupt routine written in Forth
\
ram variable icnt1

: irq_forth
  [i
    icnt1 @ 1+ 
    icnt1 !
  i] 
;i

' irq_forth 0 int!

\ Install the interrupt for warm start
: init ['] irq_forth 0 int! ; 
' init is turnkey

icnt1 @ icnt1 @ u. u.
