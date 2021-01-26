\ *******************************************************************
\    Simple RTC for FlashForth PIC30F                               *
\    Filename:      irq_pic30.txt                                   *
\    Date:          05.01.2014                                      *
\    FlashForth:    FF5.0                                           *
\    MCU:           PIC 30                                          *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ EXAMPLE OF HOW TO SET UP AN INTERRUPT ROUTINE
\ Disable the alternate interrupts and switch back to IVT.
\ All user defined interrupts must be disabled
\ before the  switch to IVT is made.
\ Otherwise the device will reset because
\ the unused IVT vectors point to the reset instruction.

iec0 t2ie bclr ivt

-rtc
marker -rtc
hex ram

$010c constant pr2
$0106 constant tmr2
$0110 constant t2con
$0770 constant pmd1  $c constant t2md

$0084 constant ifs0 $0006 constant t2if
$008c constant iec0 $0006 constant t2ie

#14 constant t2irq  \ Timer 2 interrupt vector number

ram
variable hsec 
variable sec
variable mins
variable hour

\ Interrupt routine
: T2RtcIrq
  [i
     1 hsec +! hsec @ #4 u>
     if 
       0 hsec ! 1 sec +! sec @ #59 u> 
       if
         0 sec ! 1 mins +! mins @ #59 u>
         if
           0 mins ! 1 hour +! hour @ #23 u>
           if
             0 hour !
           then
         then
       then
     then
     ifs0 t2if  bclr
  i]
;i

\ Store the interrupt word address in the
\ Alternate Interrupt Vector Table
' T2RtcIrq t2irq int!

: T2RtcInit ( -- )
  pmd1 t2md bclr
  \ Calculate one second counter value
  [ Fcy #200 #256 u*/mod nip literal ] pr2 !   
  %1000000000110000 t2con ! \ / 256 prescaler
  aivt
  iec0 t2ie bset
;

\ Display the current time
: time ( -- )
  decimal hour @ u. mins @ u. sec  @ u. ;

\ Initialise the RTC
' T2RtcInit is turnkey

warm

