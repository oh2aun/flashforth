\ *******************************************************************
\    Simple RTC for FlashForth 30F                                  *
\    Filename:      rtc_30.fth                                      *
\    Date:          31.1.2009                                       *
\    FlashForth:    30F4.4                                          *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************

-rtc
marker -rtc
hex ram

$010c con pr2
$0106 con tmr2
$0110 con t2con

$0084 con ifs0 $0006 con t2if
$008c con iec0 $0006 con t2ie

#14 con t2irq  \ Timer 2 interrupt vector number

ram
variable sec
variable mins
variable hour

\ Interrupt routine
: T2RtcIrq
  [i
     1 sec +!
     sec @ #59 > 
     if
       0 sec ! 1 mins +!
       mins @ #59 >
       if
         0 mins ! 1 hour +!
         hour @ #23 >
         if
           0 hour !
         then
       then
     then
     [ t2if ifs0 bclr, ]
  i]
;i

\ Store the interrupt word address in the
\ Alternate Interrupt Vector Table

' T2RtcIrq t2irq   \ Must be on separate lines
int!               \ with this !

\ #27 is a tune value dependent on the exact clock frequency
: T2RtcInit ( -- )
  \ Calculate one second counter value
  [ Fcy #1000 #256 u*/mod nip #27 + literal ] pr2 !   
  %1000000000110000 t2con ! \ / 256 prescaler
  aivt
  [ t2ie iec0 bset, ]
;

\ Display the current time
: time ( -- )
  decimal hour @ u. mins @ u. sec  @ u. ;

\ Initialise the RTC
T2RtcInit

