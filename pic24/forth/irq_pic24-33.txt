\ *******************************************************************
\    Simple RTC for FlashForth PIC24 PIC33                          *
\    Filename:      irq_pic24-33.txt                                *
\    Date:          05.01.2014                                      *
\    FlashForth:    FF5.0                                           *
\    MCU:           PIC 24 33                                       *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ EXAMPLE OF HOW TO SET UP AN INTERRUPT ROUTINE

\ NOTE1:
\ All user defined interrupts must be disabled
\ before the  switch to IVT is made.
\ Otherwise the device will reset because
\ the unused IVT vectors point to the reset instruction.
\ NOTE2:
\ In case the interrupt routine is located on a flash page
\ which will be reprogrammed while the interrupt is enabled,
\ the CPU will crash on a address error.
\ So it is recommended to fully program the flash page
\ before user defined interrupts are enabled.
\ Or alternatively advance the flash dp to another flash page
\ before programming more into device. E.g. $400 flash allot 

\ Disable the alternate interrupts and switch back to IVT.
iec0 t2ie bclr ivt

-rtc
marker -rtc
hex ram

$010c constant pr2
$0106 constant tmr2
$0110 constant t2con
$0770 constant pmd1  $c constant t2md

$0084 constant ifs0 $0007 constant t2if
$0094 constant iec0 $0007 constant t2ie

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
     ifs0 t2if bclr
  i]
;i


: T2RtcInit ( -- )
  \ Store the interrupt word address in the
  \ Alternate Interrupt Vector Table in ram
  ['] T2RtcIrq #15 int!
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
iflush
\ Initialise the RTC
T2RtcInit
\ ' T2RtcInit is turnkey

\ warm
