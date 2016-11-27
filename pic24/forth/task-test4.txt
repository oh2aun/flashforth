\ *******************************************************************
\                                                                   *
\    Filename:      task-test4.txt                                  *
\    Date:          25.11.2016                                      *
\    FF Version:    5.0                                             *
\    MCU:           PIC 24 33                                       *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
single
-t1
marker -t1
ram hex
\ Registers for dsPIC33FJ128MC802 on microstick II.
$032c constant adpcfg
$02cc constant latb
$02ca constant portb
$02c8 constant trisb
ram variable delay
: led14off  %0100.0000.0000.0000 portb mclr ;
: led14on   %0100.0000.0000.0000 portb mset ;
: led15off  %1000.0000.0000.0000 portb mclr ;
: led15on   %1000.0000.0000.0000 portb mset ;

40 30 30 0 task: t1
: tloop 
  $3f adpcfg c!
  #250 delay !
  %1100.0000.0000.0000 trisb mclr
  begin 
    delay @ ms 
    %0100.0000.0000.0000 trisb mtst
    if
      led14off
      led15on
    else 
      led14on
      led15off
    then
  again
;

: t1go ['] tloop t1 tinit t1 run ;

' t1go is turnkey
warm

