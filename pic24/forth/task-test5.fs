\ *******************************************************************
\                                                                   *
\    Filename:      task-test2.txt                                  *
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
\ Registers for dsPIC33FJ128GP802. Change if needed.
$032c constant adpcfg
$02cc constant latb
$02ca constant portb
$02c8 constant trisb
ram variable delay
: led9off  %0000.0010.0000.0000 portb mclr ;
: led9on   %0000.0010.0000.0000 portb mset ;
: led10off %0000.0100.0000.0000 portb mclr ;
: led10on  %0000.0100.0000.0000 portb mset ;

40 30 30 0 task: t1
: tloop 
  $3f adpcfg c!
  #250 delay !
  %0000.0110.0000.0000 trisb mclr 
  begin 
    delay @ ms 
    %0000.0010.0000.0000 portb mtst
    if
      led9off
      led10on
    else 
      led9on
      led10off
    then
  again
;

: t1go ['] tloop t1 tinit t1 run ;

' t1go is turnkey
warm

