\ *******************************************************************
\                                                                   *
\    Filename:      task-test3.txt                                  *
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
\ Registers for dsPIC33EP256GP502 on MicroStick2.
$0e14 constant latb
$0e12 constant portb
$0e10 constant trisb
ram variable delay
: led14off  %0100.0000.0000.0000 portb mclr ;
: led14on   %0100.0000.0000.0000 portb mset ;
: led13off  %0010.0000.0000.0000 portb mclr ;
: led13on   %0010.0000.0000.0000 portb mset ;

40 30 30 0 task: t1
: tloop 
  #100 delay !
  %0110.0000.0000.0000 trisb mclr 
  begin 
    delay @ ms 
    %0100.0000.0000.0000 portb mtst
    if
      led14off
      led13on
    else 
      led14on
      led13off
    then
  again
;

: t1go ['] tloop t1 tinit t1 run ;

' t1go is turnkey
warm

