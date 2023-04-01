\ *******************************************************************
\                                                                   *
\    Filename:      task-test4.txt                                  *
\    Date:          18.02.2023                                      *
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
ram decimal
\ Registers and LED for dsPIC33FJ128GB202 on Scamp 1.
$0184 constant lata inlined
$0180 constant trisa inlined
$0004 lata 2constant led2 inlined
ram #250 value delay
: led2off  led2 inline mclr ; inlined
: led2on   led2 inline mset ; inlined

40 30 30 0 task: t1
: tloop 
  #250 to delay
  led2off
  begin 
    delay ms 
    led2 inline mtst
    if
      led2off
    else 
      led2on
    then
  again
;

: t1go ['] tloop t1 tinit t1 run ;

' t1go is turnkey
warm

