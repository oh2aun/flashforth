\ *******************************************************************
\                                                                   *
\    Filename:      task-nano-gu205.fs                              *
\    Date:          26.08.2025                                      *
\    FF Version:    5.0                                             *
\    MCU:           PIC2464GU205 nano board                         *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
single
-t1
marker -t1
ram decimal
\ Registers and LED for dsPIC33FJ128GB202 on Scamp 0.
$0002 latc 2constant led
ram #250 value delay
: led-off  led mclr ;
: led-on   led mset ;

40 30 30 0 task: t1
: tloop 
  #250 to delay
  2 trisc mclr
  led-off
  begin 
    delay ms 
    led mtst
    if
      led-off
    else 
      led-on
    then
  again
;

: t1go ['] tloop t1 tinit t1 run ;

' t1go is turnkey
warm

