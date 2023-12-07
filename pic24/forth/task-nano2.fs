\ *******************************************************************
\                                                                   *
\    Filename:      task-nano2.txt                                  *
\    Date:          03.12.2023                                      *
\    FF Version:    5.0                                             *
\    MCU:           Curiosity Nano dsPIC33CK64MC105                 *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
single
-t1
marker -t1
ram hex

\ LED on port D pin 10 Curiosity Nano dsPIC33CK64MC105
: ledoff  %0000.0100.0000.0000 latd mclr ;
: ledon   %0000.0100.0000.0000 latd mset ;
: ledon?  %0000.0100.0000.0000 latd mtst ;

40 30 30 0 task: t1
: tloop 
  %0000.0100.0000.0000 trisd mclr 
  begin 
    ticks 7 rshift ms 
    ledon?
    if   ledoff
    else ledon
    then
  again
;

: t1go ['] tloop t1 tinit t1 run ;
' t1go is turnkey
warm

