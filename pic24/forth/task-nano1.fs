\ *******************************************************************
\                                                                   *
\    Filename:      task-nano1.fs                                   *
\    Date:          22.11.2023                                      *
\    FF Version:    5.0                                             *
\    MCU:           Curiosity nano pic24fj64gu205                   *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
single
-t1
marker -t1
ram decimal
$068e constant latc
$068a constant trisc
2 latc 2constant led0
ram #250 value delay
: led0off  led0 mclr ;
: led0on   led0 mset ;

40 30 30 0 task: t1
: tloop 
  #250 to delay
  2 trisc mclr
  led0off
  begin 
    delay ms 
    led0 mtst
    if
      led0off
    else 
      led0on
    then
  again
;

: t1go ['] tloop t1 tinit t1 run ;

' t1go is turnkey
warm

