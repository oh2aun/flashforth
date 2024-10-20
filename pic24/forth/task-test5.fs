\ *******************************************************************
\                                                                   *
\    Filename:      task-test5.txt                                  *
\    Date:          07.04.2023                                      *
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
\ Registers for PIC24F16KL402 on Microstick for 3V PIC24 K series.
$02c4 constant lata
$02c2 constant porta
$02c0 constant trisa
: ledoff  1 lata mclr ;
: ledon   1 lata mset ;
: ledon? 1 lata mtst ;

40 30 30 0 task: t1
: tloop 
  1 trisa mclr 
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

