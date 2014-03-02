\ *******************************************************************
\                                                                   *
\    Filename:      task2-test.txt                                  *
\    Date:          01.03.2014                                      *
\    FF Version:    5.0                                             *
\    MCU:           Atmega 328(Olimex AVR-P28)                      *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ Demo for the Olimex AVR-P28 with Atmega328P. Blinks led in 
\ background task.
single
-task2
marker -task2
ram hex
\ Registers for Atmega 328P. Change if needed
$0028 constant portc
$0027 constant ddrc
$20 constant pin5

ram variable delay

: ledoff  pin5 portc mset ;
: ledon   pin5 portc mclr ;

0 18 20 0 task: task2
: task2loop 
  100 delay !
  pin5 ddrc mset  \  Output
  begin
    delay @ ms 
    pin5 portc mtst
    if
       ledon
    else
       ledoff
    then
  again
;

: t2go ['] task2loop task2 tinit task2 run ;

' t2go is turnkey
warm

