\ *******************************************************************
\                                                                   *
\    Filename:      task-test.txt                                   *
\    Date:          06.01.2014                                      *
\    FF Version:    5.0                                             *
\    MCU:           Atmega 128(Olimex AVR-MT-128)                   *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ Demo for the Olimex AVR-MT-128. Switches relay and blinks led in 
\ background task.
single
-task1
marker -task1
ram hex
\ Registers for Atmega 128. Change if needed
$003b constant porta
$003a constant ddra
$0039 constant pina
$40 constant pin6
ram variable delay
: ledoff  pin6 porta mclr ;
: ledon   pin6 porta mset ;

0 18 20 0 task: task1
: taskloop 
  400 delay !
  $40 ddra mset  \  Output 
  begin 
    delay @ ms 
    pin6 porta mtst
    if
       ledoff
    else
       ledon
    then
  again
;

: t1go ['] taskloop task1 tinit task1 run ;

\ ' t1go is turnkey
\ warm
