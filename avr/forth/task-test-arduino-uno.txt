\ *******************************************************************
\                                                                   *
\    Filename:      task-test-arduino-uno.txt                       *
\    Date:          01.10.2013                                      *
\    FF Version:    5.0                                             *
\    MCU:           ArduinoUnoR3 ATmega328P                         *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ Demo for the ArduinoUnoR3. Blinks yellow led in background task.
single
-task1
marker -task1
ram hex
\ Registers for Atmega 328p.
$0025 constant portb
$0024 constant ddrb
$0023 constant pinb
$20 constant pin5
ram variable delay
: ledoff  pin5 portb mclr ;
: ledon   pin5 portb mset ;

0 18 20 0 task: task1
: taskloop 
  $100 delay !
  pin5 ddrb mset  \  Output 
  begin 
    delay @ ms 
    pin5 portb mtst
    if
       ledoff
    else
       ledon
    then
  again
;

: t1go ['] taskloop task1 tinit task1 run ;

' t1go is turnkey
warm

