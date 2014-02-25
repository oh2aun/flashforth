\ *******************************************************************
\                                                                   *
\    Filename:      task-test-arduino-uno.txt                       *
\    Date:          02.10.2013                                      *
\    FF Version:    5.0                                             *
\    MCU:           ArduinoMega2560R3                               *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ Demo for the ArduinoMega2560R3. Blinks red led in background task.
single
-task1
marker -task1
ram hex
\ Registers for Atmega 2560.
$0025 constant portb
$0024 constant ddrb
$0023 constant pinb
$80 constant pin7
ram variable delay
: ledoff  pin7 portb mclr ;
: ledon   pin7 portb mset ;

0 18 20 0 task: task1
: taskloop 
  $100 delay !
  pin7 ddrb mset  \  Output 
  begin 
    delay @ ms 
    pin7 portb mtst
    if
       ledoff
    else
       ledon
    then
  again
;

: t1go 
  ['] taskloop task1 tinit
  task1 run
;

' t1go is turnkey
warm

