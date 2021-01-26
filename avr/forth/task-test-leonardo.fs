\ *******************************************************************
\                                                                   *
\    Filename:      task-test-leonardo.txt                          *
\    Date:          05.09.2020                                      *
\    FF Version:    5.0                                             *
\    MCU:           Arduino Leonardo ATmega32u4                     *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ Demo for the Arduino Leonardo. Blinks yellow led in background task.
single
-task1
marker -task1
ram hex
\ Registers for Atmega 32u4.
$0028 constant portc
$0027 constant ddrc
$0026 constant pinc
$80 constant pin7
ram variable delay
: ledoff  pin7 portc mclr ;
: ledon   pin7 portc mset ;

0 $20 $20 0 task: task1
: taskloop 
  $100 delay !
  pin7 ddrc mset  \  Output 
  begin 
    delay @ ms 
    pin7 portc mtst
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

