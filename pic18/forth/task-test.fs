\ *********************************************************************
\                                                                     *
\    Filename:      task-test.txt                                     *
\    Date:          28.02.2011                                        *
\    FF Version:    5.0                                               *
\    MCU            PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\
\ Test tasks and the character bit manipulation words
\
single
-lblink
marker -lblink
ram hex

$ff82 constant portc
$ff94 constant trisc
$2 user bitmask    \ The bitmask
$4 user delay      \ The delay time in milliseconds

: lblink
  bitmask c@ trisc mclr
  begin
    delay @ ms
    bitmask c@ portc mset 
    delay @ ms
    pause
    bitmask c@ portc mclr
  again
;

$0 $10 $20 $4 task: tblink

: setdelay tblink delay his ! ;

: tblink-init
  ['] lblink tblink tinit
  $4 tblink bitmask his !
  $30 setdelay
;
: blink1 tblink-init tblink run ; 

 blink1

 tasks


\ Make blink1 start after reset.
 ' blink1 is turnkey

cr decimal load . cr

