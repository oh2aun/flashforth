\ *********************************************************************
\                                                                     *
\    Filename:      3blink.txt                                        *
\    Date:          27.11.2016                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\
\ Test tasks and the character bit manipulation words
\
single
-3blink
marker -3blink
ram hex

$ff82 constant portc
$ff94 constant trisc
$2 user bitmask    \ The bitmask
$4 user delay      \ The delay time in milliseconds

: taskloop
  bitmask c@ trisc mclr
  begin
    delay @ ms
    bitmask c@ portc mset
    delay @ ms
    bitmask c@ portc mclr
  again
;

flash $0 $10 $10 $4 task: tblink0

: tblink0-init
  ['] taskloop tblink0 tinit
  %1 tblink0 bitmask his !
  $80 tblink0 delay his !
;
flash $0 $10 $10 $4 task: tblink1
: tblink1-init
  ['] taskloop tblink1 tinit
  %10 tblink1 bitmask his !
  $90 tblink1 delay his !
;
flash $0 $10 $10 $4 task: tblink2
: tblink2-init
  ['] taskloop tblink2 tinit
  %100 tblink2 bitmask his !
  $a0 tblink2 delay his !
;

: 3blink
  tblink0-init tblink0 run 
  tblink1-init tblink1 run 
  tblink2-init tblink2 run 
; 

\ Make 3blink start after reset.
' 3blink is turnkey
 warm
