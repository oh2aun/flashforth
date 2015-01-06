\ *********************************************************************
\                                                                     *
\    Filename:      8blink.txt                                        *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\
\ Test tasks and the character bit manipulation words
\
-8blink
marker -8blink
ram hex

$ff81 constant portb
$ff93 constant trisb
$2 user bitmask    \ The bitmask
$4 user delay      \ The delay time in milliseconds

: lblink
  bitmask c@ trisb mclr
  begin
    delay @ ms
    bitmask c@ portb mset 
    delay @ ms
    bitmask c@ portb mclr
  again
;

flash $0 $10 $10 $4 task: tblink0

: tblink0-init
  ['] lblink tblink0 tinit
  %1 tblink0 bitmask his !
  $80 tblink0 delay his !
;
flash $0 $10 $10 $4 task: tblink1
: tblink1-init
  ['] lblink tblink1 tinit
  %10 tblink1 bitmask his !
  $90 tblink1 delay his !
;
flash $0 $10 $10 $4 task: tblink2
: tblink2-init
  ['] lblink tblink2 tinit
  %100 tblink2 bitmask his !
  $a0 tblink2 delay his !
;
flash $0 $10 $10 $4 task: tblink3
: tblink3-init
  ['] lblink tblink3 tinit
  %1000 tblink3 bitmask his !
  $b0 tblink3 delay his !
;
flash $0 $10 $10 $4 task: tblink4
: tblink4-init
  ['] lblink tblink4 tinit
  %10000 tblink4 bitmask his !
  $c0 tblink4 delay his !
;
flash $0 $10 $10 $4 task: tblink5
: tblink5-init
  ['] lblink tblink5 tinit
  %100000 tblink5 bitmask his !
  $d0 tblink5 delay his !
;
flash $0 $10 $10 $4 task: tblink6
: tblink6-init
  ['] lblink tblink6 tinit
  %1000000 tblink6 bitmask his !
  $e0 tblink6 delay his !
;
flash $0 $10 $10 $4 task: tblink7
: tblink7-init
  ['] lblink tblink7 tinit
  %10000000 tblink7 bitmask his !
  $100 tblink7 delay his !
;


: 8blink 
  tblink0-init tblink0 run 
  tblink1-init tblink1 run 
  tblink2-init tblink2 run 
  tblink3-init tblink3 run 
  tblink4-init tblink4 run 
  tblink5-init tblink5 run 
  tblink6-init tblink6 run 
  tblink7-init tblink7 run 
; 


\ Make 8blink start after reset.
' 8blink is turnkey
warm
