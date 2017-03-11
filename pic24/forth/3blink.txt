\ *********************************************************************
\                                                                     *
\    Filename:      3blink.txt                                        *
\    Date:          27.11.2016                                        *
\    FF Version:    5.0 PIC24                                         *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\
\ Test tasks and the character bit manipulation words
\
-3blink
marker -3blink
ram hex

$2 user bitmask    \ The bitmask
$4 user delay      \ The delay time in milliseconds

: taskloop
  bitmask @ trisb mclr
  begin
    delay @ ms
    bitmask @ inline portb inline mset
    delay @ ms
    bitmask @ inline portb inline mclr
  again
;

flash $0 $30 $30 $4 task: tblink0
: tblink0-init
  ['] taskloop tblink0 tinit
  $2000 tblink0 bitmask his !
  $80 tblink0 delay his !
;

flash $0 $30 $30 $4 task: tblink1
: tblink1-init
  ['] taskloop tblink1 tinit
  $4000 tblink1 bitmask his !
  $90 tblink1 delay his !
;
flash $0 $30 $30 $4 task: tblink2
: tblink2-init
  ['] taskloop tblink2 tinit
  $8000 tblink2 bitmask his !
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
