\ *********************************************************************
\                                                                     *
\    Filename:      3blink14k50.txt                                   *
\    Date:          15.09.2019                                        *
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

$ff8b constant latc
$ff94 constant trisc
$2 user bitmask    \ The bitmask
$4 user delay      \ The delay time in milliseconds

: taskloop
  bitmask c@ trisc mclr
  begin
    delay @ ms
    bitmask c@ latc mset
    delay @ ms
    bitmask c@ latc mclr
  again
;

flash $0 $10 $10 $4 task: taskrc7

: taskrc7-init
  ['] taskloop taskrc7 tinit
  %1000.0000 taskrc7 bitmask his !
  #200 taskrc7 delay his !
;
flash $0 $10 $10 $4 task: taskrc6
: taskrc6-init
  ['] taskloop taskrc6 tinit
  %0100.0000 taskrc6 bitmask his !
  #210 taskrc6 delay his !
;
flash $0 $10 $10 $4 task: taskrc3
: taskrc3-init
  ['] taskloop taskrc3 tinit
  %0000.1000 taskrc3 bitmask his !
  #220 taskrc3 delay his !
;

: 3blink
  taskrc3-init taskrc3 run
  taskrc6-init taskrc6 run
  taskrc7-init taskrc7 run
; 

\ Make 3blink start after reset.
' 3blink is turnkey
warm
