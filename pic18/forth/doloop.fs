\ *********************************************************************
\                                                                     *
\    Filename:      doloop.txt                                        *
\    Date:          01.01.2018                                        *
\    File Version:  5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-doloop
marker -doloop

: compileonly $10 shb ; \ Set Header bit for compile only word

$f040 constant ind   \ Hardcoded index register address

: (do)
  [ $50fd i, $6eec i, $50fe i, $6eec i, ] \ R@
  dup    @  2+ 2+                \ leave to after (unloop)
  [ $50ed i, $6efe i, $50ed i, $6efd i, ] \ R!
  2+
  [ 5 i, $50ed i, $6efe i, $50ed i, $6efd i, ] \ >R
  ind @ >r                   \ oldindex
  swap $8000 swap - dup >r   \ xfactor
  + ind !
; compileonly
  
: (?do)
  2dup xor
  if
    [ '  (do) ] again  \ branch to (do) 
  then
  2drop
  [ $50fd i, $6eec i, $50fe i, $6eec i, 6 i, ] \ R>
  @ 2+ 2+ execute     \ jump to after (unloop)
; compileonly 

: (loop)
  [ $0e00 i, ]  \ movlw 0
  [ $2a40 i, ]  \ incf, ind, F, A
  [ $2241 i, ]  \ addwfc ind+1, F, A
; compileonly

: (+loop)
  [ $cfed i, ]  \ movff   Sminus, Tp
  [ $ffe1 i, ]  \ 
  [ $50ed i, ]  \ movf    Sminus, W, A
  [ $2640 i, ]  \ addwf   ind, F, A
  [ $50e1 i, ]  \ movf    Tp, W, A
  [ $2241 i, ]  \ addwfc  ind+1, F, A
; compileonly

:noname
  postpone begin
  flash 2 allot ram
  postpone begin
; dup

: do
  postpone (do) [ cf, ]
; immediate compileonly

: ?do
  postpone (?do) [ cf, ]
; immediate compileonly

: (unloop)  ( R: leave r: oldindex xfactor -- )
  [ $50fd i, $6eec i, $50fe i, $6eec i, $6 i, ] \ R>
  [ $6 i, ]     \ pop     leave 
  rdrop r> ind !
  execute
; compileonly

: unloop
  postpone (unloop)
; immediate compileonly

: leave
  rdrop r> ind ! exit
; compileonly

:noname
  $e4 until,          \ $e4=overflow
  flash here swap ! ram
  ['] (unloop) call,
; dup

: loop
  postpone (loop) [ cf, ]
; immediate compileonly

: +loop
  postpone (+loop) [ cf, ]
; immediate compileonly

: i ( -- n ) ind @ r@ - ;

: j ( -- j  R: j-xfactor j-index i-xfactor -- the same )  
  r> r>
  dup r@ - >a
  >r >r
  a>
; compileonly


