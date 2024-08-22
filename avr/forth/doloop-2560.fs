\ *********************************************************************
\                                                                     *
\    Filename:      doloop.fs                                         *
\    Date:          22.08.2024                                        *
\    File Version:  5.0                                               *
\    MCU:           Atmega 2560 and similar                           *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-doloop
marker -doloop

: compileonly $10 shb ;

#20 constant ind inlined   \ R20:R21 index

: (do)  ( limit index -- R: leave oldindex xfaxtor )
  [ $910f i, ] \ pop unused zero address byte
  r>
  dup >a xa> @ >r            \ R: leave 
  ind @ >r                   \ R: leave oldindex
  swap $8000 swap - dup >r   \ R: leave oldindex xfactor
  + ind !
  a> 1+ >r
  [ $925f i, ] \ push unused zero address byte
; compileonly

: (?do) ( limit index -- R: leave oldindex xfactor ) 
  2dup xor
  if
    [ '  (do) ] again  \ branch to (do) 
  then
  [ $910f i, ] r> xa> @ >r [ $925f i, ] 2drop
; compileonly

: (+loop) ( n -- )
  [ $0f48 i, ]   \ add r20, tosl
  [ $1f59 i, ]   \ add r21, tosh
  inline drop
; compileonly

: unloop
  [ $910f i, ] r>
  rdrop r> ind ! rdrop
  >r [ $925f i, ]
; compileonly

: do
  postpone (do)
  postpone begin
  flash 2 allot ram  \ leave address
  postpone begin
; immediate compileonly

: ?do
  postpone (?do)
  postpone begin
  flash 2 allot ram  \ leave address
  postpone begin
; immediate compileonly

: leave
  [ $910f i, ] rdrop
  rdrop r> ind !
  [ $925f i, ]
; compileonly

: i
  ind @ rp@ #4 + @ >< -
; compileonly

: j
  rp@ #6 + @ >< rp@ #10 + @ >< - 
; compileonly


: loop
  $0d46 i, $1d55 i, \ add 1 to r20:r21
\  postpone (loop)
  $f00b i,               \ bra +2 if overflow
  postpone again
  postpone unloop
  flash here >xa swap ! ram
; immediate compileonly

: +loop
  postpone (+loop)
  $f00b i,               \ bra +2 if overflow
  postpone again
  postpone unloop
  flash here >xa swap ! ram
; immediate compileonly

