\ *********************************************************************
\                                                                     *
\    Filename:      doloop.txt                                        *
\    Date:          11.04.2017                                        *
\    File Version:  5.0                                               *
\    MCU:           Atmega (not 256)                                  *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ do loop for Atmega32,64,128 (not 256)
-doloop
marker -doloop

: compileonly $10 shb ;

#20 constant ind inlined   \ R18:R19 are unused by the kernel

: (do)  ( limit index -- R: leave oldindex xfaxtor ) 
  r>
  dup >a xa> @ >r            \ R: leave 
  ind @ >r                   \ R: leave oldindex
  swap $8000 swap - dup >r   \ R: leave oldindex xfactor
  + ind !
  a> 1+ >r
; compileonly

: (?do) ( limit index -- R: leave oldindex xfactor ) 
  2dup xor
  if
    [ '  (do) ] again  \ branch to (do) 
  then
  r> xa> @ >r 2drop
; compileonly

: (+loop) ( n -- )
  [ $0f48 i, ]   \ add r20, tosl
  [ $1f59 i, ]   \ add r21, tosh
  inline drop
; compileonly

: unloop
  r>
  rdrop r> ind ! rdrop
  >r
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
  rdrop rdrop r> ind ! 
; compileonly

: i
  ind @ rp@ 3 + @ >< -
; compileonly

: j
  rp@ 5 + @ >< rp@ 9 + @ >< - 
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

