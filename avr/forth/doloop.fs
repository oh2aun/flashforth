\ *********************************************************************
\                                                                     *
\    Filename:      doloop.fs                                         *
\    Date:          24.08.2024                                        *
\    File Version:  5.0                                               *
\    MCU:           Atmega (not 2560)                                 *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ do loop for Atmega32,64,128 (not 2560)
-doloop
marker -doloop

: compileonly $10 shb ;

#20 constant ind inlined   \ R20:R21 index

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
  dup           \ st    -y, r25
                \ st    -y, r24
  [ $b7ed i, ]  \ in    r30, 3d
  [ $b7fe i, ]  \ in    r31, 3e
  [ $9633 i, ]  \ adiw  r30, 3
  [ $9191 i, ]  \ ld    r25, z+
  [ $9181 i, ]  \ ld    r24, z+   fetch xfactor
  [ $018a i, ]  \ movw  r16, r20  fetch index
  [ $1b08 i, ]  \ sub   r16, r24
  [ $0b19 i, ]  \ sbc   r17, r25  index-xfactor
  [ $01c8 i, ]  \ movw  r24, r16
; compileonly

: j
  dup           \ st    -y, r25
                \ st    -y, r24
  [ $b7ed i, ]  \ in    r30, 3d
  [ $b7fe i, ]  \ in    r31, 3e
  [ $9635 i, ]  \ adiw  r30, 5
  [ $9111 i, ]  \ ld    r17, z+
  [ $9101 i, ]  \ ld    r16, z+   fetch index
  [ $9632 i, ]  \ adiw  r30, 2
  [ $9191 i, ]  \ ld    r25, z+
  [ $9181 i, ]  \ ld    r24, z+   fetch xfactor
  [ $1b08 i, ]  \ sub   r16, r24
  [ $0b19 i, ]  \ sbc   r17, r25  index-xfactor
  [ $01c8 i, ]  \ movw  r24, r16
; compileonly

: loop
  $0d46 i, $1d55 i,      \ add 1 to r20:r21
  $f00b i,               \ bra +2 if overflow
  postpone again
  postpone unloop
  flash here >xa swap ! ram
; immediate compileonly

: +loop
  $0f48 i,               \ add r20, tosl
  $1f59 i,               \ add r21, tosh
  $9189 i, $9199 i,      \ drop
  $f00b i,               \ bra +2 if overflow
  postpone again
  postpone unloop
  flash here >xa swap ! ram
; immediate compileonly

