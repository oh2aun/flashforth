\ *********************************************************************
\                                                                     *
\    Filename:      doloop.fs                                         *
\    Date:          24.08.2024                                        *
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
  [ $910f i, ]   \ pop unused zero address byte
  r>
  dup >a xa> @ >r            \ R: leave 
  ind @ >r                   \ R: leave oldindex
  swap $8000 swap - dup >r   \ R: leave oldindex xfactor
  + ind !
  a> 1+ >r
  [ $925f i, ]  \ push unused zero address byte
; compileonly

: (?do) ( limit index -- R: leave oldindex xfactor ) 
  2dup xor
  if
    [ '  (do) ] again  \ branch to (do) 
  then
  [ $910f i, ] r> xa> @ >r [ $925f i, ] drop drop
; compileonly

: unloop
  [ $910f i, ] r>  \ remember return address
  rdrop            \ xfactor 
  r> ind !         \ old index
  rdrop            \ leave address
  >r [ $925f i, ]  \ restore return address
; compileonly

: do
  postpone (do)
  postpone begin     \ leave storage address
  flash 2 allot ram  \ leave address storage
  postpone begin     \ jump back address
; immediate compileonly

: ?do
  postpone (?do)
  postpone begin
  flash 2 allot ram
  postpone begin
; immediate compileonly

: leave
  [ $910f i, ] rdrop  \ drop the return address
  rdrop               \ xfactor
  r> ind !            \ old index
  [ $925f i, ]        \ return to leave address 
; compileonly

: i
  dup           \ st    -y, r25
                \ st    -y, r24
  [ $b7ed i, ]  \ in    r30, 3d
  [ $b7fe i, ]  \ in    r31, 3e
  [ $9634 i, ]  \ adiw  r30, 4
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
  [ $9636 i, ]  \ adiw  r30, 6
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
  $0d46 i, $1d55 i,       \ add 1 to r20:r21
  $f00b i,                \ bra +2 if overflow
  postpone again
  postpone unloop
  flash here >xa swap ! ram \ leave address stored after (do)
; immediate compileonly

: +loop
  $0f48 i,               \ add r20, tosl
  $1f59 i,               \ add r21, tosh
  $9189 i, $9199 i,      \ drop
  $f00b i,               \ bra +2 if overflow
  postpone again
  postpone unloop
  flash here >xa swap ! ram \ leave address stored after (?do)
; immediate compileonly

