\ *******************************************************************
\                                                                   *
\    Filename:      doloop.txt                                      *
\    Date:          21.08.2014                                      *
\    FF Version:    5.0                                             *
\    MCU:           PIC30 PIC24 PIC33                               *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
-doloop
marker -doloop

: compileonly $10 shb ; \ Make word compile only

: (do)   ( limit index -- R: leave oldindex xfactor ) 
  rdrop r>                   \ Address hi byte is always zero
  dup >a xa> @ >r            \ R: leave 
  ind @ >r                   \ R: leave oldindex
  swap $8000 swap - dup >r   \ R: leave oldindex xfactor
  + ind !
  a> 2+ >r 0 >r
; compileonly

: (?do)
  2dup xor
  if
    [ '  (do) ] again        \ branch to (do) 
  then
  r> r> xa> @ >r >r 2drop
; compileonly

: unloop
  r> r>
  rdrop r> ind ! rdrop
  >r >r
; compileonly

: do
  postpone (do)
  postpone begin
  flash 2 allot ram
  postpone begin
; immediate compileonly

: ?do
  postpone (?do)
  postpone begin
  flash 2 allot ram
  postpone begin
; immediate compileonly

: leave
  rdrop rdrop rdrop r> ind ! 0 >r
; compileonly

: i ( -- i   R: xfactor -- xfactor )
  \ i = index - xfactor
  [ $97b87f. as, ] \ mov [W15-2], w0
  [ ind $b5  as, ] \ sub ind, wreg
  [ $782f00. as, ] \ mov w0,  [++W14]
; inlined compileonly

: j  ( -- j  R: j-xfactor i-leave j-index i-xfactor -- the same ) 
  [ $97b86f. as, ] \ mov [W15-4], W0
  [ $97b8cf. as, ] \ mov [W15-8], W1
  [ $502f01. as, ] \ sub w0, w1, [++W14]
; inlined compileonly

: loop
  \ postpone (loop)
  ind $2000 or $ec as, \ inc index
  0 until,             \ until overflow
  postpone unloop
  flash here >xa swap ! ram
; immediate compileonly

: +loop
  \ postpone (+loop)
  $78002e.         as,  \ mov [W14--], W0
  ind $2000 or $b4 as,  \ add index
  0 until,              \ until overflow
  postpone unloop
  flash here >xa swap ! ram
; immediate compileonly

