\ *******************************************************************
\                                                                   *
\    Filename:      see.txt                                         *
\    Date:          20.05.2015                                      *
\    FF Version:    5.0                                             *
\    MCU:           PIC30 PIC24 PIC33                               *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************

-see
marker -see
decimal ram
' chars to prompt
: ct ( ew cw n -- ) \ compile a condition table
     ( m -- m )     \ execute aword corresponding to m.
                    \ m may consist of several stack cells
                    \ it is upto the condition word to
                    \ preserve m on the stack
    create
    dup ,                  \ store the condition table size
    for
      , ,                  \ store an entry
    next
  does>                    \ m addr
    dup @                  \ m addr n
    for
      cell+ dup            \ m addr addr
      cell+ >r             \ m addr
      @ex                  \ m flag
      if                   \ m
        r> @ex rdrop exit  \ m     a match was found
      then 
      r>
    next
    drop
;

: dup@ ( addr -- addr lo hi ) dup @ ;
: hi@ ( addr -- addr hi ) dup cf@ nip ;
: field@ ( x mask offset -- field )
  rot swap rshift and ;
: u.4 4 u.r ;

: u.. decimal 0 <# #s #> type hex ;

: lookup:
  create does> swap cells + @ex ;

\ Register offset
:noname ." [W" u.. ." +Wb]" ;

\ Register offset
:noname ." [W" u.. ." +Wb]" ;

\ Indirect with Pre-Increment
:noname ." [++W" u.. ." ]" ;

\ Indirect with Pre-Decrement
:noname ." [--W" u.. ." ]" ;

\ Indirect with Post-Increment
:noname ." [W" u.. ." ++]" ;

\ Indirect with Post-Decrement
:noname ." [W" u.. ." --]" ;

\ Indirect
:noname ." [W" u.. ." ]" ;

\ Register Direct
:noname ." W" u.. ;

flash lookup: mov.amode , , , , , , , ,


\ take the next cell
:noname cell+ ;
' true

\ return 
:noname ." return" drop false ;
:noname ( addr -- addr f ) hi@ $6 = ;

\ unintialised flash or nop
\ :noname drop false ;
\ :noname ( addr -- addr f ) hi@ $ff = ;

\ goto
:noname ." goto  " dup@ xa> c>n .id drop false ;
:noname ( addr -- addr f ) hi@ $4 = ;

\ cp0 Wn
:noname ." cp0 " dup@ 
 dup $f $0 field@ swap $7 $4 field@ mov.amode cell+ ;
:noname hi@ $e0 = ;

: .bra ." bra " type dup@ 2* over + 2+ u.4 cell+ ; 

\ bra z
:noname s" z, " .bra ;
:noname ( addr --  addr f ) hi@ $32 = ;

\ bra nz
:noname s" nz, " .bra ;
:noname ( addr --  addr f ) hi@ $3a = ;

\ bra nn
:noname s" nn, " .bra ;
:noname ( addr --  addr f ) hi@ $3b = ;

\ bra n
:noname s" n, " .bra ;
:noname ( addr --  addr f ) hi@ $33 = ;

\ bra c
:noname s" c, " .bra ;
:noname ( addr -- addr f ) hi@ $31 = ;

\ bra unconditionally
:noname s" un, " .bra ;
:noname ( addr -- addr f ) hi@ $37 = ;

\ sub Wb, #li5, Wd
:noname ." sub W" 
 dup cf@ $7 and 1 lshift swap #15 rshift 1 and + u..
 dup@  ." , " $1f and u.. ." , "
 dup@ $f $7 field@  over @ $7 #11 field@ mov.amode
 cell+ ;
:noname hi@ $f8 and $50 = ;

\ pop f
:noname ." pop " @+ u. ;
:noname hi@ $f9 = ;

\ mov  #16, Wn
:noname ." mov " 
  dup cf@ over $fff 4 field@ swap $f and #12 lshift or u.
  ." , W" $f and u.  cell+ ;
:noname ( addr -- addr f ) hi@ $f0 and $20 = ;

\ mov Ws, Wd
:noname ." mov" dup@
  dup $4000 and if s" .b " else s" .w " then type
  dup $f $0 field@ over $7 #4 field@ mov.amode
  [char] , emit space
  dup $f $7 field@ swap $7 #11 field@ mov.amode cell+ ;
:noname ( addr -- addr f ) hi@ $1f $3 field@ $f = ;

\ rcall
:noname ." rcall " dup@ 2* over + 2+ c>n .id cell+ ;
:noname ( addr -- addr f ) hi@ $07 = ;

\ define a condition table 
\ called (see) with 15 elements
flash 
#15 ct (see)
ram

: see 
  ' cr hex
  begin
    dup u.4
    dup cf@ u.4 u.4  
    (see) cr 
    dup 0=     \ dup and 0= will be optimised away
  until
  drop
;
' .st to prompt

