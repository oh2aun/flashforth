\ *********************************************************************
\                                                                     *
\    Filename:      seen.txt                                          *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ This file needs ct.txt

-see
marker -see
hex ram
: dup@ dup @ ;
: u.4 4 u.r ;
: dup+@. dup cell+ @ u.4 ;
: 5sp 5 spaces ;
: @saddr ( addr -- addr xt-addr )
        dup@ 7ff and dup 400 and 
        if f800 or then 2* over +  cell+ ;
: @laddr ( addr -- addr xt-addr )
        dup dup c@ swap cell+ @ 8 lshift + 2* ;
: .dw ( addr -- addr+2 ) dup@ u.4 cell+ ;
: .bl ( addr -- addr+2 ) @saddr space u.4 cell+ ;
: .bs  ( addr -- addr+2 )
        dup@ ff and dup 80 and
        if ff00 or then 2* over + cell+ space u.4 cell+ ;

\ output a cell as a number
:noname cell+ ;
' true

\ goto
:noname dup+@. ." goto  " @laddr c>n .id drop false ;
:noname ( addr -- addr f ) dup@ ff00 and ef00 = ;

\ bz
:noname 5sp ." bz" .bs ;
:noname ( addr --  addr f ) dup@ ff00 and e000 = ;

\ bnz
:noname 5sp ." bnz" .bs ;
:noname ( addr --  addr f ) dup@ ff00 and e100 = ;

\ bc
:noname 5sp ." bc" .bs ;
:noname ( addr -- addr f ) dup@ ff00 and e200 = ;

\ bra
:noname 5sp ." bra" .bl ;
:noname ( addr -- addr f ) dup@ f800 and d000 = ;

\ return 
:noname 5sp ." return" drop false ;
:noname ( addr -- addr f ) dup@ 0012 = ;

\ rcall
:noname 5sp ." rcall " @saddr c>n .id cell+ ;
:noname ( addr -- addr f ) dup@ f800 and d800 = ;

\ call
:noname dup+@. ." call  " @laddr c>n .id cell+ cell+ ;
:noname ( addr -- addr f ) dup@ fe00 and ec00 = ;

\ define a condition table called (see) with 9 elements
flash      \ define a condition table stored in flash memory 
9 ct (see)
ram

: see 
  ' cr 
  begin
    dup u.4
    dup@ u.4  
    (see) cr 
    dup 0= \ dup and 0= will be optimised away
  until
  drop
;

