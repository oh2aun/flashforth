\ *********************************************************************
\                                                                     *
\    Filename:      see.txt                                           *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

-see 
marker -see
hex ram
: *@ dup @ ;
: u.4 4 u.r ;
: *@+ dup cell+ @ u.4 ;
: 5sp 5 spaces ;
: @braddr ( addr -- addr xt-addr )
    *@ 7ff and dup 400 and 
    if f800 or then 2* over +  cell+ ;
: @xtaddr ( addr -- addr xt-addr )
    dup dup c@ swap 2+ @ 8 lshift + 2* ;
: .dw ( addr -- addr+2 ) *@ u.4 cell+ ;
: .bra ( addr -- addr+2 ) @braddr u.4 cell+ ;
: .bz  ( addr -- addr+2 )
    *@ ff and dup 80 and
    if ff00 or then 2* over + cell+ u.4 cell+ ;
: .lit ( addr -- addr+6 ) @xtaddr c>n .id cell+ cell+ .dw ;
: .bit ( addr -- addr+2 ) dup @ fff and 9 rshift u.4 cell+ ;
: .reg ( addr -- addr ) space dup @ ff and u.4 ;
: ?call ( addr -- addr f ) *@ fe00 and ec00 = ;
: ?ret ( addr -- addr f ) *@ 0012 = ;
: ?goto  ( addr -- addr f ) *@ ff00 and ef00 = ;
: ?bra ( addr -- addr f ) *@ f800 and d000 = ;
: ?bz ( addr --  addr f ) *@ ff00 and e000 = ;
: ?bnz ( addr --  addr f ) *@ ff00 and e100 = ;
: ?bov ( addr --  addr f ) *@ ff00 and e400 = ;
: ?bnov ( addr --  addr f ) *@ ff00 and e500 = ;
: ?bc ( addr -- addr f ) *@ ff00 and e200 = ;
: ?rcall ( addr -- addr f ) *@ f800 and d800 = ;
: ?bcf ( addr -- addr f ) *@ f000 and 9000 = ;
: ?bsf ( addr -- addr f ) *@ f000 and 8000 = ;
: ?btfsc ( addr -- addr f ) *@ f000 and b000 = ;
: ?btfss ( addr -- addr f ) *@ f000 and a000 = ;
: (see) ( addr -- addr' | false )
        dup u.4
        *@ u.4
        ?call if *@+ ." call  " @xtaddr c>n .id cell+ cell+ else 
        ?rcall if 5sp ." rcall " @braddr c>n .id cell+ else
        ?bz if 5sp ." bz    " .bz else
        ?bnz if 5sp ." bnz   " .bz else
        ?bov if 5sp ." bov   " .bz else
        ?bnov if 5sp ." bnov  " .bz else
        ?bc if 5sp ." bc    " .bz else 
        ?bra if 5sp ." bra   " .bra else
        ?ret if 5sp ." return" drop false else
        ?goto if *@+ ." goto  " @xtaddr c>n .id cell+ drop false else
        ?bcf if 5sp ." bcf  " .reg .bit else
        ?bsf if 5sp ." bsf  " .reg .bit else
        ?btfsc if 5sp ." btfsc" .reg .bit else
        ?btfss if 5sp ." btfss" .reg .bit else
        cell+
        then then then then then 
        then then then then then 
        then then then then
        cr ;

: dis ( addr -- )
  cr
  begin
    (see) dup 0=
  until
  drop
;
: see ( "word" -- )
  ' dis
;

hex ram

