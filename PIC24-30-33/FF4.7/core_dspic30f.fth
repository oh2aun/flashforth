\ *******************************************************************
\                                                                   *
\    Filename:      core.fth                                        *
\    Date:          10.10.2010                                      *
\    FF Version:    4.7                                             *
\    MCU:           dsPIC30F                                        *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ Some extra core words
-core
marker -core
hex ram

\ Interpret a string. The string must be in ram
: evaluate ( caddr n -- )
  'source 2@ >r >r >in @ >r
  interpret
  r> >in ! r> r> 'source 2!
;

: forget ( --- name )
  bl word latest @ (f) 0= ?abort?
  c>n 2- dup @ 0= ?abort?
  dup flash dp ! @ latest ! ram
;

: erase  ( addr n -- )
  0 fill
;

: blanks  ( addr n -- )
  bl fill
;

: ?dup ( x -- 0 | x x )
  dup if dup then 
;

: pick ( xu ... x0 u -- xu ... x0 xu) 2* 2+ sp@ swap - @ ;

