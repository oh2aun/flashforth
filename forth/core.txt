\ *********************************************************************
\                                                                     *
\    Filename:      core.txt                                          *
\    Date:          31.12.2013                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
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
  bl word latest @ (f) ?abort?
  c>n 2- dup @ ?abort?
  dup flash dp ! @ latest ! ram
;

 ( addr n c -- ) \ fill addr to addr+n with c
: fill rot !p>r swap for dup pc! p+ next r>p drop ;

\  addr n --
: erase  0 fill ;

\ addr n --
: blanks bl fill ;

\ x -- 0 | x x
: ?dup dup if inline dup then ;

\ nfa -- flag
: in? c@ $40 and ;

\ addr -- addr+1 n 
: count c@+ ;

hex ram

