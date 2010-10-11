\ *******************************************************************
\                                                                   *
\    Filename:      math_dspic30.fth                                *
\    Date:          10.3.2009                                       *
\    FF Version:    4.6                                             *
\    MCU:           dsPIC30F                                        *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ Double and mixed math words

\ ud/mod   ud1 u2 -- u3 ud4   32/16->32 divide
: ud/mod
  >r 0 r@ um/mod rot rot r> um/mod rot
;

\ ud* ud1 u2 -- u3           32*16->32 multiply
: ud*
  dup >r um* drop swap r> um* rot +
;

\ dnegate  d1 -- d2          32 bit negate
: dnegate
  swap invert swap invert 1 m+
;

\ ?dnegate d1 n -- d2  negate d1 if n is negative
: ?dnegate
  0< if dnegate then
;

\ dabs   d1 -- +d2     32 bit absolute value
: dabs
  dup ?dnegate
;

: fm/mod ( d1 n1 -- n2 n3 )
  dup >r
  2dup xor >r
  >r
  dabs r@ abs um/mod
  swap r> ?negate swap
  r> 0< if
    negate
    over if
      r@ rot - swap 1-
    then
  then
  r> drop
;

\ d+, d- by Andrew Smith
: d+ ( d1 d2 -- d1+d2 ) dup >r dabs r> tuck ?negate >r ?negate m+ r> + ;
: d- ( d1 d2 -- d1-d2 ) dnegate d+ ;


