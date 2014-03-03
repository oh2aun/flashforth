\ *********************************************************************
\                                                                     *
\    Filename:      math.txt                                          *
\    Date:          31.12.2013                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Double, triple and mixed math words

: m*  ( n1 n2 -- d )
  2dup xor >r
  abs swap abs um*
  r> ?dnegate
;

: sm/rem ( d1 n1 -- n2 n3 )
  2dup xor >r over >r
  abs >r dabs r> um/mod
  swap r> ?negate
  swap r> ?negate
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
: /mod ( n1 n2 -- n3 n4 )
  >r s>d r> sm/rem
;
: mod ( n1 n2 -- n3 )
  /mod drop
;

: */mod ( n1 n2 n3 -- n4 n5 )
  >r m* r> sm/rem
;
: */ ( n1 n2 n3 -- n4 )
  >r m* r> sm/rem nip
;

\ multiply single number with double number.
\ Triple precision (48-bit) result
: ut* ( ud u -- ut)
  dup >r  swap >r  um*  r> r> um* >r
  0 swap  0 d+  r> + 
;

  
\ Divide triple number with single number
\ Double result
: ut/ ( ut u -- ud)
  dup >r um/mod  r> swap >r
  um/mod  swap drop r> 
;

\ Scale with triple number intermediate result
: um*/ ( ud1 u1 u2 -- ud2)
  >r ut* r> ut/
;
\ Signed scale d1*n1/n2 with intermediate triple result
: m*/ ( d1 n1 n2 -- d2 )
   rot dup >r rot rot 2dup xor r> xor >r \ save result sign
   abs >r abs >r dabs r> r> \ now have S:ud1 u1 u2
   um*/ r> ?dnegate
;

