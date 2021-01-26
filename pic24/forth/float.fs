\ Floating point routines for FlashForth
\ This code is adapted from the FP code made by Leon Maurer for AMFORTH
\ Adaptations to Flashforth.
\ Igor Mokos 07/2014
\ Mikael Nordman 15.05.2015

\  This program is free software; you can redistribute it and/or
\  modify it under the terms of the GNU General Public License
\  as published by the Free Software Foundation; using version 2
\  of the License.


-fpmath
marker -fpmath
decimal

: 0> negate 0< ;
: d>s drop ;

\ FP STACK MANIPULATION WORDS

: fdrop ( f -- )
  drop drop ;

: fdup ( f -- f f )
  over over ;

: fover ( f1 f2 -- f1 f2 f1 )
  >r >r
  over over
  r>
  rot rot
  r>
  rot rot ;
  
: fswap ( f1 f2 -- f2 f1 )
  rot >r rot r> ;

: frot ( f1 f2 f3 -- f2 f3 f1 )
  >r >r fswap r> r> fswap ;

: fnip ( f1 f2 -- f2 )
  fswap fdrop ;

: ftuck ( f1 f2 -- f2 f1 f2 )
  fswap fover ;

: nfswap ( f n -- n f )
  rot rot ;

: fnswap ( n f -- f n )
  rot ;

: nfover ( f n -- f n f )
  >r fdup r> nfswap ;

: fnover ( n f -- n f n )
  >r over r> swap ;

\ WORDS FOR STORING FLOATS

: f@ 2@ ;
: f! 2! ;
: fconstant 2constant ;
: fvariable 2variable ;
: fliteral swap postpone literal postpone literal ; immediate

\ USEFUL CONSTANTS

0 0 fconstant f0 \ 0.0
0 16256 fconstant f1 \ 1.0
0 16672 fconstant f10 \ 10.0
0 16128 fconstant f0.5

\ OPERATORS FOR SIGNLES

: >= ( n1 n2 -- f)
  over over > >r = r> or ;

\ OPERATORS FOR DOUBLES

: d10* ( d -- d*10 )
  10 * ( n-lower n-uppper*10 )
  swap 10 um* ( n-uppper*10 d-lower*10 )
  fnswap + ;

\ negates d if it's negative and returns a flag saying whether 
\ it was negated or not
: dnegateifneg ( d -- d flag )
  fdup ( ddup ) d0< if dnegate true else false then ;

\ splits a 24 bit double (e.g. the significand) in to two 12 
\ bit singles --
\ an upper and a lower (nU, nL) keeping the signs
\ we remove the sign at the beginning and add it back at the
\ end because you
\ can run in to inconsistancies otherwise. For example:
\ > -13176795. d2/ d.
\ -6588398  ok
\ > 13176795. d2/ d.
\ 6588397  ok
: dsplit ( d -- nU nL )
  dnegateifneg >r fdup ( ddup )
  \ get the upper half by shifting it 12 times to the right
  12 for d2/ next d>s
  r@ if negate then nfswap ( ndswap )
  \ get the lower half using a mask
  drop 4095 ( 0000111111111111 ) and
  r> if negate then ;

\ HELPER WORDS FOR SEPERATING OUT AND PUTTING BACK TOGETHER 
\ THE DIFFERENT
\ PARTS OF FLOATS

\ FROM WIKIPEDIA: "The true significand includes an implicit 
\ leading bit with
\ value 1 unless the exponent is stored with all zeros." So 
\ we must test that
\ both significand and exponent are all zeros (making the 
\ exponent -127). Also
\ note that we can have +0 and -0. This exponent of -127 
\ really means -126,
\ it's just a way to indicate the implicit leading bit should
\ be left out.

\ returns the exponent, even if -127
: frawexponent ( f -- n )
  nip
  32640 and ( 0111111110000000 )
  7 rshift
  127 - ;

\ remember that an exponent of -127 really means subnormal 
\ so the actual
\ exponent is -126
: fexponent ( f -- n )
  frawexponent -126 max ;

\ returns +/- 1 for positive or negative, counts 0 as postive
: fsign ( f -- n )
  nip 0< if -1 else 1 then ;

\ returns the significand including sign and implicit 1 in 
\ the 24th place
\ if it should be there (i.e. unless frawexponent = -127 ). 
\ Note that the -0
\ and +0 both return d 0
: fsignificand ( f -- d )
  fdup 127 and ( 0000000001111111 )
  fover frawexponent -127 = 0= if 128 + then
  fover fsign 0< if dnegate then fnip ;

\ n is exponent in range [-127,127]
\ handles exponent error checking
: fsetexponent ( f n -- f )
  dup -127 < 0= abort" exponent < -127 "
  dup 127 > 0= abort" exponent > 127 "
  >r -32641 and ( 1000000001111111 ) r>
  127 + 7 lshift or
  ;

\ stores the sign of n in to f at addr
: fsetsign ( f n -- )
  0< if ( make negative )
    32768 or ( 1000000000000000 ) \ -32768
  else ( make positive )
    32767 and ( 0111111111111111 )
  then
  ;

\ only keeps the first 24 digits (23 explicitly), so 
\ significand must be in
\ proper form beforehand aborts if out of range
: fmksgnd ( d -- f-with-exponent=-127 )
  dnegateifneg >r ( f d, R: flag )
  \ only need to look at upper half of double to see if 
  \ it's too large
  \ the upper half can only use the first 8 digits
  dup 255 > 0= abort" |significand| > 16777215 "
  127 and
  r> if -1 fsetsign then
  ;

\ IT'S HANDY TO BREAK A FLOAT IN TO A D (SIGNIFICAND) 
\ AND N (EXPONENT)
\ THE FOLLOWING FUNCTIONS MANIPULATE THAT PAIR

\ performs a 'right shift' on a float split in to an d and n
\ d is shifted right (halved) and n is incrimented
: frshift ( d-significand n-exponent -- d n )
  1+ >r d2/ r> ;

\ rshifts n times
: frshiftn ( d-significand n-exponent n-times -- d n )
  for frshift next ;

\ like rshift, but in other direction
: flshift ( d-significand n-exponent -- d n )
  1- >r d2* r> ;

: flshiftn ( d-significand n-exponent n-times -- d n )
  for flshift next ;

\ shifts until the exponent is the desired value
: fshifttoexp 
  ( d-significand n-exponent n-desired-exponent -- d n )
  over - dup abs swap 0> ( d n n-abs-diff n-diff )
  if 
    frshiftn
  else
    flshiftn
  then ;

: sigexp>f ( d-significand n-exponent -- f )
\ the plan is to first make the signficand positive and 
\ then shift it so that
\ it has a one in the 24th place (and handle the exponent 
\ accordingly) then
\ take care of the sign and stick the significand and 
\ exponent together to
\ make a float
  
  \ take off sign
  nfswap ( ndswap ) dnegateifneg >r fnswap ( d n, R: flag )

  \ if the significand is too large, shift it right
  \ OR
  \ shift it right if the exponent is too small
  begin
    \ only need to look at upper half of double to see 
	\ if it's too large
    \ the upper half should use the first 8 digits
    over 255 > 
	\ 255 is all ones in the first 8 places -- b11111111
    over -126 < or
  while
    frshift
  repeat

  \ if the significand is too small, shift it left
  \ however, make sure to keep the exponent >=-126
  \ or zero would cause it to shift left forever
  begin
    over 128 < 
	\ 128 is one followed by seven zeros -- b10000000
    over -126 > and
  while
    flshift
  repeat

  \ check to see if it's a subnromal number by checking 
  \ if the significand is
  \ less than 24 digits
  nfover 0 128 d<
  if 1- then 
  \ if it is, change the exponent from -126 to -127

  \ restore sign
  r> swap >r if dnegate then ( d, R: n )

  fmksgnd r> fsetexponent ;

: f>sigexp ( f -- d-significand n-exponent )
  fdup fexponent >r fsignificand r> ;

: faddtoexponent ( f n -- f )
  >r f>sigexp r> + sigexp>f ;
\  >r fdup fexponent r> + fsetexponent ;

\ CONVERSION

: d>f ( d -- f )
  23 sigexp>f ;

: s>f ( n -- f )
  s>d d>f ;

: f>d ( f -- d )
  f>sigexp 23 fshifttoexp drop ;

: f>s ( f -- n )
  f>d d>s ;  

\ MATHEMATICAL OPERATORS

\ fsignificand takes care of the +/- 0 problem
: f0= ( f -- flag )
  fsignificand d0=  ;

\ works because 0 is postive for doubles
: f0< ( f -- flag)
  fsignificand d0< ;

: fnegate ( f -- -f )
  fdup fsign negate fsetsign ;

: fnegateifneg ( f -- f flag )
  fdup f0< if fnegate true else false then ;

: negateiftrue ( f flag -- f )
  if fnegate then
;

: fabs ( f -- |f| )
  fdup f0< negateiftrue ;

\ strict equality -- no range for wiggle room
: f= ( f1 f2 -- flag )
  f>sigexp >r fswap f>sigexp >r d= r> r> = and ;

\ sigexp>f will take care of changing significand
\ if nescessary --
\ such as for subnormal numbers
: f2/ ( f -- f/2 )
  f>sigexp 1- sigexp>f ;

: f2* ( f -- f/2 )
  f>sigexp 1+ sigexp>f ;


: f< ( f1 f2 -- flag )
  f- f0< ;

: f> ( f1 f2 -- flag )
  fswap f< ;

: f>= ( f1 f2 -- flag )
  f< 0= ;

: f<= ( f1 f2 -- flag )
  f> 0= ;

: fmax ( f1 f2 -- f )
  fover fover f- f0<
  if fswap then
  fdrop ;

: fmin ( f1 f2 -- f )
  fover fover f- f0< 0=
  if fswap then
  fdrop ;

: fprep4div ( f -- d n )
  f>sigexp

  \ get d so that it has a one in the 25th place
  begin
    >r dup 4096 < r> swap
	\ only need to look at upper part of d
  while
    flshift
  repeat ;

\ from the standard:
\ If f3 is positive, flag is |f1-f2| < f3
\ If f3 is zero, this is the same as f1 f2 f=
\ If f3 is negative, flag is |f1-f2| < |f3|*(|f1|+|f2|)
: f~ ( f1 f2 f3 -- flag )
  fdup f0=
  if
    fdrop f=
  else
    fdup f0<
    if
      fabs >r >r ( f1 f2, R: |f3|)
      fover fabs fover fabs f+ r> r> f*
	  ( f1 f2 |f3|*[|f1|+|f2|] )
    then
    >r >r f- fabs r> r> f<
  then
;

\ the greatest integer <= the float
\ e.g. the floor of 3.5 is 3
\ and the floor of -3.5 is -4
\ the division in fshifttoexp gets rid of the fractional part
: floor ( f -- f )
  f>sigexp 23 fshifttoexp sigexp>f ;

\ the ceiling of x is -floor(-x)
: ceil ( f -- f )
  fnegate floor fnegate ;

\ returns f mod 1 -- basically the fractional part of f
\ fmod1(f) = f - floor(f)
: fmod1 ( f -- f )
  fdup floor f- ;

\ round to nearest integer
: fround ( f -- f )
  fdup fmod1 f0.5 f<
  if
    floor
  else
    ceil
  then ;

: f10^n ( n -- f-10^n )
  f1 ( n f )
  fnover 0= if \ n is zero, so just return the 1.0 we have
    fnswap drop
  else
    fnover abs for f10 f* next ( n f-10^|n| )
    fnswap 0 < if
      f1 fswap f/
    then
  then
;

\ finds the integer n-steps with the smallest magnitude
\ such that
\ f > 10^n-steps where n-steps = n * n-stepsize for some
\ integer n
\ for exaple "3 .123 smlrpow10" yeilds n-steps=3
\ and f-10^n-steps=10^-3
\ note that f must be strictly greather than zero
: smlrpow10 ( n-stepsize f  -- n-steps f-10^n-steps )
  0 nfswap f1 fswap ( n-stepsize n-steps f-comparison f )

  \ first, we increase comparison until it's too large
  begin
    fover fover f< \ is f-comparison < f
  while
    >r >r >r >r ( n-stepsize n-steps, R: f f-comparison )
    over + \ add n-stepsize to n-steps
    over f10^n r> r> f*
	\ multiply f-comparison by 10^n-stepsize
    r> r>
  repeat

  \ then we divide until it's too small
  begin
    fover fover f> \ is f-comparison > f
  while
    >r >r >r >r ( n-stepsize n-steps, R: f f-comparison )
    over - \ subtract n-stepsize from n-steps
    over f10^n r> r> fswap f/
	\ divide f-comparison by 10^n-stepsize
    r> r>
  repeat

  fdrop >r >r >r drop r> r> r>
;

\ FIRST, SOME OUTPUT HELPER WORDS
\ if f is negative, negate it and emit a minus sign
: tkcofsign ( f -- |f|)
  fdup f0<
  if
    fnegate \ make it positive
    45 emit \ print a "-"
  then
;

: emitdigit ( n -- )
  48 + emit ;

\ f is a float in [1.0,10); this word emits the digit in the
\ ones place and returns the fractional part
: femitdigit ( f -- f-remainder )
  fdup floor f>s emitdigit \ emit the integer
  fmod1 \ get rid of it, leaving the remainder
;

\ f is a float in [1.0,10); this word emits 'n'
\ digits and returns the remainder
: fprintdigits ( f n -- f-remainder )
  for femitdigit f10 f* next
;

\ prints out 'n' zeros
: emitnzeros ( n -- )
  for 48 emit next ;

\ this stores how many digits will be printed
\ by F., FE., and FS.
\ (EVENTUALLY) if it's set to zero, then the word will
\ choose how many digits to print
\ using the dragon2 algorithm, for now, FSD. does that
6 constant precision

\ : set-precision ( u -- )
\   abs precision ! ;

\ returns f such that the PRECISIONth digit has been rounded
: round2prc ( f -- f )
  fnegateifneg >r \ remove sign and store it on the stack
  1 nfover smlrpow10 fnswap >r f/
  \ scale f to be in the range the range [1.0, 10.0)
  precision 1- f10^n f*
  \ shift so that PRECISION digits are in the integer part
  fround \ round the number
  f0.5 fover f0<
  if f- else f+ then
  \ put something in the next digit to get around later rounding errors
  precision 1- f10^n f/
  \ shift the number back to where it was initially
  r> f10^n f* \ scale f back to its original size
  r> negateiftrue \ restore sign
;

\ OUTPUT WORDS

\ note that this doesn't do any rounding
: f.no-space ( f -- )
  \ handle zero seperately
  fdup f0=
  if
    fdrop 46 48 emit emit \ prints "0."
    precision 1- emitnzeros \ prints "0" precision-1 times
  else
    tkcofsign
	\ we're now working with a positive number
    1 nfover smlrpow10 fnswap
	( f f-10^n-steps n-steps )
    \ f/10^n-steps is a number in [1.0,10)
    >r f/ r> ( f/10^n-steps n-steps )
    dup 0 <
    if \ the number is less than 1.0, so print "0." and
	\ then enough leading zeros
      46 48 emit emit \ prints "0."
      abs 1- emitnzeros \ print n-steps - 1 leading zeros
      \ print out the right number of digits
      precision fprintdigits fdrop
    else \ f is greather than 1.0
      dup 1+ precision >=
      if \ everything we need to print is in front
	  \ of the decimal place
	>r ( f/10^n-steps, R: n-steps )
	precision fprintdigits fdrop r> ( n-steps )
	precision - 1+ emitnzeros
	\ print n-steps - precision + 1 trailing zeros
	46 emit \ finially, print a '.' for good measure
      else \ last case: we have to print some before and
	  \ some after the decimal place
	>r ( f/f-10^n-steps, R: n-steps )
	r@ 1+ fprintdigits
	\ print the digits before the decimal place
	46 emit \ print a '.'
	precision r> - 1- fprintdigits fdrop
	\ print digits after decimal place
      then
    then
  then
;

\ : f. ( f -- )
\  round2prc f.no-space bl emit ;
: f. ( f -- )
  fdup f0=
  if
    f.no-space bl emit
  else
    round2prc f.no-space bl emit
  then
;

\ print a float with engineering notation
: fe. ( f -- )
  \ handle zero seperately
  fdup f0=
  if
    f. \ f. prints zero the same way fe. would
  else
    tkcofsign round2prc
    fdup 3 nfswap smlrpow10 ( f n-steps f-10^n-steps )
    fnswap >r f/ f.no-space \ normalize the number and print it
    [char] e emit r> . \ print the exponent
  then
;

\ print f using scientific notation
: fs. ( f -- )
  \ handle zero seperately
  fdup f0=
  if
    f. \ f. prints zero the same way fs. would
  else
    tkcofsign round2prc
    fdup 1 nfswap smlrpow10 ( f n-steps f-10^n-steps )
    fnswap >r f/ f.no-space \ normalize the number and print it
    [char] e emit r> . \ print the exponent
  then
;

\ INPUT AND PARSING WORDS

\ the last returned value is true if the char was found, and false if not
: extract ( n-adr n-length c-char -- n-adr n-new-length n-extracted flag )
  >r 2dup r> scan     ( adr len adr-e count-e )
  dup 0= if           \ character not found
    false
  else                \ character found, convert and cut of string
    >r nip 2dup swap - swap r>
    1 /string dup if
      0. 2swap sign? >r
      >number if
        rdrop drop false
      else
       drop r> if dnegate then drop true
      then
    else
      2drop 0 true
    then
  then ;

\ the plan is to first get the exponent (if there is one)
\ then we take care of the sign, if any
\ next, we start storing the digits in to a double while keeping track
\ of the exponent. For example 12.34e will get turned in to the double
\ 1234, so we need to decriment the exponent by two (one for each digit
\ after the decimal place) to get the right answer. If the double fills
\ up, then we stop since the double has more significant digits than a
\ float has. If the float fills up before we get to the decimal place,
\ then we have to add one to the exponent for every digit before the
\ decimal place we miss.
\ the double is then converted to a float, which we divide or multiply
\ by the appropriate power of 10 to get the exponent right
\ finally, we restore the sign
\ string of form 'integer'.'fractioal'e'exp'

: >float ( c-addr u-length -- float flag )
  sign? >r                           ( adr length R: isneg)
  \ get exponent -- this is the number that follows e
  [char] e extract 0=
  if 2drop false rdrop exit then     ( adr length exp R: isneg )
  rot !p>r                           ( length exp, R:isneg P, P:adr )
  swap >r 0. r>                      ( exp d-0 length, R:isneg P, P:adr )
  >r false nfswap r>       ( exp after_decimal d-0 length, R: isneg P, P:adr )
  for ( exp after_decimal d-0 R:isneg P count P:adr)
    \ get next character
    pc@  ( exp after_decimal d-sum char )
    [char] . = if
      fnswap 1- nfswap ( exp after_decimal d-sum )
    else
      2dup #214748364. d> if \ d-sum can't hold any more
        fnover if \ there's nothing left to do if we're after the decimal place
          2drop rdrop r>p rdrop false exit
        else \ we're before the decimal place, so add one to the exponent
          >r >r >r 1+ r> r> r>
        then
      else
        pc@ [char] 0 [char] 9 1+ within if
          d10* pc@ [char] 0 - m+ ( exp bool-after_decimal d-sum R:isneg P:adr)
          fnover if ( exp after_decimal d-sum R:isneg P:adr)
            >r >r >r 1- r> r> r> \ decrement the exponent by one
          then
        else \ it's not a digit, abort
          2swap 2drop rdrop r>p rdrop false exit
        then
      then
    then p+
  next ( exp after_decimal d-sum, R: isneg P:adr )
  r>p  ( exp after_decimal d-sum, R: isneg )
  >r >r drop r> r> ( exp d-sum, R: isneg )
  d>f fnswap       ( f-sum exp, R: isneg )
  f10^n f*         \ take care of the exponent ( f, R: isneg )
  r> negateiftrue true ;  \ take care of negative sign

\ Add float parsing capability to INTERPRET
: finit ['] >float to float? ;
' finit is turnkey
finit
