\ Some tests for the Atmega assembler
\ needs the assembler and see
-asmtest
marker -asmtest
: asmtest ( n1 n2 -- ) 
  = if ." OK" else ." ERROR" then cr ;

#30 #28 mov, 
flash here ram 2- @ $2fec asmtest  \ Rd,Rr:

#17 #15 ldi, 
flash here ram 2- @ $e01f asmtest  \ Rd,k:

#17 $1234 lds, 
flash here ram 4 - @+ swap @ u. u. \ Rd: 9110 1234

$09 constant Y+
9 Y+ $31 ldd,
 flash here ram 2 - @ $a899 asmtest

$01 constant Z+
9 Z+ $31 ldd,
 flash here ram 2 - @ $a891 asmtest

\ Leave true flag if zero flag is true
: testif0
  [ sez,    ] \ Set zero flag
  [ eq, if, ] \ if zero
      true
  [ else, ]   \ else not zero
      false
  [ then, ]
;
testif0 .

\ Leave true flag if zero flag is false
: testif1 
  [ clz,    ]      \ Clear zero flag
  [ eq, not, if, ] \ if not zero
      true
  [ else, ]        \ else zero 
      false
  [ then, ]
;
testif1 .

\ Increment 24 bit value until result is zero
: testuntil
  [ #16 #0 ldi, ]
  [ #17 #0 ldi, ]
  [ #18 #1 ldi, ]
  [ begin, ]
  [   #16 #6 add, ] \ R6 contains 1
  [   #17 #5 adc, ] \ R5 contains 0
  [   #18 #5 adc, ]
  [ eq, until, ]    \ until R18 is zero
;
testuntil

