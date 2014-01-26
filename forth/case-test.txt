-case-test
marker -case-test
ram hex

: case-test
  case
  2 of ." two " 2222 endof
  3 of ." three " 3333 endof
  default  ." default " 9999 endof
  endcase
  u.
;

2 case-test
3 case-test
8 case-test

: case-test2
  case
  11 of endof
  default endof
  endcase
;
