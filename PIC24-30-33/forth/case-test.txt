-case-test
marker -case-test
ram hex

: case-test
  case
  2 of ." two" cr endof
  3 of ." three" endof
  ." default"
  endcase
;
2 case-test
3 case-test
8 case-test
