: $cat ( straddr1, count1, straddr2, count2 --- straddr1, count )
\ concatenate strings     \ straddr1 must be in RAM
  >r >r                   \ -- straddr1, count1   R-- count2, straddr2
  2dup + r> swap r@ cmove \ -- straddr1, count1  R-- count2 
  r> + 
  over 1- over swap c! ;  \ store new count in case we concatenate again


