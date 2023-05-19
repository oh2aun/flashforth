-compare
marker -compare
: compare ( c-addr1 u1 caddr2 u2 -- n )
  rot over -
  if nip
  else
    swap !p>r 0 swap
    for
      swap
      c@+ pc@ p+ xor rot xor
      dup if endit then
    next
    r>p
  then nip
;

