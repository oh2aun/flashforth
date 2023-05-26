\ n=0 match; otherwise sign(n) = lexical order
: compare ( c-addr1 u1 caddr2 u2 -- n )
  rot over - dup
  if nip nip
  else
    rot !p>r swap
    for
      drop
      c@+ pc@ p+ -
      dup if endit then
    next
    r>p
  then nip
;

