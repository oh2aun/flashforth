\ n=0 match; otherwise sign(n) = lexical order
: n1+ swap 1+ swap ;
: compare ( c-addr1 u1 caddr2 u2 -- n )
  rot swap 2dup - >r min 0 swap
  for  drop over c@ over c@ - >r 1+ n1+ r> 
       dup if endit then 
  next nip nip
  dup if rdrop else drop r> then ;


