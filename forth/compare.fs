\ n=0 match; otherwise sign(n) = lexical order
: compare ( c-addr1 u1 caddr2 u2 -- n )
  rot over - dup  \ 
  if  nip
  else
     swap
     for                      \ c-addr1 c-addr2 n 
       drop
       over c@ over c@ - dup  \ c-addr1 c-addr2 n n
       if   endit 
       else >r 1+ swap 1+ r>
       then
     next
  then nip nip
;
