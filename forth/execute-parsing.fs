
\ Simple execute-parsing
: execute-parsing ( c-addr u xt -- )
  'source 2@ >r >r >in @ >r
  >r pad place pad c@+ 'source 2! 0 >in !
  r> execute
  r> >in ! r> r> 'source 2!
;

