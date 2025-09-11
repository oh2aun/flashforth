\ Transient definitions.  Load TRANSIENT at 8000 - size absolute address
\ (e.g. assembler) and DISCARD when no longer needed.
\ This version is a workaround for FF older than 08-2025 that could not
\ compile code to a virtual address higher than $8000+PFL.

\ Size must be large enough to contain the needed transient code size.

: transient ( size -- ) 
  flash
    latest @  align here  >r >r
    ( size ) aligned  $8000 pfl +  swap -  dp !
    latest @ ,  here $2081 ,  latest !  r> , r> ,
  ram ;
  
: tfind ( -- xt ) ram hi 1-  $2001 over  !  find ?abort? ;

: permanent ( -- )  
  tfind 
  flash 
    cell+ @ dp !
    latest @ , 
    here $2000 , latest !
  ram ;

: discard ( -- ) tfind 2@ ! ;

\ example
\ $100 transient
\ : foo ." This is a transient word" ;
\ permanent
\ : bar ." This is a permanent word" ;
\ discard

