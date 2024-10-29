\ Transient definitions.  Load TRANSIENT code in high memory
\ (e.g. assembler) and DISCARD when no longer needed.

: transient ( size -- ) 
  flash
    align latest @ here >r >r
    ( size ) aligned hi 1+ swap - dp !
    latest @ , here $2081 , latest ! r> , r> ,
  ram ;
  
: tfind ( -- xt ) ram hi 1- $2001 over ! find ?abort? ;

: permanent ( -- )  
  tfind 
  flash 
    cell+ @ dp !
    latest @ , 
    here $2000 , latest !  \ dummy def for discard
  ram ;

: discard ( -- ) tfind 2@ ! ;

\ example
\ $100 transient
\ : foo ;
\ permanent
\ : bar ;
\ discard

