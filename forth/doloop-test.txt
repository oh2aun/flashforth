\ test some do loop words
-test
marker -test
decimal

: tdo0 3 0 do cr i . loop ;
: tdo1 do i . i 5 = if leave then loop cr ." leaving" ;
: tdo2 do 10 0 do j . i . loop loop ;
: tdo3 ?do i . 1 +loop cr ." leaving" ;
: tdo4 do i . 10 +loop ;
: tdo5 do i . -10 +loop ;
: tdo ticks  #30000 0 do  loop  ticks swap - u. ;
: tfor ticks #30000   for next  ticks swap - u. ;
\ 
cr
tdo0
cr
10 0 tdo1 
cr
3 0 tdo2 
cr
0 0 tdo3 
cr
10 0 tdo3 
cr
100 0 tdo4 
cr
0 100 tdo5 
cr
tdo  
cr
tfor 
