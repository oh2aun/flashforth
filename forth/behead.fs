\ Hide word names in user defined words

: behead ( from to -- )
  ' c>n ' c>n 2dup u< 
  if swap then 
  2- @ swap latest
  begin 2dup @ - 
    while @ dup 
    while 2- 
  repeat 
  then nip ! ;
\ Hide word names in user defined words
: behead ( from to -- ) ' c>n 2- @ ' c>n 2- swap ! ; 
