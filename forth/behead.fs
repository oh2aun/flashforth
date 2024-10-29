\ Hide word names in the user dictionary

: behead ( from to -- )
  ' c>n ' c>n 2dup u< 
  if swap 
  then 2- @ swap latest
  begin 2dup @ - 
    while @ dup 
    while 2- 
  repeat 
  then nip ! ;

