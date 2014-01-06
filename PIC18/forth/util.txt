\ Check of some calls could be relative calls
-rcall?
marker -rcall?
hex ram

: *@ dup @ ;
: u.4 4 u.r ;
: ?call ( addr -- addr f ) *@ fe00 and ec00 = ;
: @xtaddr ( addr -- addr xt-addr ) dup dup c@ swap 2+ @ 8 lshift + 2* ;
\ print instances of call that can be rcall in the kernel
: rcall? ( addr n -- ) 
  for
    ?call
    if
      dup @xtaddr - abs $800 <
      if
        cr dup u.4 cr @xtaddr c>n .id cell+ 
      then
    then
    2+
  next
  drop
;


