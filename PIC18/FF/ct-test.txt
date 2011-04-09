\ USAGE EXAMPLE
: ?9 dup 9 = ;
: ?6 dup 6 = ;
: .9 ." nine" cr ;
: .6 ." six" cr ;
' .6 ' ?6 ' .9 ' ?9 
2 flash ct test ram

\ WITH noname:

:noname ." default" cr ; 
' true
:noname ." six" cr ;
:noname dup 6 = ;
:noname ." nine" cr ;
:noname dup 9 = ;
3 flash ct testnoname

6 test
6 testnoname
.
.
