\ *********************************************************************
\    Filename:      see.txt                                           *
\    Date:          20.03.2017                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega                                            *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-see 
marker -see
hex ram
: *@ dup @ ;
: u.4 4 u.r ;
: *@+ dup cell+ @ u.4 ;
: 5sp 5 spaces ;
: @braddr ( addr -- addr xt-addr )
    *@ fff and dup 800 and 
    if f800 or then 2* over +  cell+ ;
: @xtaddr ( addr -- addr xt-addr )
  dup cell+ @ xa> ;
: .rjmp ( addr -- addr+2 ) @braddr u.4 cell+ ;
: .br  ( addr -- addr+2 )
     *@ 3 rshift 7f and dup 40 and 
     if ff80 or then 2* over + cell+ u.4 cell+ ;
: .reg ( addr -- addr ) 
   dup @ 4 rshift 1f and ." r" decimal 2 u.r hex cell+ ;
: .ldi ( addr -- addr )
  *@ dup 4 rshift dup 000f and 0010 + 
  ." r" decimal 2 u.r hex
  00f0 and swap 000f and + 2 u.r cell+ ;
: ?call ( addr -- addr f ) *@ fe0e and 940e - ;
: ?ret ( addr -- addr f ) *@ 9508 - ;
: ?rcall ( addr -- addr f ) *@ f000 and d000 - ;
: ?jmp  ( addr -- addr f ) *@ fe0e and 940c - ;
: ?rjmp ( addr -- addr f ) *@ f000 and c000 - ;
: ?breq ( addr --  addr f ) *@ fc07 and f001 - ;
: ?brne ( addr --  addr f ) *@ fc07 and f401 - ;
: ?brcc ( addr -- addr f ) *@ fc07 and f400 - ;
: ?pop ( addr -- addr f ) *@ fe0f and 900f - ;
: ?push ( addr -- addr f ) *@ fe0f and 920f - ;
: ?st-y ( addr -- addr f ) *@ fe0f and 920a - ;
: ?ldy+ ( addr -- addr f ) *@ fe0f and 9009 - ;
: ?ijmp ( addr -- addr f ) *@ 9409 - ;
: ?ldi ( addr -- addr f ) *@ f000 and e000 - ;
: (see) ( addr -- addr' | false )
  dup u.4
  *@ u.4
  ?call 0= if *@+ ." call  " @xtaddr c>n .id cell+ cell+ else 
  ?rcall 0= if 5sp ." rcall " @braddr c>n .id cell+ else
  ?breq 0= if 5sp ." breq  " .br else
  ?brne 0= if 5sp ." brne  " .br else
  ?brcc 0= if 5sp ." brcc  " .br else
  ?rjmp 0= if 5sp ." rjmp  " .rjmp else
  ?ijmp 0= if 5sp ." ijmp" drop false else
  ?ret  0= if 5sp ." ret"  drop false else
  ?jmp  0= if *@+ ." jmp   " @xtaddr c>n .id drop false else
  ?pop  0= if 5sp ." pop   " .reg else
  ?push 0= if 5sp ." push  " .reg else
  ?ldy+ 0= if 5sp ." ld    " .reg ." y+" else
  ?st-y 0= if 5sp ." st    -y " .reg else
  ?ldi  0= if 5sp ." ldi   " .ldi else
  cell+
  then then then then then
  then then then then then
  then then then then
  cr ;

: dis ( addr -- )
  hex cr
  begin (see) dup 0=
  until drop ;

: see ( "word" -- )  ' dis ;
hex ram

