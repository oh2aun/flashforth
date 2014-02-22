\ *******************************************************************
\                                                                   *
\    Filename:      sieve2.txt                                      *
\    Date:          22.02.2014                                      *
\    MCU:           PIC 18 24 30 33  Atmega                         *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ sieve2 requires 1 Kbyte of RAM.
-sieve2
marker -sieve2
decimal ram

 ( addr n c -- ) \ fill addr to addr+n with c
: fill rot !p>r swap for dup pc! p+ next r>p drop ;

8192 constant size2
ram variable flags2 size2 8 / allot
: bit-addr ( addr bit -- eff-addr )
  3 rshift  ( -- addr off)
  +         ( -- eff-addr) ;

: bit? ( addr bit -- f )
  swap over bit-addr swap ( -- eff-addr bit )
  7 and 1 swap lshift     ( -- eff-addr bitmask)
  swap c@ and             ( -- f) ;

: bit-reset ( addr bit -- )
  swap over bit-addr swap ( -- eff-addr bit )
  7 and 1 swap lshift     ( -- eff-addr bitmask)
  invert over c@ and swap c! ;

: sieve2      
  flags2 [ size2 8 / ] literal -1 fill
  0 0 !p>r size2 
  for 
     flags2 @p bit? 
     if 
        @p 2*  3 +
        dup  @p +
        begin  
          dup size2 u< 
        while  
          flags2 over bit-reset
          over +
        repeat
        2drop 1+
     then
     p+
  next
  r>p   . ." primes " cr
;

: bench2 ticks sieve2 ticks swap - u. ." milliseconds" cr ;

bench2

