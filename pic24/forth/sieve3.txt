\ *******************************************************************
\                                                                   *
\    Filename:      sieve3.txt                                      *
\    Date:          15.02.2015                                      *
\    FF Version:    5.0                                             *
\    MCU:           PIC 24 30 33                                    *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ This normal sieve requires 8 KBytes of RAM.
\ It will not run on most PICs due to lack of memory.
\ sieve2 and sieve3 require 1 Kbyte of RAM.

-sieve3
marker -sieve3
decimal ram
8192 constant size3 inlined
ram variable flags3 inlined size3 8 / allot

: BTST ( addr bit-index -- )
  dup >r 4 inline rshift 2* + r> inline btst 
;

: BCLR ( addr bit-index -- )
  dup >r 4 inline rshift 2* + r> inline bclr 
;

: sieve3      
  flags3 [ size3 8 / ] literal -1 fill
  0 0 !p>r size3 
  for 
     flags3 @p BTST 
     if 
        @p 2*  3 +
        dup @p +
        begin  
          dup size3 u< 
        while  
          flags3 over BCLR
          over +
        repeat
        2drop 1+
     then
     p+
  next
  r>p   . ." primes " cr
;

: bench3 ticks sieve3 ticks swap - u. ." milliseconds" cr ;
