\ *******************************************************************
\                                                                   *
\    Filename:      sieve.txt                                       *
\    Date:          31.12.2013                                      *
\    FF Version:    5.0                                             *
\    MCU:           PIC 18 24 30 33 Atmega                          *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ This normal sieve requires 8 KBytes of RAM.
\ It will not run on most PICs due to lack of memory.
\ sieve2 requires 1 Kbyte of RAM.

-sieve
marker -sieve
decimal ram
8191 constant size inlined
ram align here size allot constant flags inlined
: sieve
  flags size 1 fill
  0 1 !p>r size 1-
  for
    flags @p + c@
    if                              
        @p dup + 3 + 
        dup @p +
        begin
           dup size  <
        while
           0 over flags + c!
           over +
        repeat
        drop drop 1+ 
     then
     p+
  next
  r>p
  . ." primes " cr ;

: bench ticks sieve ticks swap - u. ." milliseconds" cr ;

