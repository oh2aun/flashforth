\ *********************************************************************
\                                                                     *
\    Filename:      bit.txt                                           *
\    Date:          06.01.2015                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega                                            *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Words for manipulating bits in ram.
\ Memory mapped addresses of I/O ports must be used.
\ CBI SBI SBIS instructions will be generated for adresses $20-$3f
\ IN and OUT instruction will be used for addresses $40 to $5f
\ LDS and STS instructions will be used for addresses over $60
\ Bit has value 0..7

-bit
marker -bit
: (bio) ( c-addr -- in/out-addr ) $20 - dup $5 lshift or $60f and ;
: (bit) ( c-addr bit flag "name" -- )
  : >r
  over $40 < if
    swap $20 - 3 lshift or
    r> 
    if    $9a00   \ sbi io-addr, bit
    else  $9800   \ cbi io-addr, bit
    then  or i,
  else
    over $60 < 
    if    over (bio) $b100 or   \ in r16 io-addr
    else  $9100 i, over         \ lds r16 c-addr
    then  i, 
    1 swap lshift 
    r>
    if   $6000 >r
    else $7000 >r invert $ff and
    then dup 4 lshift or $f0f and r> or i, \ andi/ori r16, mask
    dup $60 < 
    if   (bio) $b900 or         \ out io-addr r16 
    else $9300 i,               \ sts c-addr r16
    then i,
  then 
  $9508 i,            \ return
  postpone [
;

\ Define a word that clears a bit in ram
\ The defined word can be inlined
( c-addr bit "name" -- )
: bit0: false (bit) ;

\ Define a word that sets a bit in ram
\ The defined word can be inlined
( c-addr bit "name" -- )
: bit1: true (bit) ;

\ Define a word that leaves a true flag if a bit in ram is one
\ and a false flag if a bit is zero.
\ The defined word can be inlined
( c-addr bit "name" -- )
: bit?:
  :
  $939a i, $938a i, $ef8f i, $ef9f i, \ true
  over $40 < if   
    swap $20 - 3 lshift or $9b00 or i, \  sbis io-addr, bit   
  else 
    over $60 < 
    if swap (bio) $b100 or      \ in r16 io-addr
    else $9100 i, swap          \ lds r16 c-addr
    then i, $ff00 or i,         \ sbrs r16, bit
  then
  $9601 i,            \ 1+
  $9508 i,            \ return
  postpone [
;
