\ *********************************************************************
\                                                                     *
\    Filename:      bit.txt                                          *
\    Date:          28.03.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Words for manipulating bits in Access Ram and in Banked ram 
-bit
marker -bit

\ Need movlb in the assembler
$0100 as1 movlb,        ( k -- )  

\ Create the bit word, compile movlb if needed 
( port bit -- port bit banked/access )
: (bit)
  :
  over $f040 $ff60 within
  if
    over #8 rshift $f and movlb, 
    1    \ Banked ram 
  else
    0    \ Access ram
  then
;
\ Define a word that clears a bit in ram
\ Can be inlined
( port bit "name" -- )
: bit0:
  (bit) bcf, $12 i, postpone [
;

\ Define a word that sets a bit in ram
\ Can be inlined
( port bit "name" -- )
: bit1:
  (bit) bsf, $12 i, postpone [
;

\ Define a word that leaves a true flag if a bit in access ram is one
\ and a false flag if a bit is zero.
\ Can NOT be inlined
( port bit "name" -- )
: bit?:
  (bit) btfss,
  ['] false goto,
  ['] true goto,
  postpone [
;

