\ *********************************************************************
\                                                                     *
\    Filename:      bit.txt                                           *
\    Date:          08.04.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC24-30-33                                       *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Words for manipulating bits in Access Ram and in Banked ram 
-bit
marker -bit

\ Define a word that clears a bit in ram
\ The defined word can be inlined
( port bit "name" -- )
: bit0:
  : swap bclr, return, postpone [
;

\ Define a word that sets a bit in ram
\ The defined word can be inlined
( port bit "name" -- )
: bit1:
  : swap bset, return, postpone [
;

\ Define a word that leaves a true flag if a bit in ram is one
\ and a false flag if a bit is zero.
\ The defined word can be inlined
( port bit "name" -- )
: bit?:
  : ['] true  in, swap btss, ['] 1+ in, return, postpone [
;

