\ *********************************************************************
\                                                                     *
\    Filename:      xdump.txt                                         *
\    FlashForth:    5.0                                               *
\    MCU            PIC18                                             *
\    Application:                                                     *
\                                                                     *
\    Author:        Mikael Nordman                                    *
\                                                                     *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

-xdump
marker -xdump

\ Display the contents of raw FLASH memory,
\ given the starting address and length.
\ The address is a raw address without mapping
\ Displays in hex notation and printable ASCII.
\ xdump expects base to be hex.
\

: ud.r <# 1- for # next #s #> type ;
: u.2 $ff and 0 2 ud.r space ;
: xx@ 2dup x@ dup ;

\ Extended Memory Dump.
\
: xdump  ( d.addr +n -- )
   rot $fffe and           \ start on even address
   rot rot $10 u/          \ number of rows to print
   for
     cr 2dup 6 ud.r 
     [char] : emit space   \ display row addr
     $8 for                \ number of instructions per row
       xx@                 \ print 1 row of program space
       u.2 8 rshift u.2
       2 m+
     next
     -$10 m+               \ wind back the addr
     $8 for                \ print ASCII
       xx@
       >pr emit $8 rshift >pr emit
       2 m+
     next
   next
   2drop cr  ;

