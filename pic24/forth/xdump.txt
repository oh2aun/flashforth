\ *********************************************************************
\                                                                     *
\    Filename:      xdump.txt                                         *
\    FlashForth:    5.0                                               *
\    MCU            PIC 24 30 33                                      *
\    Application:                                                     *
\                                                                     *
\    Author:        Mikael Nordman                                    *
\    Created:       22.11.2015                                        *
\                                                                     *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

-xdump
marker -xdump

\ Display the contents of raw FLASH memory

: ud.r <# 1- for # next #s #> type ;
: u.2 $ff and 0 2 ud.r ;
: xx@ 2dup x@ swap dup rot ;

\ Extended Memory Dump.
\
: xdump  ( d.addr +n -- )
   rot $fffe and           \ start on even address
   rot rot $10 u/          \ number of rows to print
   for
     cr 2dup 6 ud.r
     [char] : emit space   \ display row addr
     $8 for                \ number of instructions per row
       xx@
       u.2 >< u.2 u.2
       space 2 m+
     next
     -$10 m+               \ wind back the addr
     $8 for                \ print ASCII
       xx@
       >pr emit
       >< >pr emit
       >pr emit space
       2 m+
     next
   next
   2drop cr ;

