\ *********************************************************************
\                                                                     *
\    Filename:      idump.txt                                         *
\    FlashForth:    4.8                                               *
\    MCU            PIC 24 30 33                                      *
\    Application:                                                     *
\                                                                     *
\    Author:        Pete Zawasky                                      *
\    Created:       Friday, February 06 2009 - 9:40  ppz              *
\    Last Edit      Friday, August 19 2011      min                   *
\                                                                     *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

-idump
marker -idump

\ Display the contents of program space in FLASH memory,
\  given the starting address and length.
\ In dsPIC FF, program space addr range is $2000 to $fbff .
\ Displays in hex notation and printable ASCII.
\ idump expects base to be hex.
\
\ u.r+  ( u +n -- )    \ display u unsigned in field of +n
                       \ with no trailing space
: u.r+  0 swap <# 1- for # next #s #> type ;

\ Instruction Memory Dump.
\
: idump  ( addr +n -- )
   swap $fffe and         \ start on even address
   swap $10 u/ 1+         \ number of rows to print
\   dup 0= if drop 1 then \ Always print at least one line
   for
     cr dup 4 u.r+ [char] : emit space  \ display row addr
     $8                \ number of instructions per row
     for
       dup cf@ 2 u.r+ 4 u.r 2+  \ print 1 row of program space
     next
     $10 -             \ wind back the addr
     $8                \ number of instructions
     for               \ print ASCII
       dup cf@ swap drop
       over c@+ swap c@ rot
       3
       for
         >pr emit
       next 2+
     next
   next
   drop cr  ;

