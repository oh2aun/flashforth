\ *********************************************************************
\                                                                     *
\    Filename:      jmptbl-test.txt                                   *
\    FlashForth:    5.0                                               *
\                                                                     *
\    Author:        Pete Zawasky                                      *
\    Created:       Tuesday, January 15 2008 - 18:50  ppz             *
\    Last Edit      Tuesday, January 29 2008 - 12:25  ppz             *
\                                                                     *
\ *********************************************************************
\  Based on jt.fth by Mikael Nordman, Jump_Table by Haskell           *
\ *********************************************************************
\  FlashForth is licensed acording to the GNU General Public License  *
\ *********************************************************************

hex

\ Create an execution table with n entries.
\ Each entry consists of 'nn' cell sized comparison value
\   and 'an' the address of the corresponding word to be executed.
\ At least two entries must be provided, the last one being the
\   default action.
\
\ Jump Table (from Haskell)
\ Example:
\
\    JUMP_TABLE do.key
\               control H  |  bkspace
\               control Q  |  quit
\               HEX 2B     |  escape  DECIMAL
\               DEFAULT    |  chrout
\ Useage:
\    do.key  ( n -- )   \ enter with n=code-to-match on TOS
\

\ *********************************************************************
hex ram

: .1st    ( -- )
  ." First "
;

: .2nd    ( -- )
  ." Second "
;

: .3rd    ( -- )
  ." Third "
;

: .4th    ( -- )
  ." Default "
;

jumptable do_test
          $00    | .1st
          $01    | .2nd
          $02    | .3rd
          default| .4th

ram
1 do_test
2 do_test
9 do_test

\ 1 do_test Second ok <16,2>
\ 2 do_test Third ok <16,2>
\ 9 do_test Default ok <16,2>


