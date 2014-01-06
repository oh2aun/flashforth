\ *********************************************************************
\                                                                     *
\    Filename:      jmptbl.txt                                        *
\    FlashForth:    5.0                                               *
\    Application:   FP                                                *
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

-jmptbl
marker -jmptbl

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
\                   DEFAULT|  chrout
\ Useage:
\    do.key  ( n -- )   \ enter with n=code-to-match on TOS
\

\ Create a jump table.
\
: jumptable   ( -- )   \ compile an execution table
              ( m -- ) \ execute a word corresponding to m
  flash                \ The jumptable goes into flash
  create
    here 0 ,           \ initial test_cnt stored at pfa
                       \ ( addr -- )
  does>                \ ( m addr -- )
    dup @              \ ( m a cnt -- )
    for
      cell+
      2dup @ =         \ ( m a flag -- )
      if               \ a match was found
        nip cell+ @ex  \ execute the matched word
        rdrop exit     \ and exit
      then
      cell+            \ ( m a -- ) point to next nn to test
    next
    nip cell+ @ex      \ execute the default word
;

\ Use the words | and default| to fill jump table.
\
: |  ( addr nn -- addr )
  , ' ,              \ store m (match) and cfa in table
  1 over +!          \ increment test_cnt at pfa
;

: default| ( addr -- )
  drop ' ,             \ store default word cfa in table
;

ram



