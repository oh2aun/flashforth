\ *********************************************************************
\                                                                     *
\    Filename:      jumptable.txt                                     *
\    FlashForth:    5.0                                               *
\    Application:   FP                                                *
\                                                                     *
\    Author:        Pete Zawasky                                      *
\    Created:       Tuesday, January 15 2008 - 18:50  ppz             *
\    Last Edit      Tuesday, 27.10.2020            Mikael             *
\                                                                     *
\ *********************************************************************
\  Based on jt.fth by Mikael Nordman, Jump_Table by Haskell           *
\ *********************************************************************
\  FlashForth is licensed acording to the GNU General Public License  *
\ *********************************************************************

-jumptable:
marker -jumptable:

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
\    jumptable: do.key
\               controlH  |  bkspace
\               controlQ  |  quit
\               HEX 2B    |  escape  DECIMAL
\                  default|  chrout
\ Useage:
\    do.key  ( n -- )   \ enter with n=code-to-match on TOS
\

\ Create a jump table.
\
: jumptable:   ( -- )   \ compile an execution table
              ( m -- ) \ execute a word corresponding to m
  flash                \ The jumptable goes into flash
  create
    here 0 dup ,       \ initial test_cnt stored at pfa
                       \ ( addr count -- )
  does>                \ ( m addr -- )
    dup @              \ ( m a cnt -- )
    for
      cell+            \ point to test value
      2dup @ =         \ ( m a flag -- )
      if
        endit          \ a match was found
      else
        cell+          \ ( m a' -- ) point to execution address
      then
    next
    nip cell+ @ex      \ execute the word
;

\ Use the words | and default| to fill jump table.
\
: |  ( addr count nn -- addr count )
  , ' ,              \ store m (match) and cfa in table
  1+                 \ increment count
;

: default| ( addr count -- )
  swap ! ' ,  ram   \ store default word cfa in table
;

ram



