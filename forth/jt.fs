\ *********************************************************************
\                                                                     *
\    Filename:      jt.txt                                            *
\    Date:          27.10.2020                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ create an execution table with n entries
\ each entry consists of 'nn' cell sized comparison value
\ and 'an' the address of the corresponding word to be executed.
\ At least two entries must be provided, the last one beeing the
\ default action. 
\ ' default ' first-entry 3 ' 2nd-entry 5 2 jt:
-jt:
marker -jt:

: jt: ( an nn n -- ) \ compile an jump table table
     ( m -- )        \ execute aword corresponding to m
  flash create
  dup ,                \ store the table size
  for
    , ,                \ store an entry
  next
  ,                   \ store the default entry
  ram
  does>                \ m addr 
  dup @                \ m a n
  for
    cell+              \ Point to value
    2dup @ =           \ m a flag
    if                 \ m a
      endit            \ a match was found
    else
      cell+            \ Point to execution address
    then
  next
  \ Execute the action.
  nip cell+ @ex
;


