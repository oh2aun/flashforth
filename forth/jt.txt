\ *********************************************************************
\                                                                     *
\    Filename:      jt.txt                                            *
\    Date:          06.01.2014                                         *
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
-jt
marker -jt

: jte nip cell+ @ex ;
: jt ( an nn n -- ) \ compile an execution table
     ( m -- )       \ execute aword corresponding to m
  create
  dup 1- ,             \ store the table size
  for
    , ,                \ store an entry
  next
  does>                \ m addr 
  dup @                \ m a n
  for
    cell+
    2dup @ =           \ m a flag
    if
      \ a match was found
      jte rdrop exit
    then
    cell+             \ m a
  next
  \ Execute the default action.
  cell+ jte
;
ram

