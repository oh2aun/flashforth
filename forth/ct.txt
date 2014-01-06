\ *********************************************************************
\                                                                     *
\    Filename:      ct.txt                                            *
\    Date:          06.01.2014                                        *
\    File Version:  5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ create an condition table with n entries
\ each entry consists of a comparison word
\ and an execution word, which is executed if
\ the comparison word leaves a true value on the stack.
-ct
marker -ct
hex ram
: ct ( ew cw n -- ) \ compile a condition table
     ( m -- m )     \ execute aword corresponding to m.
                    \ m may consist of several stack cells
                    \ it is upto the condition word to
                    \ preserve m on the stack
    create
    dup ,                  \ store the condition table size
    for
      , ,                  \ store an entry
    next
  does>                    \ m addr
    dup @                  \ m addr n
    for
      cell+ dup            \ m addr addr
      cell+ >r             \ m addr
      @ex                  \ m flag
      if                   \ m
        r> @ex rdrop exit  \ m     a match was found
      then 
      r>
    next
    drop
;
