\ *********************************************************************
\    Case for FlashForth                                              *
\    Filename:      case.txt                                          *
\    Date:          26.01.2014                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ A case implementation posted by Jenny Brien on c.l.f.
\ Modified to use for..next instead of do..loop

-case
marker -case
hex ram

\ of compare
: (of) ( n1 n2 -- n1 flag ) 
  inline over
  inline -
  0=
;

: case  (  -- #of )
  0
; immediate

: of  ( #of -- #of orig )  
    postpone (of)  ( copy and test case value)
    postpone if    ( add orig to control flow stack )
    postpone drop  ( discard case value if case is matching )
; immediate

: default ( #of -- #of orig )
  postpone true      ( Force to take the default branch )
  postpone if        ( add orig to control flow stack )
  postpone drop      ( discard case value )
; immediate

: endof ( orig1  -- orig2 #of )
   postpone else 
   swap 1+
; immediate

: endcase  ( orig1..orign #of  -- )
    postpone drop  ( discard case value )
    for
      postpone then ( resolve of branches )
    next
; immediate

