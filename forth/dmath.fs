\ *********************************************************************
\                                                                     *
\    Filename:      dmath.fs                                          *
\    Date:          21.04.2023                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

\ Signed double number  multiplication
: d* ( d d - d )
  dup >r rot dup >r rot rot
  dabs 2swap dabs uq* 2drop
  r> r> xor ?dnegate
;

\ Signed double number division and remainder
: d/mod ( d d -- drem dquot )
  dup >r 2swap dup r> xor >r dup >r 2swap
  >r >r dabs d>q r> r> dabs uq/mod
  2swap r> ?dnegate
  2swap r> ?dnegate
;

\ Signed double number divide
: d/ ( d d  -- dquot ) d/mod >r nip nip r> ;

\ Shift right a double number
: d>> ( ud n -- ud ) for d2/ $7fff and next ;

\ Shift left a double number
: d<< ( ud n -- ud ) for d2* next ;

\ d+ exists in core FF
\ d- exists in core FF
