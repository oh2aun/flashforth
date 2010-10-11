\ *******************************************************************
\                                                                   *
\    Filename:      dump.fth                                        *
\    Date:          10.2.2008                                       *
\    FF Version:    3.3 4.2                                         *
\    MCU:           PIC18F dsPIC30F                                 *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
: dump1 ( addr +n -- )
  $10 u/
  for
    cr dup 4 u.r [char] : emit \ display row addr
    $10
    for                       \ display bytes
      c@+ 2 u.r
    next
    $10 -
    $10
    for                       \ display ASCII
      c@+ >pr emit
    next
  next
  drop cr
;
