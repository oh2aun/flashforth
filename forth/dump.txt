\ *******************************************************************
\                                                                   *
\    Filename:      dump.txt                                        *
\    Date:          14.11.2010                                      *
\    FF Version:    3.6 4.7                                         *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
: dump ( addr +n -- )
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
