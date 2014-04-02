\ *******************************************************************
\                                                                   *
\    Filename:      2literal.txt                                    *
\    Date:          21.03.2013                                      *
\    FF Version:    5.0                                             *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************

: 2literal ( x x -- )
  swap postpone literal postpone literal postpone ; immediate

