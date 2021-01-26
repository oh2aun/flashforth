\ *********************************************************************
\                                                                     *
\    Filename:      core.txt                                          *
\    Date:          31.12.2013                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Some extra core words

: forget ( --- name )
  bl word latest @ (f) ?abort?
  c>n 2- dup @ ?abort?
  dup flash dp ! @ latest ! ram
;

