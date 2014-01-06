\ *********************************************************************
\                                                                     *
\    Filename:      pick.txt                                          *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ xu ... x0 u -- xu ... x0 xu
: pick 2* #3 + sp@ swap - @ ;
