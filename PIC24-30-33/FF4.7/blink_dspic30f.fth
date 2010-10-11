\ *******************************************************************
\    Blinker for FlashForth 30F                                     *
\    Filename:      blink.fth                                       *
\    Date:          16.1.2009                                       *
\    FlashForth:    30F4.2                                          *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
-blink
marker -blink
hex ram

$02d6 con latd
$02d2 con trisd

: blink ( n -- )
  1 trisd mclr
  begin
    1 latd mclr  \ Port D.0
    dup ms
    1 latd mset  \ Port D.0
    dup ms
    key?
  until
  key 2drop 
;
ram
: 99blink 99 blink ;
' 99blink is turnkey
warm
