\ *******************************************************************
\    Filename:      blink_pic30.txt                                 *
\    Date:          05.01.2014                                      *
\    FlashForth:    5.0                                             *
\    MCU:           PIC 30F                                         *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
-blink
marker -blink
hex ram

$02d6 constant latd
$02d2 constant trisd

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
