\ *********************************************************************
\                                                                     *
\    Filename:      i2c-tcn75.txt                                     *
\    Date:          06.10.2022                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Read temp from tcn75 temp sensor
-tcn75
marker -tcn75

\ set the address for your device here.
$48 constant addr-tcn75

: temp ( -- n )
  i2c.init addr-tcn75 i2c.read i2c.c@.nack i2c.stop
  dup $7f > if $ff80 or then
;

