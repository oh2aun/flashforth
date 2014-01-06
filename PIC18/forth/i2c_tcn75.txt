\ *********************************************************************
\                                                                     *
\    Filename:      i2c_tcn75.txt                                     *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ *****************************************
\ Read temp from tcn75 temp sensor
-tcn75
marker -tcn75

\ set the address for your device here.
\ $9b constant addr-tcn75
$91 constant addr-tcn75

: temp ( -- n )
  i2cinit
  addr-tcn75 i2cws
  i2c@nak dup $7f > if $ff80 or then
;

\ Average temperature over 20 samples
: avt ( -- n )
  i2cinit
  0
  #20
  for
    #50 ms
    addr-tcn75 i2cws i2c@nak +
  next
  2+ #20 / dup $7f > if $ff80 or then
;

: bt temp ." The temperature is " . ." C" cr
;
: dt avt ." The temperature is " . ." C" cr
;


: btc decimal begin bt #4000 ms key? until ;
: dtc decimal begin dt key? until ;
ram
