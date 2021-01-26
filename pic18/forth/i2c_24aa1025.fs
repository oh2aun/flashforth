\ *********************************************************************
\                                                                     *
\    Filename:      i2c_24aa1025.txt                                  *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

\ ***********************************************
\ 24AA1025 128 Kbyte EEPROM, Only 64 KB supported by these words

-24aa1025
marker -24aa1025

$a0 constant addr-24aa1025

\ Read one byte from external eeprom
: eec@ ( addr -- c )
  i2cinit
  addr-24aa1025 i2c-addr2
  srsen                   \ Repeted start enable
  addr-24aa1025 1+ i2c!   \ write the read bit
  i2c@nak                 \ get data, send stop bit 
;


\ Write one byte to external eeprom
: eec! ( c addr -- )
  i2cinit
  addr-24aa1025 i2c-addr2
  i2c!                 \ c
  spen                 \ pen   send stop bit
;


