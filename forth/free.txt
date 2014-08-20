\ *******************************************************************
\                                                                   *
\    Filename:      free.txt                                        *
\    Date:          06.01.2014                                      *
\    FF Version:    5.0                                             *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************

\ MCU with eeprom
: .free
  cr ." Flash:" flash hi here - u. ." bytes"
  cr ." Eeprom:" eeprom hi here - u. ." bytes"
  cr ." Ram:" ram hi here - u. ." bytes"
;

\ MCU without eeprom
: .free
  decimal
  cr ." Flash:" flash hi here - u. ." bytes"
  cr ." Ram:" ram hi here - u. ." bytes"
;

