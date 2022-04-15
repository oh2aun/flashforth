\ *******************************************************************
\                                                                   *
\    Filename:      free.txt                                        *
\    Date:          14.04.2022                                      *
\    FF Version:    5.0                                             *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************

: unused hi here - 1+ ;
\ MCU with eeprom
: .free
  cr ." Flash:" flash unused u. ." bytes"
  cr ." Eeprom:" eeprom unused u. ." bytes"
  cr ." Ram:" ram unused u. ." bytes"
;

\ MCU without eeprom
: .free
  decimal
  cr ." Flash:" flash unused u. ." bytes"
  cr ." Ram:" ram unused u. ." bytes"
;

