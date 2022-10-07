\ *********************************************************************
\                                                                     *
\    Filename:      i2c-base.txt                                      *
\    Date:          05.10.2022                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega 328 32u4 2560 1280 640                                        *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-i2c-base
marker -i2c-base
hex ram

\ Two-Wire-Interface Registers
\ Check the register addressess from the datasheet.
$b8 constant TWBR
$b9 constant TWSR
$bb constant TWDR
$bc constant TWCR

\ Bits in the Control Register
%10000000 constant mTWINT
%01000000 constant mTWEA
%00100000 constant mTWSTA
%00010000 constant mTWSTO
%00001000 constant mTWWC
%00000100 constant mTWEN
%00000001 constant mTWIE

eeprom #100 value i2c.speed \ 100 KHz i2c
ram
: i2c.init ( -- )
  %11 TWSR mclr \ prescale value = 1
  Fcy i2c.speed / #16 - 2/ TWBR c!
  mTWEN TWCR mset
;

: i2c.wait ( -- ) \ Wait for operation to complete
  \ When TWI operations are done, the hardware sets 
  \ the TWINT interrupt flag, which we will poll.
  \ Watchdog timeout
  7 wd+ begin mTWINT TWCR mtst until  wd-
;

: i2c.start ( -- ) \ Send start condition
  [ mTWINT mTWEN or mTWSTA or ] literal TWCR c!
  i2c.wait
;

: i2c.rstart ( -- ) \ Send repeated start condition
  i2c.start \ AVR doesn't distinguish
;

: i2c.stop ( -- ) \ Send stop condition
  [ mTWINT mTWEN or mTWSTO or ] literal TWCR c!
;

: i2c.ack? ( -- flag )
  \ Test for arrival of an ACK depending on what was sent.
  TWSR c@ $f8 and $18 =     \ SLA+W
  TWSR c@ $f8 and $28 = or  \ data byte
  TWSR c@ $f8 and $40 = or  \ SLA+R
  if true else false then
;
\ Write one byte to bus, returning false if ACK was received, -1 otherwise.
: i2c.c! ( c -- flag )
  i2c.wait \ Must have TWINT high to write data
  TWDR c!
  [ mTWINT mTWEN or ] literal TWCR c!
  i2c.wait
;

\ Read one byte and ack for another.
: i2c.c@.ack ( -- c )
  [ mTWINT mTWEN or mTWEA or ] literal TWCR c!
  i2c.wait
  TWDR c@
;

\ Read one last byte.
: i2c.c@.nack ( -- c ) 
  [ mTWINT mTWEN or ] literal TWCR c!
  i2c.wait
  TWDR c@
;

\ Address slave for writing, leaving true if slave ready.
: i2c.write ( 7-bit-addr -- )
  2*            \ Build full byte with write-bit as 0
  i2c.start i2c.c!
;

\ Address slave for reading, leaving true if slave ready.
: i2c.read ( 7-bit-addr -- )
  2* 1+         \ Build full byte with read-bit as 1
  i2c.start i2c.c!
;

\ Detect presence of device, leaving true if slave responded.
\ If the slave ACKs the read request, fetch one byte only.
: i2c.ping ( 7-bit-addr -- f )
  i2c.read i2c.ack? if i2c.c@.nack drop true 
                    else false 
                    then i2c.stop ;

