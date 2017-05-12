\ i2c-base-avr.txt
\ Low-level words for TWI/I2C on Atmega328P.
\
\ Modelled on i2c-twi.frt from amforth, 
\ i2c_base.txt for FlashForth on PIC18
\ and the Atmel datasheet, of course.
\ Peter J.  2014-10-27
\ Watchdog added Mikael Nordman @ 12.5.2017

-i2c-base
marker -i2c-base
hex ram

\ Two-Wire-Interface Registers
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

: i2c.init ( -- ) \ Set clock frequency to 100kHz
  %11 TWSR mclr \ prescale value = 1
  [ Fcy #100 / #16 - 2/ ] literal TWBR c!
  mTWEN TWCR mset
;

: i2c.wait ( -- ) \ Wait for operation to complete
  \ When TWI operations are done, the hardware sets 
  \ the TWINT interrupt flag, which we will poll.
  \ Watchdog timeout
  7 wd+ begin TWCR c@ mTWINT and until  wd-
;

: i2c.start ( -- ) \ Send start condition
  [ mTWINT mTWEN or mTWSTA or ] literal TWCR c!
  i2c.wait
;

: i2c.rsen ( -- ) \ Send repeated start condition
  i2c.start \ AVR doesn't distinguish
;

: i2c.stop ( -- ) \ Send stop condition
  [ mTWINT mTWEN or mTWSTO or ] literal TWCR c!
;

\ Write one byte to bus, returning 0 if ACK was received, -1 otherwise.
: i2c.c! ( c -- f )
  i2c.wait \ Must have TWINT high to write data
  TWDR c!
  [ mTWINT mTWEN or ] literal TWCR c!
  i2c.wait
  \ Test for arrival of an ACK depending on what was sent.
  TWSR c@ $f8 and $18 = if 0 exit then \ SLA+W
  TWSR c@ $f8 and $28 = if 0 exit then \ data byte
  TWSR c@ $f8 and $40 = if 0 exit then \ SLA+R
  -1 \ Something other than an ACK resulted
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
: i2c.addr.write ( 7-bit-addr -- )
  2*            \ Build full byte with write-bit as 0
  i2c.start i2c.c! if false else true then
;

\ Address slave for reading, leaving true if slave ready.
: i2c.addr.read ( 7-bit-addr -- )
  2* 1+         \ Build full byte with read-bit as 1
  i2c.start i2c.c! if false else true then
;

\ Detect presence of device, leaving true if slave responded.
\ If the slave ACKs the read request, fetch one byte only.
: i2c.ping? ( 7-bit-addr -- f )
  2* 1+  \ Build full byte with read-bit as 1
  i2c.start i2c.c! 0= if i2c.c@.nack drop true else false then
;
