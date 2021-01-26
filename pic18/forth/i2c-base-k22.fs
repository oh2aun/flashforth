\ i2c-base-k22.txt
\ Low-level words for I2C master on PIC18F26K22
\
\ Modelled on the original i2c-base.txt for PIC18, 
\ i2c-twi.frt from amforth and
\ the datasheet for Microchip PIC18F26K22.
\ Peter J.  2014-11-08

-i2c-base-k22
marker -i2c-base-k22
hex ram

\ Registers related to I2C operation of MSSP1
$ff3a constant anselc
$ff82 constant portc
$ff8b constant latc
$ff94 constant trisc
$ff9e constant pir1
$ffc5 constant ssp1con2
$ffc6 constant ssp1con1
$ffc7 constant ssp1stat
$ffc8 constant ssp1add
$ffc9 constant ssp1buf
$ffca constant ssp1msk
$ffcb constant ssp1con3

\ Masks for bits
%00000001 constant mSEN \ in ssp1con2
%00000010 constant mRSEN
%00000100 constant mPEN
%00001000 constant mRCEN
%00010000 constant mACKEN
%00100000 constant mACKDT
%01000000 constant mACKSTAT
%00100000 constant mSSP1EN \ in ssp1con1
%00000001 constant mBF \ in ssp1stat
%00001000 constant mSSP1IF \ in pir1

: i2c.init ( -- )
  %00001000 ssp1con1 c! \ Master mode
  [ Fcy #100 / 1- ] literal ssp1add c! \ Set clock frequency to 100 kHz
  mSSP1IF pir1 mclr \ Clear interrupt bit
  %00011000 trisc mset \ SCL1 on RC3, SDA1 on RC4
  %00011000 anselc mclr
  mSSP1EN ssp1con1 mset \ Enable hardware
;

: i2c.close ( -- )
  mSSP1EN ssp1con1 mclr
  mSSP1IF pir1 mclr
;

: i2c.wait ( -- ) \ Wait for interrupt flag and clear it
  begin mSSP1IF pir1 mtst until
  mSSP1IF pir1 mclr
;

: i2c.idle? ( -- f )
  %00011111 ssp1con2 mtst \ ACKEN RCEN REN RSEN SEN
  %100 ssp1stat mtst \ R/^W 
  or 0=
;

: i2c.start ( -- ) \ Send start condition
  begin i2c.idle? until
  mSSP1IF pir1 mclr
  mSEN ssp1con2 mset
  i2c.wait
;

: i2c.rsen ( -- ) \ Send repeated start condition
  mSSP1IF pir1 mclr
  mRSEN ssp1con2 mset
  i2c.wait
;

: i2c.stop ( -- ) \ Send stop condition
  mSSP1IF pir1 mclr
  mPEN ssp1con2 mset
  i2c.wait
;

: i2c.buf.full? ( -- f ) 
  mBF ssp1stat mtst
;

\ Write one byte to bus, leaves ACK bit.
\ A value of 0 indicates ACK was received from slave device.
: i2c.c! ( c -- f )
  begin i2c.buf.full? 0= until
  ssp1buf c!
  begin i2c.buf.full? 0= until
  begin i2c.idle? until
  ssp1con2 c@ mACKSTAT and
;

\ Send ack bit.
: i2c.ack.seq ( -- )
  mACKEN ssp1con2 mset
  begin mACKEN ssp1con2 mtst 0= until
;

\ Read one byte and ack for another.
: i2c.c@.ack ( -- c )
  mRCEN ssp1con2 mset
  begin i2c.buf.full? until
  mACKDT ssp1con2 mclr i2c.ack.seq \ ack
  ssp1buf c@
;

 \ Read one last byte.
: i2c.c@.nack ( -- c ) 
  mRCEN ssp1con2 mset
  begin i2c.buf.full? until
  mACKDT ssp1con2 mset i2c.ack.seq \ nack
  ssp1buf c@
;

\ Address slave for writing, leaves true if slave ready.
: i2c.addr.write ( 7-bit-addr -- f )
  2*           \ Build full byte with write-bit as 0
  i2c.start i2c.c! 0=
;

\ Address slave for reading, leaves true if slave ready.
: i2c.addr.read ( 7-bit-addr -- f )
  2* 1+ \ Build full byte with read-bit as 1
  i2c.start i2c.c! 0=
;

\ Detect presence of device, leaving true if device present, 0 otherwise.
\ We actually fetch a byte if the slave has acknowledged, then discard it. 
: i2c.ping? ( 7-bit-addr -- f )
  i2c.addr.read if i2c.c@.nack drop true else false then
;
