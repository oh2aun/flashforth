\ i2c-base-pic24fv16km202.txt
\ Low-level words for I2C master on PIC24
\
\ Modelled on i2c-base.txt for PIC18, i2c-twi.frt from amforth
\ the Microchip PIC24 Family Reference Manual 
\ and the datasheet for PIC24FV16KM202.
\ Peter J.  2014-11-06

-i2c-base
marker -i2c-base
hex ram

\ Registers related to I2C operation of MSSP1
$0086 constant ifs1
$0200 constant ssp1buf
$0202 constant ssp1con1
$0204 constant ssp1con2
$0206 constant ssp1con3
$0208 constant ssp1stat
$020a constant ssp1add
$020c constant ssp1msk
$02c8 constant trisb
$02ca constant portb
$02cc constant latb
$02ce constant odcb
$04e2 constant ansb
$0770 constant pmd1

\ Masks for bits
%00100000 constant mSSP1EN \ in ssp1con1
%00000001 constant mSEN \ in ssp1con2
%00000010 constant mRSEN
%00000100 constant mPEN
%00001000 constant mRCEN
%00010000 constant mACKEN
%00100000 constant mACKDT
%01000000 constant mACKSTAT
%00000001 constant mBF \ in ssp1stat
%00000001 constant mSSP1IF \ in ifs1

: i2c.init ( -- )
  $80 pmd1 mclr \ Enable the SSP1 module
  %00001000 ssp1con1 c! \ Master mode
  [ Fcy #100 / 1- ] literal ssp1add c! \ Set clock frequency to 100 kHz
  mSSP1IF ifs1 mclr \ Clear interrupt bit
  %1100000000 trisb mset \ SCL1 on RB8, SDA1 on RB9
  %1100000000 odcb mset
  %1100000000 ansb mclr
  mSSP1EN ssp1con1 mset \ Enable hardware
;

: i2c.close ( -- )
  mSSP1EN ssp1con1 mclr
  mSSP1IF ifs1 mclr
;

: i2c.wait ( -- ) \ Wait for interrupt flag and clear it
  begin mSSP1IF ifs1 mtst until
  mSSP1IF ifs1 mclr
;

: i2c.idle? ( -- f )
  %00011111 ssp1con2 mtst \ ACKEN RCEN REN RSEN SEN
  %100 ssp1stat mtst \ R/^W 
  or 0=
;

: i2c.start ( -- ) \ Send start condition
  begin i2c.idle? until
  mSSP1IF ifs1 mclr
  mSEN ssp1con2 mset
  i2c.wait
;

: i2c.rsen ( -- ) \ Send repeated start condition
  mSSP1IF ifs1 mclr
  mRSEN ssp1con2 mset
  i2c.wait
;

: i2c.stop ( -- ) \ Send stop condition
  mSSP1IF ifs1 mclr
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
  1 lshift 1 invert and \ Build full byte with write-bit as 0
  i2c.start i2c.c! 0=
;

\ Address slave for reading, leaves true if slave ready.
: i2c.addr.read ( 7-bit-addr -- f )
  1 lshift 1 or \ Build full byte with read-bit as 1
  i2c.start i2c.c! 0=
;

\ Detect presence of device, 
\ leaving true if device present, 0 otherwise.
\ The 16KM202 must actually fetch a byte if the slave has acknowledged. 
: i2c.ping? ( 7-bit-addr -- f )
  i2c.addr.read if i2c.c@.nack drop true else false then
;
