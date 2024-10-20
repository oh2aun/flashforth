\ *********************************************************************
\                                                                     *
\    Filename:      i2c-base-b.txt                                    *
\    Date:          21.02.2023                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ For PIC18F26K42 and other Kxx and Qxx chips with new I2C peripheral

-i2c-base
marker -i2c-base
hex ram

\ Check the I2C pins of your chip and if ANSEL must be used to
\ to set the I/O pins to digital mode.

fa60 constant anselc
fa62 constant odconc
ffbc constant latc
ffc4 constant trisc
fa13 constant rc3pps
fa14 constant rc4pps
fae2 constant i2c1sdapps
fae1 constant i2c1sclpps

fd77 constant i2c1stat0
fd78 constant i2c1stat1
fd73 constant i2c1con0
fd74 constant i2c1con1
fd75 constant i2c1con2
fd7b constant i2c1clk
fd6b constant i2c1txb
fd6a constant i2c1rxb
fd6c constant i2c1cnt
fd6e constant i2c1adb1

fd79 constant i2c1pir

\ i2c1con1 bits
6 constant ackdt
7 constant ackcnt

\ i2c1stat bits
0 constant rxbf
5 constant txbe

: b, 1 ;  \ Banked address mode

ram
: i2c.init ( -- )
  pps+
  $21 rc3pps c!           \ SCL
  $22 rc4pps c!           \ SDA
  %010.100 i2c1sdapps c!  \ RC4 SDA
  %010.011 i2c1sclpps c!  \ RC3 SCL
  pps-
  $18 anselc mclr     \ digital PC3 PC4
  $18 odconc mset    \ open drain PC3 PC4
  $18 latc mclr
  $18 trisc mclr     \ output PC3 PC4
  $04 i2c1con0 c!
  $80 i2c1con1 c!
  $00 i2c1con2 c!    \ fast mode, adb 
  $03 i2c1clk  c!    \ MFINTOSC 500 KHz
  $80 i2c1con0 mset  \ Master mode, 7-bit address 
;

: i2c.start ( -- )  \ Set start condition
  [ i2c1con0 8 rshift  $3f and movlb, ]
  [ i2c1con0 5 b, bsf, ] 
;

: i2c.stop ( -- )
  [ i2c1stat0 8 rshift  $3f and movlb, ]
  [ begin,
  [   i2c1stat0 5 b, btfsc, ] 
  [ again,                 ]
;

: i2c.txbe ( -- )  \  Wait for TX buffer empty
  [ i2c1stat1 8 rshift  $3f and movlb, ]
  [ begin,
  [   i2c1stat1 txbe b, btfss, ] 
  [ again,                 ]
;
: i2c.rxbf ( -- )  \ Wait for RX buffer full
  [ i2c1stat1 8 rshift $3f and movlb, ]
  [ begin,
  [   i2c1stat1 rxbf b, btfss, ]
  [ again,                 ]
;

: i2c.c! ( c -- ) 
  1 i2c1cnt c! 
  i2c1txb c!
  i2c.start
  i2c.txbe i2c.stop 
;
: i2c.2c! ( c c -- ) \ Write two chars to the I2C bus 
  2 i2c1cnt c! 
  i2c1txb c!
  i2c.start
  i2c.txbe
  i2c1txb c!
  i2c.txbe i2c.stop
;
: i2c.c!! ( u c -- ) \ Write a char and a cell to the I2C bus
  3 i2c1cnt c! 
  i2c1txb c!
  i2c.start
  i2c.txbe
  dup 8 rshift i2c1txb c!
  i2c.txbe
  i2c1txb c! 
  i2c.txbe i2c.stop
;
: i2c.c!c!! ( u c c -- ) \ Write two chars and a cell to the I2C bus
  4 i2c1cnt c! 
  i2c1txb c!
  i2c.start
  i2c.txbe
  i2c1txb c!
  i2c.start
  i2c.txbe
  dup 8 rshift i2c1txb c!
  i2c.txbe
  i2c1txb c! 
  i2c.txbe i2c.stop
;
: i2c.write ( i2c.addr -- ) 2* i2c1adb1 c! ;    \ Set I2C address for writing
: i2c.read  ( i2c.addr -- ) 2* 1+ i2c1adb1 c! ; \ Set I2C address for reading

: i2c.c@.ack ( --  c ) \ Read a char from the I2C bus and ack. 
  1 i2c1cnt c!
  i2c.start
  [ i2c1con1 ackdt b, bcf,  ]
  [ i2c1con1 ackcnt b, bcf, ]
  i2c.rxbf
  i2c1rxb c@
  i2c.stop
;

: i2c.c@.nack ( -- c ) \ Read a char from the I2C bus and nack
  1 i2c1cnt c!
  i2c.start
  [ i2c1con1 ackdt b, bcf,  ]
  [ i2c1con1 ackcnt b, bsf, ]
  i2c.rxbf
  i2c1rxb c@
  i2c.stop
;

\ The I2C hardware makes a i2c.ping impossible to implement
\ Detect presence of device, leaving true if device present, 0 otherwise.
\ We actually fetch a byte if the slave has acknowledged, then discard it. 
\ : i2c.ping ( 7-bit-addr -- f )
\  i2c.read i2c.ack? if   i2c.c@.nack drop true 
\                    else false 
\                    then i2c.stop ;

\ i2c.init must be called before any other i2c words.
 
\ Read one byte from i2c bus address $60
\ $60 i2c.read i2c.c@.nack

\ Read 2 bytes from i2c bus address $60
\ $60 i2c.read  i2c.c@.ack i2c.c@.nack

\ Write 8 bits to i2c bus address $60
\ $12 $60 i2c.write i2c.c!

\ Write 16 bits to i2c bus address $60
\ $12 $60 i2c.write dup 8 rshift i2c.c! i2c.c!

\ Read a value from register 3 at address $60
\ $60 i2c.write 3 i2c.c! i2c.repstart $60 i2c.read i2c.c@.nack

