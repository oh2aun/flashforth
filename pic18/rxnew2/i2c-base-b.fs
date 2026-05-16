\ *********************************************************************
\                                                                     *
\    Filename:      i2c-base-b.txt                                    *
\    Date:          22.10.2023                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ For PIC18F26K42 and other Kxx and Qxx chips with new I2C peripheral
fl+
-i2c-base
marker -i2c-base
hex ram

\ Check the I2C pins of your chip and if ANSEL must be used to
\ to set the I/O pins to digital mode.

fa60 constant anselc
fa62 constant odconc
ffbc constant latc
ffc4 constant trisc
fa15 constant rc5pps
fa14 constant rc4pps
fa63 constant slrconc
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

f9dd constant oscen
f9df constant oscfrq

\ i2c1con0 bits
3 constant mdr
5 constant start
6 constant rsen

\ i2c1con1 bits
6 constant ackdt
7 constant ackcnt

\ i2c1stat0 bits
5 constant mma

\ i2c1stat1 bits
0 constant rxbf
5 constant txbe

: b, 1 ;  \ Banked address mode

ram
: i2c.init ( -- )
  pps+
  $21 rc5pps c!           \ SCL
  $22 rc4pps c!           \ SDA
  %010.100 i2c1sdapps c!  \ RC4 SDA
  %010.101 i2c1sclpps c!  \ RC5 SCL
  pps-
  $30 anselc mclr    \ digital PC5 PC4
  $30 odconc mset    \ open drain PC5 PC4
  $30 latc mclr
  $30 trisc mclr     \ output PC5 PC4
  $30 slrconc mset
  $04 i2c1con0 c!
  $80 i2c1con1 c!
  $20 i2c1con2 c!    \ fast mode, adb 
\  $03 i2c1clk  c!    \ MFINTOSC 500 KHz, 125 KHz 12c clock
  $00 oscfrq c!      \ 1 MHz HFINTOSC 250 KHz i2c clock
  $40 oscen c!
  $02 i2c1clk  c!    \ HFINTOSC
  $80 i2c1con0 mset  \ Master mode, 7-bit address 
;

: i2c.start ( -- )  \ Set start condition
  [ i2c1con0 8 rshift  $3f and movlb, ]
  [ i2c1con0 start b, bsf, ] 
;
: i2c.rsen+ ( -- )  \ Request restart condition
  [ i2c1con0 8 rshift  $3f and movlb, ]
  [ i2c1con0 rsen b, bsf, ] 
;
: i2c.rsen- ( -- )  \ Remove restart condition
  [ i2c1con0 8 rshift  $3f and movlb, ]
  [ i2c1con0 rsen b, bcf, ] 
;

: i2c.stop ( -- ) \ wait for master state machine idle
  [ i2c1stat0 8 rshift  $3f and movlb, ]
  [ begin,
  [   i2c1stat0 mma b, btfsc, ] 
  [ again,                  ]
;
: i2c.txbe ( -- )  \  Wait for TX buffer empty
  [ i2c1stat1 8 rshift  $3f and movlb, ]
  [ begin,
  [   i2c1stat1 txbe b, btfss, ] 
  [ again,                     ]
;
: i2c.rxbf ( -- )  \ Wait for RX buffer full
  [ i2c1stat1 8 rshift $3f and movlb, ]
  [ begin,
  [   i2c1stat1 rxbf b, btfss, ]
  [ again,                     ]
;
: i2c.mdr ( -- )  \ Wait for RX buffer full
  [ i2c1con0 8 rshift $3f and movlb, ]
  [ begin,
  [   i2c1con0 mdr b, btfss, ]
  [ again,                     ]
;

: i2c.c! ( c -- ) 
  1 i2c1cnt c! 
  i2c1txb c!
  i2c.start
  i2c.stop
;
: i2c.2c! ( c c -- ) \ Write two chars to the I2C bus 
  2 i2c1cnt c! 
  i2c1txb c!
  i2c.start
  i2c.txbe
  i2c1txb c!
  i2c.stop
;
: i2c.c!! ( u c -- ) \ Write a char and a cell to the I2C bus
  3 i2c1cnt c! 
  i2c1txb c!
  i2c.start
  i2c.txbe
  dup 8 rshift i2c1txb c!
  i2c.txbe
  i2c1txb c! 
  i2c.stop
;
: i2c.c!c!! ( u c c -- ) \ Write two chars and a cell to the I2C bus
  4 i2c1cnt c! 
  i2c1txb c!
  i2c.start
  i2c.txbe
  i2c1txb c!
  i2c.txbe
  dup 8 rshift i2c1txb c!
  i2c.txbe
  i2c1txb c! 
  i2c.stop
;
: i2c.write ( i2c.addr -- ) 2* i2c1adb1 c! ;    \ Set I2C address for write
: i2c.read  ( i2c.addr -- ) 2* 1+ i2c1adb1 c! ; \ Set I2C address for read

: i2c.c@ ( i2c.addr -- c )
  i2c.read 1 i2c1cnt c! i2c.start i2c.rxbf i2c1rxb c@ i2c.stop ;
   
: i2c.nc@ ( n i2c.addr -- c1...cn ) \ read n bytes from the I2C bus
  i2c.read dup i2c1cnt c!
  i2c.start
  for
    i2c.rxbf
    i2c1rxb c@
  next
  i2c.stop
;
: i2c.rc@ ( register i2c.addr -- c )
  dup >r i2c.write 1 i2c1cnt c!
  i2c.start i2c.rsen+
  i2c.mdr
  i2c1txb c!
  i2c.mdr
  r> i2c.read 1 i2c1cnt c!
  i2c.start i2c.rsen-
  i2c.rxbf i2c1rxb c@
  i2c.stop
;

\ Detect presence of device, leaving true if device present, 0 otherwise.
\ We actually fetch a byte if the slave has acknowledged, then discard it. 
: i2c.ping ( hw -- f )
    0 i2c1pir c! 
    4 i2c1stat1 c! 
    i2c.read               \ put hw in address register  
    1 i2c1cnt c!           \ set count of bytes to get to one  
    i2c.start              \ start 
    begin 4 i2c1pir mtst until  \ wait until stop issued to bus 
 
    \ if device at hw there will be something in I2CRXB which will 
    \ need to be cleared or it will hang via clock stretch when 
    \ the machine next comes accross a byte that needs to be read  
    \ into the buffer p694 Easiest to set clrbf flag and force 
    \ the clearing of rx tx buffers. Put this in i2c.clear at top  
     
    i2c1cnt c@ 0=         \ count=0 gives true = response from hw  
;

\ i2c.init must be called before any other i2c words.
 
\ Read one byte from i2c bus address $60
\ $60 i2c.read i2c.c@.nack

\ Read 2 bytes from i2c bus address $60
\ $60 i2c.read i2c.c@.ack i2c.c@.nack

\ Write 8 bits to i2c bus address $60
\ $12 $60 i2c.write i2c.c!

\ Write 16 bits to i2c bus address $60
\ $12 $60 i2c.write dup 8 rshift i2c.c! i2c.c!

\ Read a value from register 3 at address $60
\ 3 $60 i2c.rc@

