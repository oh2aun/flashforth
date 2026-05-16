\ *********************************************************************
\                                                                     *
\    Filename:      i2c2-base-b.fs                                     *
\    Date:          22.02.2026                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ For PIC18F26K42 and other Kxx and Qxx chips with new I2C peripheral
\
fl+
-i2c2-base
marker -i2c2-base
hex ram

\ Check the I2C pins of your chip and if ANSEL must be used to
\ to set the I/O pins to digital mode.

fa60 constant anselc
fa62 constant odconc
ffbc constant latc
ffc4 constant trisc
fa10 constant rc0pps
fa11 constant rc1pps
fa63 constant slrconc
fae4 constant i2c2sdapps
fae3 constant i2c2sclpps

fd61 constant i2c2stat0
fd62 constant i2c2stat1
fd5d constant i2c2con0
fd5e constant i2c2con1
fd5f constant i2c2con2
fd65 constant i2c2clk
fd55 constant i2c2txb
fd54 constant i2c2rxb
fd56 constant i2c2cnt
fd58 constant i2c2adb1

fd63 constant i2c2pir

f9dd constant oscen
f9df constant oscfrq

\ i2c2con0 bits
3 constant mdr
5 constant start
6 constant rsen

\ i2c2con1 bits
6 constant ackdt
7 constant ackcnt

\ i2c2stat0 bits
5 constant mma

\ i2c2stat1 bits
0 constant rxbf
5 constant txbe

: b, 1 ;  \ Banked address mode

ram
: i2c2.init ( -- )
  pps+
  $23 rc0pps c!           \ SCL2
  $24 rc1pps c!           \ SDA2
  %010.001 i2c2sdapps c!  \ RC1 SDA2
  %010.000 i2c2sclpps c!  \ RC0 SCL2
  pps-
  $03 anselc mclr    \ digital PC0 PC1
  $03 odconc mset    \ open drain PC0 PC1
  $03 latc mclr
  $03 trisc mclr     \ output PC0 PC1
  $03 slrconc mset
  $04 i2c2con0 c!
  $80 i2c2con1 c!
  $20 i2c2con2 c!    \ fast mode, adb 
  $03 i2c2clk  c!    \ MFINTOSC 500 KHz, 125 KHz 12c clock
\  $00 oscfrq c!      \ 1 MHz HFINTOSC 250 KHz i2c clock
\  $40 oscen c!
\  $00 i2c2clk  c!    \ HFINTOSC
  $80 i2c2con0 mset  \ Master mode, 7-bit address 
;

: i2c2.start ( -- )  \ Set start condition
  [ i2c2con0 8 rshift  $3f and movlb, ]
  [ i2c2con0 start b, bsf, ] 
;
: i2c2.rsen+ ( -- )  \ Request restart condition
  [ i2c2con0 8 rshift  $3f and movlb, ]
  [ i2c2con0 rsen b, bsf, ] 
;
: i2c2.rsen- ( -- )  \ Remove restart condition
  [ i2c2con0 8 rshift $3f and movlb, ]
  [ i2c2con0 rsen b, bcf, ] 
;

: i2c2.stop ( -- ) \ wait for master state machine idle
  [ i2c2stat0 8 rshift $3f and movlb, ]
  [ begin,                    ]
  [   i2c2stat0 mma b, btfsc, ] 
  [ again,                    ]
;
: i2c2.txbe ( -- )  \  Wait for TX buffer empty
  [ i2c2stat1 8 rshift $3f and movlb, ]
  [ begin,                     ]
  [   i2c2stat1 txbe b, btfss, ] 
  [ again,                     ]
;
: i2c2.rxbf ( -- )  \ Wait for RX buffer full
  [ i2c2stat1 8 rshift $3f and movlb, ]
  [ begin,                     ]
  [   i2c2stat1 rxbf b, btfss, ]
  [ again,                     ]
;
: i2c2.mdr ( -- )  \ Wait for RX buffer full
  [ i2c2con0 8 rshift $3f and movlb, ]
  [ begin,                   ]
  [   i2c2con0 mdr b, btfss, ]
  [ again,                   ]
;

: i2c2.c! ( c -- ) 
  1 i2c2cnt c! 
  i2c2txb c!
  i2c2.start
  i2c2.stop
;
: i2c2.2c! ( c c -- ) \ Write two chars to the I2C bus 
  2 i2c2cnt c! 
  i2c2txb c!
  i2c2.start
  i2c2.txbe
  i2c2txb c!
  i2c2.stop
;
: i2c2.c!! ( u c -- ) \ Write a char and a cell to the I2C bus
  3 i2c2cnt c! 
  i2c2txb c!
  i2c2.start
  i2c2.txbe
  dup 8 rshift i2c2txb c!
  i2c2.txbe
  i2c2txb c! 
  i2c2.stop
;
: i2c2.c!c!! ( u c c -- ) \ Write two chars and a cell to the I2C bus
  4 i2c2cnt c! 
  i2c2txb c!
  i2c2.start
  i2c2.txbe
  i2c2txb c!
  i2c2.txbe
  dup 8 rshift i2c2txb c!
  i2c2.txbe
  i2c2txb c! 
  i2c2.stop
;
: i2c2.write ( i2c.addr -- ) 2* i2c2adb1 c! ;    \ Set I2C address for write
: i2c2.read  ( i2c.addr -- ) 2* 1+ i2c2adb1 c! ; \ Set I2C address for read

: i2c2.c@ ( i2c.addr -- c )
  i2c2.read 1 i2c2cnt c! i2c2.start i2c2.rxbf i2c2rxb c@ i2c2.stop ;
   
: i2c2.nc@ ( n i2c.addr -- c1...cn ) \ read n bytes from the I2C bus
  i2c2.read dup i2c2cnt c!
  i2c2.start
  for
    i2c2.rxbf
    i2c2rxb c@
  next
  i2c2.stop
;
: i2c2.rc@ ( register i2c.addr -- c )
  dup >r i2c2.write 1 i2c2cnt c!
  i2c2.start i2c2.rsen+
  i2c2.mdr
  i2c2txb c!
  i2c2.mdr
  r> i2c2.read 1 i2c2cnt c!
  i2c2.start i2c2.rsen-
  i2c2.rxbf i2c2rxb c@
  i2c2.stop
;

\ Detect presence of device, leaving true if device present, 0 otherwise.
\ We actually fetch a byte if the slave has acknowledged, then discard it. 
: i2c2.ping ( hw -- f )
    0 i2c2pir c! 
    4 i2c2stat1 c! 
    i2c2.read               \ put hw in address register  
    1 i2c2cnt c!           \ set count of bytes to get to one  
    i2c2.start              \ start 
    begin 4 i2c2pir mtst until  \ wait until stop issued to bus 
 
    \ if device at hw there will be something in I2CRXB which will 
    \ need to be cleared or it will hang via clock stretch when 
    \ the machine next comes accross a byte that needs to be read  
    \ into the buffer p694 Easiest to set clrbf flag and force 
    \ the clearing of rx tx buffers. Put this in i2c.clear at top  
     
    i2c2cnt c@ 0=         \ count=0 gives true = response from hw  
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

