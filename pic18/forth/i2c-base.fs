\ *********************************************************************
\                                                                     *
\    Filename:      i2c-base.txt                                      *
\    Date:          05.10.2022                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-i2c-base
marker -i2c-base
hex ram

\ Check the I2C pins of your chip and if ANSEL must be used to
\ to set the I/O pins to digital mode.
$03  constant i2c.pins     \ pin bitmask for i2c.ansel
ff5c constant i2c.ansel

ffc5 constant sspcon2
ffc6 constant sspcon1
ffc7 constant sspstat
ffc8 constant sspadd
ffc9 constant sspbuf
ff9e constant pir1

\ pir1 bits
3 constant sspif

\ SSPCON2 bits
0 constant sen
1 constant rsen
2 constant pen
3 constant rcen
4 constant acken
5 constant ackdt
6 constant ackstat

eeprom #100 value i2c.speed \ 100 KHz i2c
ram
: i2c.init ( -- )
  i2c.pins i2c.ansel mclr
  %10000000 sspstat c!
  Fcy i2c.speed / sspadd c! 
  %00101000 sspcon1 c!  \ HW controlled mastermode i2c
  %00000000 sspcon2 c!
;
: i2c.wait ( -- ) \ Wait for operation to complete
  \ [  pir1 sspif a, bcf,     ] \ clear interrupt
  [  begin,                 ]
  [    pir1 sspif a, btfss, ] \ Interrupt
  [  again,                 ]
  [  pir1 sspif a, bcf,     ] \ clear interrupt
;

: i2c.start  ( -- ) \ Send start condition enable /stretch bit
  [  sspcon2 sen a, bsf,    ]
  i2c.wait
;

: i2c.rstart ( -- ) \ Send repeated start condition enable bit
  [ sspcon2 rsen a, bsf,    ]
  i2c.wait
;
: i2c.stop    ( -- ) \ Send stop bit 
  [ sspcon2 pen a, bsf,     ]  \ pen send stop bit
  i2c.wait
;

: i2c.c! ( c --  write one byte to the i2c bus )
  sspbuf c! i2c.wait ;

: i2c.c@.ack ( -- c ) \ Read one byte from the i2c bus
  [ sspcon2 rcen a, bsf,    ]  \ rcen  enable receive mode
  i2c.wait
  sspbuf c@ 
  [ sspcon2 ackdt a, bcf,   ]  \ ackdt ACK
  [ sspcon2 acken a, bsf,   ]  \ acken send ACKDT bit
  i2c.wait
;

: i2c.c@.nack ( -- c ) \ Read one byte from the i2c bus
  [ sspcon2 rcen a, bsf,    ]  \ rcen  enable receive mode
  i2c.wait
  sspbuf c@
  [ sspcon2 ackdt a, bsf,   ]  \ ackdt no ACK
  [ sspcon2 acken a, bsf,   ]  \ acken send ACKDT bit
  i2c.wait
;

: i2c.ack? ( -- flag )
  [ 1 ackstat lshift ] literal sspcon2 mtst 0= \ check for Ack from slave
;

: i2c.write ( i2c.addr -- ) i2c.start 2*    i2c.c! ;
: i2c.read  ( i2c.addr -- ) i2c.start 2* 1+ i2c.c! ;


\ Detect presence of device, leaving true if device present, 0 otherwise.
\ We actually fetch a byte if the slave has acknowledged, then discard it. 
: i2c.ping ( 7-bit-addr -- f )
  i2c.read i2c.ack? if   i2c.c@.nack drop true 
                    else false 
                    then i2c.stop ;

\ i2c.init must be called before any other i2c words.
 
\ Read one byte from i2c bus address $60
\ $60 i2c.read i2c.c@.nack i2c.stop

\ Read 2 bytes from i2c bus address $60
\ $60 i2c.read  i2c.c@.ack i2c.c@.nack i2c.stop  

\ Write 8 bits to i2c bus address $60
\ $12 $60 i2c.write i2c.c! i2c.stop

\ Write 16 bits to i2c bus address $60
\ $12 $60 i2c.write dup 8 rshift i2c.c! i2c.c! i2c.stop 

\ Read a value from register 3 at address $60
\ $60 i2c.write 3 i2c.c! i2c.repstart $60 i2c.read i2c.c@.nack i2c.stop

