\ *********************************************************************
\                                                                     *
\    Filename:      i2c_base.fs                                       *
\    Date:          05.10.2022                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC24 PIC30 PIC33                                 *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-i2c-base
marker -i2c-base
hex ram

\ Check the register addresses from the datasheet of your chip
$02e0 constant i2c1con
$02e4 constant i2c1stat
$02de constant i2c1brg
$02dc constant i2c1trn
$02da constant i2c1rcv
$0086 constant ifs1

\ ifs1 bits
1 constant mi2c1if

\ i2c1conl bits
0 constant sen
1 constant rsen
2 constant pen
3 constant rcen
4 constant acken
5 constant ackdt

\ i2c1stat bits
#15 constant ackstat

eeprom #100 value i2c.speed \ 100 KHz i2c
flash  #100 value i2c.speed \ 100 KHz i2c
ram
: i2c.init ( -- )
  Fcy 2/ i2c.speed / i2c1brg !
  $8000 i2c1con !
;
: i2c.wait ( -- ) \ Wait for operation to complete
  [  begin,                 ]
  [    mi2c1if ifs1 btss,   ] \ Interrupt
  [  again,                 ]
  [  mi2c1if ifs1 bclr,     ] \ clear interrupt
;

: i2c.start  ( -- ) \ Send start condition enable /stretch bit
  [ sen i2c1con bset,     ]
  i2c.wait
;

: i2c.rstart ( -- ) \ Send repeated start condition enable bit
  [ rsen i2c1con bset,    ]
  i2c.wait
;
: i2c.stop    ( -- ) \ Send stop bit 
  [ pen i2c1con bset,     ]  \ pen send stop bit
  i2c.wait
;

: i2c.c! ( c --  write one byte to the i2c bus )
  i2c1trn ! i2c.wait ;

: i2c.c@.ack ( -- c ) \ Read one byte from the i2c bus
  [ rcen i2c1con bset,    ]  \ rcen  enable receive mode
  i2c.wait
  i2c1rcv c@ 
  [ ackdt i2c1con bclr,   ]  \ ackdt ACK
  [ acken i2c1con bset,   ]  \ acken send ACKDT bit
  i2c.wait
;

: i2c.c@.nack ( -- c ) \ Read one byte from the i2c bus
  [ rcen i2c1con bset,    ]  \ rcen  enable receive mode
  i2c.wait
  i2c1rcv c@
  [ ackdt i2c1con bset,   ]  \ ackdt no ACK
  [ acken i2c1con bset,   ]  \ acken send ACKDT bit
  i2c.wait
;

: i2c.ack? ( -- flag )
  [ 1 ackstat lshift ] literal i2c1stat mtst 0= \ check for Ack from slave
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

