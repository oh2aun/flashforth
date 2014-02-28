\ *********************************************************************
\                                                                     *
\    Filename:      i2c_base.txt                                      *
\    Date:          28.02.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-i2c
marker -i2c
hex ram


\ NOTE: 18F2455 and friends USB pics do NOT use PORTC for SDA,SCL.
\ Most others do. 
ff82 constant portc
ff94 constant trisc

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

: i2cinit ( -- )
  [ trisc %011 0 bsf, ]
  [ trisc %100 0 bsf, ]
  %10000000 sspstat c!
  [ Fcy #100 / ] literal sspadd c! \ 100 KHz i2c
  %00101000 sspcon1 c!  \ HW controlled mastermode i2c
  %00000000 sspcon2 c!
;

: ssen  ( -- ) \ Send start condition enable /stretch bit
  [  pir1 sspif a, bcf,     ]
  [  sspcon2 sen a, bsf,    ]
  [  begin,                 ]
  [    pir1 sspif a, btfss, ]
  [  again,                 ]
;

: srsen ( -- ) \ Send repeated start condition enable bit
  [ pir1 sspif a, bcf,      ]
  [ sspcon2 rsen a, bsf,    ]
  [ begin,                  ]
  [   pir1 sspif a, btfss,  ]
  [ again,                  ]
;
: spen  ( -- ) \ Send stop bit 
  [ pir1 sspif a, bcf,      ]
  [ sspcon2 pen a, bsf,     ]  \ pen send stop bit
  [ begin,                  ]
  [   pir1 sspif a, btfss,  ]
  [ again,                  ]
;
: srcen ( -- ) \ Send receive enable
  [ pir1 sspif a, bcf,      ]  \ sspif clear interrupt flag
  [ sspcon2 rcen a, bsf,    ]  \ rcen  enable receive mode
  [ begin,                  ]
  [   pir1 sspif a, btfss,  ]  \ sspif Wait for reception to complete
  [ again,                  ]
;
: snoack ( -- ) \ send no ack
  [ pir1 sspif a, bcf,      ]  \ sspif clear interrupt flag
  [ sspcon2 ackdt a, bsf,   ]  \ ackdt no ACK
  [ sspcon2 acken a, bsf,   ]  \ acken send ACKDT bit
  [ begin,                  ]
  [   pir1 sspif a, btfss,  ]  \ sspif Wait
  [ again,                  ]
;
: sack ( -- ) \ Send ack
  [ pir1 sspif a, bcf,      ]  \ sspif clear interrupt flag
  [ sspcon2 ackdt a, bcf,   ]  \ ackdt ACK
  [ sspcon2 acken a, bsf,   ]  \ acken send ACKDT bit
  [ begin,                  ]
  [   pir1 sspif a, btfss,  ]  \ sspif Wait
  [ again,                  ]
;
: sspbuf! ( c -- )          \ sspbuf! takes 90 us @ 100 KHz
    sspbuf c!
  [ pir1 sspif a, bcf,      ]
  [ begin,                  ]
  [   pir1 sspif a, btfss,  ]
  [ again,                  ]
;

\ i2c wakeslave
\ NOTE: the 7 bit address is in bits 7-1. 
\ Bit 0 is the R/W bit.

: i2cws ( slaveaddr -- )
  ssen
  begin
    dup sspbuf!
    [ sspcon2 w, a,    movf,  ]
    [ 1 ackstat lshift andlw, ] \ check for Acknowledge from slave
    [ z, if,                  ]
       drop exit
    [ then,                   ]
    srsen                       \ Repeated start condition
  again
;

: i2c! ( c --  write one byte to the i2c bus )
  sspbuf!
  begin
    [ sspcon2 ackstat a, btfsc, ] \ ackstat Wait for ACK from slave
  again
;
: i2c@nak ( -- c ) \ read one last byte from the i2c bus
  srcen              \ Receive enable
  sspbuf c@          \ save data to stack
  snoack             \ NO ACK
  spen               \ Stop Bit Enable
;
: i2c@ak ( -- c c ) \ read one byte and continue
  srcen                \ Receive enable
  sspbuf c@            \ save data to stack
  sack                 \ Send  ACK
;

\ Write 8-bit addr to i2c-addr
: i2c-addr1 ( addr i2c-addr -- )
  i2cws        \ wake slave
  i2c!         \ addr lo byte
;

\ Write 16-bit addr to i2c-addr
: i2c-addr2 ( addr i2c-addr -- )
  i2cws                \ wake slave
  dup #8 rshift i2c!    \ addr hi byte
  $ff and i2c!        \ addr lo byte
;
