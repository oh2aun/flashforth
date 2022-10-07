\ *********************************************************************
\                                                                     *
\    Filename:      i2c-ds1307.txt                                    *
\    Date:          07.10.2022                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega PIC18 PIC24 PIC30 PIC33                    *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

\ ********************************
\ DS1307 RTC i2c words
\ ********************************
-ds1307
marker -ds1307

\ i2c device address
$68 constant addr-ds1307

: ds1307.addr! ( c -- )  \ Set ds1307 register address
  i2c.init addr-ds1307  i2c.write
  i2c.c! i2c.stop ;

: time! ( c c c c c c c  -- )
  i2c.init addr-ds1307  i2c.write
  0 i2c.c! i2c.c! i2c.c! i2c.c! i2c.c! i2c.c! i2c.c! i2c.c!
  i2c.stop
;

: time@ ( -- c c c c c c c )
  0 ds1307.addr!
  addr-ds1307 i2c.read
  i2c.c@.ack i2c.c@.ack i2c.c@.ack
  i2c.c@.ack i2c.c@.ack i2c.c@.ack i2c.c@.nack
  i2c.stop
;

: bin>bcd ( c -- c )
   #10 u/mod #4 lshift or
;
: set-time ( year month date day hour min sec -- )
  >r >r >r >r >r >r
  $00 swap     \ 11 = 4.096 KHz output 00 = no output
  bin>bcd      \ Year 0-99
  r> bin>bcd   \ Month
  r> bin>bcd   \ Date
  r>           \ Day 1-7
  r> bin>bcd   \ Hours
  r> bin>bcd   \ Minutes
  r> bin>bcd   \ Seconds
  time!
;

: i2c.ds1307.c@ ( addr -- c )
  ds1307.addr! addr-ds1307 i2c.read i2c.c@.nack i2c.stop
;
: i2c.ds1307.c! ( c addr -- )
  i2c.init addr-ds1307 i2c.write i2c.c! i2c.c! i2c.stop
;

 
: i2c.ds1307.n@ ( n addr -- )
  ds1307.addr!
  addr-ds1307  i2c.read
  for i2c.c@.ack next 
  i2c.c@.nack i2c.stop
;

