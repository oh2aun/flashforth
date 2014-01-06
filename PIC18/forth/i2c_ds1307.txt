\ *********************************************************************
\                                                                     *
\    Filename:      i2c_ds1307.txt                                    *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

\ ********************************
\ DS1307 RTC i2c words

-ds1307
marker -ds1307

\ i2c device address
$d0 constant addr-ds1307

: time! ( c c c c c c c c -- )
  i2cinit 0 addr-ds1307 i2c-addr1
  i2c! i2c! i2c! i2c! i2c! i2c! i2c! i2c!
  spen
;

: time@ ( - c c c c c c c c )
  i2cinit 
  0 addr-ds1307 i2c-addr1 spen
  addr-ds1307 1+ i2cws
  i2c@ak i2c@ak i2c@ak i2c@ak
  i2c@ak i2c@ak i2c@ak i2c@nak
;

: bin>bcd ( c -- c )
   #10 u/mod #4 lshift or
;
: set-time ( year month date day hour min sec -- )
  >r >r >r >r >r >r
  $11 swap     \ 91 = 4096 KHz output 11 = no output
  bin>bcd      \ Year 0-99
  r> bin>bcd   \ Month
  r> bin>bcd   \ Date
  r>           \ Day 1-7
  r> bin>bcd   \ Hours
  r> bin>bcd   \ Minutes
  r> bin>bcd   \ Seconds
  time!
;
