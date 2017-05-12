\ i2c-detect.txt
\ Detect presence of all possible devices on I2C bus.
\ Only the 7 bit address schema is supported.
\
\ Copied from amForth distribution (lib/hardware/)
\ and lightly edited to suit FlashForth 5.0 on AVR.
\ Builds upon i2c-base.
\ Peter J. 2014-10-27
\ Mikael N. 2017-5-12 for..next instead of do..loop
-i2c-detect
marker -i2c-detect

\ not all bitpatterns are valid 7bit i2c addresses
: i2c.7bitaddr? ( a -- f)  $7 $78 within ;

: i2c.detect   ( -- )
    i2c.init
    base @ hex
    \ header line
    cr 5 spaces 0 $10 for dup 2 u.r  1+ next drop
    0 $80 for
      dup $0f and 0= if
        cr dup 2 u.r [char] : emit space
      then
      dup i2c.7bitaddr? if
        dup i2c.ping? if \ does device respond?
            dup 2 u.r
          else
            ." -- " 
        then
      else
         ."    "
      then
      1+
    next drop 
    i2c.stop
    cr base !
;

\ With a lone Microchip TC74A0 sitting on the bus,
\ the output looks like
\ ok<$,ram>
\ i2c.detect 
\      00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 
\ 00 :                      -- -- -- -- -- -- -- -- -- 
\ 10 : -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
\ 20 : -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
\ 30 : -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
\ 40 : -- -- -- -- -- -- -- -- 48 -- -- -- -- -- -- -- 
\ 50 : -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
\ 60 : -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
\ 70 : -- -- -- -- -- -- -- --                         
\  ok<$,ram> 

