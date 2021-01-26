\ Read temperature from TC74 on I2C bus.
\ Requires i2c-base.txt to be previously loaded.
\ Modelled on Mikael Nordman's i2c_tcn75.txt.
\ Peter J. 2014-10-28

-tc74-app
marker -tc74-app

%1001000 constant addr-TC74A0

: tc74-init ( -- )
  \ Selects temperature register for subsequent reads.
  addr-TC74A0 i2c.addr.write if 0 i2c.c! drop then i2c.stop
;

: sign-extend ( c -- n )
  \ If the TC74 has returned a negative 8-bit value,
  \ we need to sign extend to 16-bits with ones.
  dup $7f > if $ff80 or then
;

: degrees@ ( -- n ) 
  \ Wake the TC74 and fetch its register value.
  addr-TC74A0 i2c.addr.read if i2c.c@.nack else 0 then i2c.stop 
  sign-extend
;

: tc74-main ( -- ) 
  i2c.init
  tc74-init
  begin
    degrees@ . 
    #1000 ms
  key? until
;

\ Now, report temperature in degrees C 
\ while we warm up the TC74 chip with our fingers...
\
\ decimal tc74-main 23 23 23 23 23 23 23 24 24 24 25 25 26 26 26 26 26 27 
