\ EA DOGS164 display routines
\ needs i2c-base.fs
-dogs164e
marker -dogs164e
decimal
$ff8a constant latb
$ff93 constant trisb
$3c constant dogs164e

: dogs.reset $04 trisb mclr $04 latb mclr 1 ms $04 latb mset ;
: dogs.i2c! ( c c -- ) dogs164e i2c.write i2c.c! i2c.c! i2c.stop ;
: dogs.cmd  ( c -- ) $00 dogs.i2c! ;
: dogs.data ( c -- ) $40 dogs.i2c! ;
: dogs.cmds ( a b c ... n -- ) for dogs.cmd next ;
: dogs.cursor ( column row -- ) $20 * + 4 + $80 or dogs.cmd ;
: dogs.home 0 0 dogs.cursor ;
: dogs.type ( addr n -- )
  dogs164e i2c.write for c@+ dogs.data next drop ;
hex
: dogs.init
  i2c.init
  dogs.reset
  0c 1 38 6b 57 6c 1b 39 1e 05 09 3a #12 dogs.cmds
  3c 1f 3a 3 dogs.cmds
  dogs.home
;
decimal

