\ EA DOGS164 display routines
-dogs164e
marker -dogs164e
decimal
$ffbb constant latb
$ffc3 constant trisb

: dogs.reset $40 trisb mclr $40 latb mclr 1 ms $40 latb mset ;
: dogs.i2c!  $3c i2c.write i2c.2c! ;
: dogs.cmd  ( c -- ) $00 dogs.i2c! ;
: dogs.data ( c -- ) $40 dogs.i2c! ;
: dogs.cmds ( a b c ... n -- ) for dogs.cmd next ;
: dogs.cursor ( column row -- ) $20 * + 4 + $80 or dogs.cmd ;
: dogs.home 0 0 dogs.cursor ;
: dogs.type ( addr n -- ) for c@+ dogs.data next drop ;
hex
: dogs.init
  i2c.init
  dogs.reset
  0f 1 38 6b 56 6c 1b 39 1e 05 09 3a #12 dogs.cmds
  dogs.home
;
decimal

