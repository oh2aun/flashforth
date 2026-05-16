\ #sendm ../forth/qmath i2c1-base-b i2c2-base-b 
fl+
-si570
marker -si570
decimal

\ i2c device address
$55 constant si570
: si1c! ( c reg -- ) si570 i2c.write i2c.2c! ;
: si1c@ ( reg -- c ) si570 i2c.rc@ ;
: si2c! ( c reg -- ) si570 i2c2.write i2c2.2c! ;
: si2!  ( u reg -- ) si570 i2c2.write i2c2.c!! ; 
: si2c@ ( reg -- c ) si570 i2c2.rc@ ;

: si1f! ( u u u reg -- ) \ write new frequency
  7 i2c1cnt c! 
  i2c1txb c! \ register, autoincrement
  i2c.start
  3 for
    i2c.txbe
    dup 8 rshift  i2c1txb c!
    i2c.txbe
    i2c1txb c!
  next 
  i2c.stop
;

eeprom
#114.251.700. 2value clk1
#114.242.014. 2value clk2
ram 
2variable clock1
2variable clock2
2variable freq1
2variable freq2
4 constant hs
variable n
variable n-prev
0 value freeze?

: si1freeze ( -- ) #137 si1c@ $10 or  #137 si1c! ;
: si1melt   ( -- ) #137 si1c@ $ef and #137 si1c! $40 #135 si1c! ;
: si1pause  ( -- ) #135 si1c@ $20 or  #135 si1c! ;
: si1run    ( -- ) #135 si1c@ $df and #135 si1c! ;

: si2freeze ( -- ) #137 si2c@ $10 or  #137 si2c! ;
: si2melt   ( -- ) #137 si2c@ $ef and #137 si2c! $40 #135 si2c! ;

: findn ( d -- n )
  #1000 um/mod nip
  dup #4400 #5000 within if drop #28 exit then 
  dup #5000 #5100 within if drop #26 exit then
  dup #5100 #6000 within if drop #24 exit then
  dup #6000 #6500 within if drop #22 exit then
      #6500 #6800 within if      #20 exit then 
                                 #18
;
: ?freeze ( d d -- )
  d- dabs 150.00. d> to freeze? ;
 
: si.vco ( d -- q ) 
  hs n c@ um* uq* 2drop $1000.0000. uq* ;
: si.div ( q d -- d n ) 
  uq/ #10. uq* drop n c@ 1- #6 lshift or ;

: si1freq ( d -- ) \ 10 Hz resolution, #50.000.00. == 50 MHz
  2dup n @ n-prev ! findn n c!
  2dup freq1 2@ ?freeze
  n @ n-prev @ <> if true to freeze? then  
  freeze? if si1freeze 2dup freq1 2! else si1pause then
  si.vco clock1 2@ si.div  #7 si1f! ( #9 si1! #11 si1! )
  freeze? if si1melt else si1run then
;

: si2freq ( d -- ) \ 10 Hz resolution
  2dup findn n c!
  si2freeze
  si.vco clock2 2@ si.div  #7 si2! #9 si2! #11 si2!
  si2melt
;

: si1init ( clock -- )
  i2c.init
  clock1 2!
  0. freq1 2!
;

: si2init ( clock -- )
  i2c2.init
  clock2 2!
  0. freq2 2!
;

