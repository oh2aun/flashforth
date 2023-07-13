\ pic18f26K42 and si5351a, NCO used  as BFO
\ needs case.fs 2value.fs 
\ needs qmath.fs  i2c-base-b ncok42.fs si5351-d.fs
\ needs knobi.fs
\ needs dogs164e.fs

fl+ busy
-rx
marker -rx
decimal

\ Assembler
$e2 constant cc,    \ bc
$ffed constant Sminus
$ffef constant Srw
$ffe1 constant Tp
$34 as3 rlcf,           ( f d a -- )  
$0e00 as1 movlw,        ( k -- )  
$6e as2 movwf,          ( f a -- )    
$04 as3 decf,           ( f d a -- )  
$70 as3 btg,            ( f b a -- )

: 2array: create cells cells allot does> swap cells cells + ;

\ Register definitions
$ffba constant lata
$ffbb constant latb
$ffbc constant latc

$ffc2 constant trisa
$ffc3 constant trisb
$ffc4 constant trisc

$fe9e constant dac1con0
$fe9c constant dac1con1
$fef8 constant adcon0
$fef1 constant adpch
$feef constant adres

ram
variable mode
variable newMode
0 constant modeAm
1 constant modeUsb
2 constant modeLsb
3 constant modeFm

0 constant filter8K
1 constant filter4K
2 constant filter3K
3 constant filterFM
variable filter
variable newFilter
variable amFilter
variable s-old
2variable rxOffset
eeprom 
#45.000.038. 2value if1Offset
#10.730.000. 2value fmOffset
#455.000.   2value if2Offset
#1500. 2value usbOffset
-#1300. 2value lsbOffset

ram
2variable newrxf
4 2array: rxf          \ Use mode as index

eeprom
4 2array: RX0
defer mode0
defer bw0
ram
variable vol
variable newVol

: modeAm? mode @ modeAm = ;
: modeFm? mode @ modeFm = ;
: fmOn  %1.0000 lata mset ;
: amOn  %1.0000 lata mclr ;

: change? ( a a -- f ) @ swap  @ <> ;

: rxf@ ( -- d ) mode @ rxf 2@ ;  \ 10 Hz resolution
: rxf! ( d -- ) mode @ rxf 2! ;  \ 10 Hz resolution

\ Initialize ports
: io.init ( -- )
  %0110 lata mclr
  %1.0000 lata mode0 modeFm? if mset else mclr then 
  %1111 latb mclr
  %1.0110 trisa mclr
  %1111 trisb mclr
  $d4 adcon0 c!
  $d5 adcon0 c!
  3 adpch c!
;

: dac.init ( -- ) $90 dac1con0 c! ;

\ Turn off not used peripherals
: pmd.init ( -- ) \ pic18f26k42
  %0111.1010 $f9c0 c!
  %0111.1100 $f9c1 c!
  %0000.0111 $f9c2 c!
  %1111.1111 $f9c3 c!
  %1110.0000 $f9c4 c!
  %0010.0110 $f9c5 c!
  %0011.1111 $f9c6 c!
  %0000.0011 $f9c7 c!
;

: filter3k [ lata 2 a, bcf, lata 1 a, bcf, ] ;
: filter4k [ lata 2 a, bcf, lata 1 a, bsf, ] ;
: filter8k [ lata 2 a, bsf, lata 1 a, bcf, ] ;
: det-fm   [ latb 0 a, bsf, latb 1 a, bsf, ] ;
: det-am   [ latb 0 a, bcf, latb 1 a, bsf, ] ;
: det-ssb  [ latb 0 a, bcf, latb 1 a, bcf, ] ;
: det-mute [ latb 0 a, bsf, latb 1 a, bcf, ] ;

decimal
flash
create presel
   4 c,  5 c,  5 c,  6 c,  7 c,  \ 87 - 91 MHz
   8 c,  9 c, 10 c, 11 c, 12 c,  \ 92 - 96 MHz
  13 c, 15 c, 16 c, 17 c, 18 c,  \ 97 - 101 MHz
  19 c, 21 c, 22 c, 24 c, 25 c,  \ 102 - 106 MHz
  27 c, 28 c,                    \ 107 - 108 MHz
ram

: fm.presel! ( -- )
  rxf@ #10000 um/mod nip #10 u/mod nip
  #87 - presel + c@ dac1con1 c!
;

3 constant sdata
2 constant sclk

: spi! ( u -- )
  [ #2 movlw, ]
  [ Tp a, movwf, ]
  [ begin, ]
    [ #8 movlw, ]
    [ Tp 1+ a, movwf, ]
    [ begin, ]
      [ latb sclk a, bcf, ]
      [ Srw f, a, rlcf, ]
      [ cc, if, latb sdata a, bsf, else, latb sdata a, bcf, then, ]
      [ latb sclk a, bsf, ]
    [ Tp 1+ f, a, decf,  ]
    [ z, until, ]
    [ Sminus w, a, movf, ]
  [ Tp f, a, decf,  ]
  [ z, until,  ]
  [ latb sclk a, bcf, ]
;

: vol! ( u -- )
  #127 swap -
  $007f and \ dup 
  spi!
;

\ Set si5351 vco to about 1.1 GHz
\ My SI5351 vco can go up to 1.3 GHz
: divider!
  #11000 rxf@ #10000 um/mod nip 450 + / aligned
  dup divider <> 
  if a.init else drop then 
;

: rx-freq  ( -- ) \ Ten herz resolution
  rxf@ #10 ud*
  modeFm? 
  if   fmOffset d- b.f
  else 
       divider!
       rxOffset 2@ d+ if1Offset d+ a.f
  then
;

: bfo-off   0 nco1con c! ;
: bfo-freq ( Hz. -- ) \ 8 Hz resolution with 16MHz clock
  $80 nco1con c! nco.f ;

: amOffset
  mode @ modeAm =
  if 
    filter @ filter8K = 
    if 500. rxOffset 2!
    else 0. rxOffset 2!
    then
  then ;

: am  amFilter @ filter ! modeAm  newMode ! ;
: usb filter3K filter ! modeUsb newMode ! usbOffset rxOffset 2! ;
: lsb filter3K filter ! modeLsb newMode ! lsbOffset rxOffset 2! ;
: fm  modeFm newMode ! fmOffset rxOffset 2! filterFM filter ! ;

: det-set
  modeAm? modeFm? or
  if   bfo-off
  else if2Offset rxOffset 2@ d+ bfo-freq
  then
  mode @ case
    modeAm  of amOn det-am  endof
    modeUsb of amOn det-ssb endof
    modeLsb of amOn det-ssb endof
    modeFm  of fmOn b.init det-fm  endof
  endcase
;
: filter-set
  filter @ case
    filter3K of filter3k endof
    filter4K of filter4k endof
    filter8K of filter8k endof
  endcase amOffset
;
ram variable muted
: mute det-mute true muted ! ;
: unmute 
  muted @ if 
    mode @ case
      modeAm  of det-am  endof
      modeUsb of det-ssb endof
      modeLsb of det-ssb endof
      modeFm  of det-fm  endof
    endcase
  then
  false muted !
;

: vol-set
  newVol vol change?
  if 
    newVol @ 0= 
    if   mute
    else unmute newVol @ dup vol ! 1- vol!
    then
  then
 ;
  
: limit-freq
  modeFm? 
  if   newrxf 2@  #87.500.00. d< if #87.500.00. newrxf 2! then 
       newrxf 2@ #108.000.00. d> if #108.000.00. newrxf 2! then 
  else newrxf 2@ 0. d< if 0. newrxf 2! then 
       newrxf 2@ #22.000.00. d> if #22.000.00. newrxf 2! then
  then ;

: rx-set
  newMode mode change?
  if 0 vol ! 0 vol!
     newMode @ mode ! det-set filter-set 
     rxf@ 2dup newrxf 2! 1+ rxf! #100 ms
  then
  newrxf 2@ rxf@ d- d0= 0=
  if limit-freq newrxf 2@ rxf! rx-freq
  then
  vol-set
  modeFm? if fm.presel! then
;

#09.400.00.  modeAm  RX0 2!
#03.699.00.  modeLsb RX0 2!
#14.200.00.  modeUsb RX0 2!
#103.700.00. modeFm  RX0 2!

' fm is mode0

: rx-reset ( -- )
  cr
  decimal
  pmd.init
  io.init
  dac.init
  i2c.init
  nco/
  100 to divider
  0 RX0 0 rxf #4 cells cells cmove
  true mode ! mode0
  filter8K amFilter !
  rx-set
  #8 dup  vol! newVol ! 
  knob-init
  dogs.init
;

: mem
  fl+ 
  0 rxf 0 RX0 #4 cells cells cmove 
  mode @
  case
    modeAm   of ['] am  endof
    modeUsb  of ['] usb endof
    modeLsb  of ['] lsb endof
    modeFm   of ['] fm  endof
  endcase
  is mode0
  fl- ;

: .rx-freq 
  decimal rxf@
  modeFm?
  if   #1000 ud/mod rot drop
       <# # # [char] . hold #s #>
  else <# # # [char] . hold # # # # #s #> 
  then type ;

: .mode mode @ [char] A + emit ;

: .bw  \ A = 3 KHz B=10 KHz C=WFM D=5KHz
  filter @ case
    filter3K of [char] A endof
    filter4K of [char] D endof
    filter8K of [char] B endof
    filterFM of [char] C endof
  endcase emit
;
  filter @ [char] @ + emit ;

: s.get ( -- c ) $fff adres @ - 8 rshift 2- 0 max ;

: s.meter ( n -- ) \ 0-15
  15 over - swap
  for $2d emit next
  spaces
;
: >dogs ['] dogs.data 'emit ! ;
: >uart ['] tx1 'emit ! ;
: dogs.bw ( -- )
  filter @ case
    filter3K of s" BW 2.7 KHz" endof
    filter4K of s" BW 4 KHz"   endof
    filter8K of s" BW 9 KHz"   endof
    filterFM of s" BW 200 KHz" endof
  endcase type
;
: dogs.mode
  mode @ case
    modeAm  of s" AM " endof
    modeUsb of s" USB" endof
    modeLsb of s" LSB" endof
    modeFm  of s" FM " endof
  endcase type space
;
: dogs.display 
  >dogs 
  3 0 dogs.cursor dogs.mode .rx-freq 3 spaces
  3 1 dogs.cursor dogs.bw 8 spaces
  >uart ;

: rd ( n -- ) s>d 
  rxf@ newrxf 2@ d- or
  if   newrxf 2@ 
  else rxf@ 
  then d+ newrxf 2! 
 ;

: vo+ vol @ #15 * #10 / 2 max #128 min newVol ! ;
: vo- vol @ #10 * #15 / 0 max newVol ! ;

: lw.mw? 
  rxf@ #100 um/mod nip  dup #150 #300 within  swap #530 #1700 within or ;

: am9khz 
  modeAm? if 
    lw.mw? if
       #900 swap ?negate
       rxf@ #100 um/mod nip
       #9 u/mod nip #900 um* 2dup rxf! newrxf 2!
    then
  then ;

: bw-set 
  modeAm? 
  if   filter ! filter-set filter @ amFilter !
  else drop 
  then ;
  
: narrow filter3K bw-set ;
: middle filter4K bw-set ;
: wide   filter8K bw-set ;

: f-1 -1 rd ;
: f+1 1 rd ;
: f-5 -5 rd ;
: f+5 5 rd ;
: f-10 -10 rd ;
: f+10 10 rd ;
: f-100 -100 rd ;
: f+100 100 rd ;
: f-500 -500 am9khz rd ;
: f+500 500 am9khz rd ;
: f-250 -25000 rd ;
: f+250 25000 rd ;
: f-1000 -10000 rd ;
: f+1000 10000 rd ;

: nop ;
flash create exec
   ' am , ' f+1000 ,    ' nop ,    ' lsb ,  \ abcd
  ' f-5 ,     ' fm , ' narrow , ' middle ,  \ efgh
' f+500 ,   ' wide ,    ' vo- ,    ' vo+ ,  \ ijkl
  ' mem ,    ' nop ,  ' f-250 ,  ' f+250 ,  \ mnop
  ' f-1 , ' f+5    ,    ' usb ,  ' f-100 ,  \ qrstx
' f-500 , ' f-1000 ,    ' f+1 ,    ' nop ,  \ uvwx
' f+100 ,  ' abort ,                        \ yz
ram

: c>s ( c -- n) dup $7f > if $ff00 or then ;

: knob-lores ( -- f ) \ True if value has changed
  knob1 c@  [ knob1 a, clrf, ]
  dup 0= if exit then 
  127 > if f-1000 else f+1000 then true
;
: knob-vol
  knob2 c@  [ knob2 a, clrf, ]
  dup 0= if exit then 
  127 > if vo- else vo+ then true
;
: knob-hires
  knob3 c@  [ knob3 a, clrf, ]
  dup 0= modeFm? or if exit then 
  c>s dup 0<
  if   negate for modeAm? if f-10 else f-1 then next
  else for modeAm? if f+10 else f+1 then next
  then true
;
: knob-1
  false
  switch1 c@ if
   [ switch1 a, clrf, ]
    mode @ case
      modeAm  of fm endof
      modeUsb of am endof
      modeLsb of usb endof
      modeFm  of lsb endof
    endcase
    0=
  then
;
: knob-2
  false
  switch2 c@ if
   [ switch2 a, clrf, ]
    modeAm? if
      filter @ case
        filter3K of wide endof
        filter4K of narrow endof
        filter8K of middle endof
      endcase
      0=
    else
      mem
    then
  then
;
: s.display
  s-old @ s.get <> if
    >dogs 
    s.get dup s-old ! 3 2 dogs.cursor s.meter 
    >uart
  then 
;
: rx
  fl- >uart true
  begin
    case
      1     of rx-set vol-set .rx-freq .mode .bw $d emit endof
      true  of rx-set vol-set dogs.display endof
      false of s.display endof
    endcase
    idle pause busy key? 
    if   1
         key [char] a - dup 0 #26 within
         if cells exec + @ex else drop then
    else false
    then
    knob-lores or knob-vol or knob-hires or
    knob-1 or knob-2 or
  again ;

: rx-init rx-reset rx ;
\ ' rx-init is turnkey

