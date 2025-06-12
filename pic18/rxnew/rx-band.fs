\ pic18f26K42 and si5351a, NCO used  as BFO
\ #sendm f/forget f/case f/2value 18f/qmath
\ #sendm pic18/rxnew/strcat pic18/rxnew/i2c-base-b pic18/rxnew/knobi
\ #sendm pic18/rxnew/ncok42 pic18/rxnew/dogs164e pic18/rxnew/si5351-d 
\ #sendm pic18/rxnew/rx


\ 
single
fl+
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
$fec1 constant fvrcon
$fef8 constant adcon0
$feff constant adclk
$fef1 constant adpch
$feef constant adres
$fefd constant adref
$fef5 constant adcap
$fef3 constant adacql

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
2variable if1Offset
create $buf 20 allot
create $rx-freq 10 allot

: 2, swap , , ;

eeprom
9 constant fm-list#
0 2array: fm-list
#87.900.00. 2,
#91.900.00. 2,
#94.000.00. 2,
#97.500.00. 2,
#98.100.00. 2,
#98.900.00. 2,
#99.400.00. 2,
#101.100.00. 2, 
#106.200.00. 2,

4 constant lsb-bands#
0 2array: lsb-bands
1800 1900 2,
3500 3800 2,
7000 7200 2,
0    18200 2,

4 constant usb-bands#
0 2array: usb-bands
10100 10200 2,
14000 14350 2,
18060 18170 2,
0     18200 2,

0 2array: e.lsb-freqs
1846.00. 2,
3699.00. 2,
7066.00. 2,
10000.00. 2,

0 2array: e.usb-freqs
10100.00. 2,
14200.00. 2,
18150.00. 2,
15000.00. 2,

12 constant am-bands#
0 2array: am-bands
150 300 2,
522 1602 2,
3950 4000 2,
4700 5100 2,
5850 6230 2,
7200 7600 2,
9300 10000 2,
11600 12100 2,
13550 13850 2,
15000 15700 2,
17400 17800 2,
0     18200 2,

0 2array: e.am-freqs
198.00. 2,
1089.00. 2,
3955.00. 2,
4950.00. 2,
6185.00. 2,
7415.00. 2,
9400.00. 2,
11900.00. 2,
13755.00. 2,
15590.00. 2,
17490.00. 2,
10000.00. 2,

create e.fm-freq 101.100.00. 2,

ram
am-bands# 2array: am-freqs
lsb-bands# 2array: lsb-freqs
usb-bands# 2array: usb-freqs
variable lsb-band \ 0-2
variable usb-band
variable am-band
variable fm-band

variable newband
ram 2variable fm-freq
2variable newrxf

eeprom
#10.730.000. 2value fmOffset
#455.000.   2value if2Offset
#1493. 2value usbOffset
-#1289. 2value lsbOffset

eeprom
defer mode0
defer bw0
0 value term?
ram
variable vol
variable newVol

: modeAm? mode @ modeAm = ;
: modeFm? mode @ modeFm = ;

: rxf ( -- a )
  mode @ case
    modeAm   of am-band @ am-freqs endof
    modeUsb  of usb-band @ usb-freqs endof
    modeLsb  of lsb-band @ lsb-freqs endof
    modeFm   of fm-freq endof
  endcase
;
: rxf@ ( -- d ) rxf 2@ ;
: rxf! ( d -- ) rxf 2! ;

: band ( -- a )
  mode @ case
    modeAm   of am-band  endof
    modeUsb  of usb-band endof
    modeLsb  of lsb-band endof
    modeFm   of fm-band endof
  endcase
;
: band@ ( -- n ) band @ ;
: band! ( n -- )
  mode @ case
    modeAm   of am-bands# 1- min am-band  endof
    modeUsb  of usb-bands# 1- min usb-band endof
    modeLsb  of lsb-bands# 1- min lsb-band endof
    modeFm   of 0 min fm-band endof
  endcase !
;

: offsetsInit 44.545.000. if2Offset d+ if1Offset 2! ;

: change? ( a a -- f ) @ swap  @ <> ;

: fm.ad/
  1 ansela c!
  1 trisa mset
;

: am.ad
  %1000 ansela mset
  %1000 trisa mset
  %0000.0100 adcon0 c!
  %0011.1111 adclk c!
  0 adref c!
  3 adpch c!
  %1000.0100 adcon0 c!  \ Fosc
;
: fm.ad
  %0001 ansela mset
  %0001 trisa mset
  %0000.0100 adcon0 c!  \ Internal osc
  %0000.0000 adpch c!   \ AN0
  %1000.0100 adcon0 c!  \ Fosc
;

: fmOn %1.0000 lata mset fm.ad ;
: amOn %1.0000 lata mclr am.ad ;

\ Initialize ports
: io.init ( -- )
  %0110 lata mclr
  %1.0000 lata mode0 modeFm? if mset else mclr then 
  %1111 latb mclr
  %1.0110 trisa mclr
  %1111 trisb mclr
;

: dac.init ( -- ) $90 dac1con0 c! ;

\ Turn off not used peripherals
: pmd.init ( -- ) \ pic18f26k42
  %0011.1010 $f9c0 c!
  %0111.1100 $f9c1 c!
  %0000.0111 $f9c2 c!
  %1111.1111 $f9c3 c!
  %1110.0000 $f9c4 c!
  %0010.0110 $f9c5 c!
  %0011.1111 $f9c6 c!
  %0000.0011 $f9c7 c!
;
: am.ad@ ( -- u )
  %1000.0101 adcon0 c!
  begin adcon0 c@ 1 and 0= until
  adres @
;
: fm.ad@ ( -- u )
  %1000.0101 adcon0 c!
  begin adcon0 c@ 1 and 0= until
  adres @
;
\ 100 mV - 3377 mV
\ 120 - 4095
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

\ Set si5351 vco to about 800 MHz
: divider!
  #8000 rxf@ #10000 um/mod nip 450 + / aligned
  dup divider <> 
  if am.init else drop then 
;

: rx-freq  ( -- ) \ Ten herz resolution
  rxf@ #10 ud*
  modeFm? 
  if   fmOffset d- fm.f
  else 
       divider!
       rxOffset 2@ d+ if1Offset 2@ d+ am.f
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
       nco/ nco.real if2Offset d- rxOffset 2!
  then
  mode @ case
    modeAm  of amOn det-am  endof
    modeUsb of amOn det-ssb endof
    modeLsb of amOn det-ssb endof
    modeFm  of fmOn fm.init det-fm  endof
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
: rxf!! ( d -- ) 2dup rxf! newrxf 2! ;
: limits-fm
  rxf@ 2dup
   #87.500.00. d< if  #87.500.00. rxf!! then 
  #108.000.00. d> if #108.000.00. rxf!! then 
;

: limits ( -- )
  rxf@ #100 um/mod >r drop
  mode @ case
    modeAm   of am-band @ am-bands  endof
    modeUsb  of usb-band @ usb-bands endof
    modeLsb  of lsb-band @ lsb-bands endof
    modeFm   of rdrop limits-fm exit endof
  endcase
  2@ dup r@ < if #100 um* rxf!! else drop then
     dup r> > if #100 um* rxf!! else drop then
;

: rx-set
  newMode mode change?
  if 0 vol ! 0 vol! 100 to divider
     newMode @ mode ! det-set filter-set
     band@ newband !
     rxf@ 2dup newrxf 2! 1+ rxf! #100 ms
  then
  newband @ band@ <>
  if  newband @ band! band@ newband ! 
      rxf@ newrxf 2! rx-freq
  then
  newrxf 2@ rxf@ d= 0=
  if newrxf 2@ rxf! limits rx-freq
  then
  vol-set
  modeFm? if fm.presel! then
;

' fm is mode0

: rx-reset ( -- )
  cr
  decimal
  pmd.init
  io.init
  dac.init
  i2c.init
  am.ad
  0 s-old !
  0 xtalOffset !
  offsetsInit
  nco/
  100 to divider
  5 am-band ! 1 usb-band ! 2 lsb-band !
  0 e.am-freqs 0 am-freqs am-bands# cells cells cmove
  0 e.usb-freqs 0 usb-freqs usb-bands# cells cells cmove
  0 e.lsb-freqs 0 lsb-freqs lsb-bands# cells cells cmove
  e.fm-freq 2@ 2dup fm-freq 2! newrxf 2!
  true mode ! mode0
  filter8K amFilter !
  rx-set
  #8 dup  vol! newVol ! 
  knob-init
  dogs.init
;

: mem
  fl+
  fm-freq 2@ e.fm-freq 2!
  mode @
  case
    modeAm   of ['] am  endof
    modeUsb  of ['] usb endof
    modeLsb  of ['] lsb endof
    modeFm   of ['] fm  endof
  endcase
  is mode0
  fl- ;

: rx-freq>$ 
  decimal rxf@
  modeFm?
  if   #1000 ud/mod rot drop
       <# # # [char] . hold #s #>
  else <# # # [char] . hold # # # # #s #> 
  then $rx-freq place ;

: .mode mode @ [char] A + emit ;

: .bw  \ A = 3 KHz B=10 KHz C=WFM D=5KHz
  filter @ case
    filter3K of [char] A endof
    filter4K of [char] D endof
    filter8K of [char] B endof
    filterFM of [char] C endof
  endcase emit
;
: s.add < if 1+ then ;
: am.s.get ( -- n ) \ 0-15
  0
  am.ad@ >r
  r@ 3700 s.add  \ s1   -105
  r@ 3300 s.add  \ s2   -101
  r@ 3000 s.add  \ s3   -97
  r@ 2520 s.add  \ s4   -93
  r@ 2200 s.add  \ s5   -89
  r@ 1900 s.add  \ s6  -85
  r@ 1750 s.add  \ s7  -81
  r@ 1600 s.add  \ s8  -77
  r@ 1500 s.add  \ s9  -73
  r@ 1300 s.add  \ +10
  r@ 1050 s.add  \ +20
  r@ 800  s.add  \ +30
  r@ 600  s.add  \ +40
  r@ 400  s.add  \ +50
  r> 200  s.add  \ +60
;
: fm.s.get ( -- n ) fm.ad@ #256 / ;
: s.get modeFm? if fm.s.get else am.s.get then ;

: fm.s.line s" 0-1-2-3-4-5-6-7" drop ;
: am.s.line s" 1-3-5-7-9-2-4-6" drop ;
: s.space   s"                " drop ;
: am.s.meter ( n -- a u ) \ 0-15
  >r am.s.line r@ $buf place
  $buf c@+ s.space #15 r> - $cat 
;
: fm.s.meter ( n -- a u ) \ 0-15
  >r fm.s.line r@ $buf place
  $buf c@+ s.space #15 r> - $cat 
;
: s.meter modeFm? if fm.s.meter else am.s.meter then ;

: s-on fl+ true to term? warm ;
: s-off fl+ false to term? warm ;

: >dogs ['] dogs.data 'emit ! ;
: >uart ['] tx1 'emit ! ;
: dogs.bw ( -- )
  filter @ case
    filter3K of s" BW 2.7 KHz" endof
    filter4K of s" BW 4 KHz"   endof
    filter8K of s" BW 9 KHz"   endof
    filterFM of s" BW 200 KHz" endof
  endcase dogs.type
;
: dogs.mode
  mode @ case
    modeAm  of s" AM  " endof
    modeUsb of s" USB " endof
    modeLsb of s" LSB " endof
    modeFm  of s" FM  " endof
  endcase
;

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
: f-1000 -10000 rd ;
: f+1000 10000 rd ;
: b-down modeFm? if -10000 rd else band@ 1- 0 max newband ! then ;
: b-up   modeFm? if  10000 rd else band@ 1+ newband ! then ;

: offset+- ( n -- ) s>d rxOffset 2@ d+ rxOffset 2! det-set rx-freq ;

: offset-
  mode @ case
    modeUsb of -50 endof
    modeLsb of  50 endof
    default 0 endof
  endcase offset+- ;

: offset+
  mode @ case
    modeUsb of  50 endof
    modeLsb of -50 endof
    default 0 endof
  endcase offset+- ;

: nop ;
flash create exec
   ' am , ' f+1000 , ' offset+ ,     ' lsb ,  \ abcd
  ' f-5 ,     ' fm ,  ' narrow ,  ' middle ,  \ efgh
' f+500 ,   ' wide ,     ' vo- ,     ' vo+ ,  \ ijkl
  ' mem ,    ' nop ,  ' b-down ,    ' b-up ,  \ mnop
  ' f-1 , ' f+5    ,     ' usb ,   ' f-100 ,  \ qrstx
' f-500 , ' f-1000 ,     ' f+1 , ' offset- ,  \ uvwx
' f+100 ,  ' abort ,                          \ yz
ram

: c>s ( c -- n) dup $7f > if $ff00 or then ;

: knob-lores ( -- f ) \ True if value has changed
  knob1 c@  [ knob1 a, clrf, ]
  dup 0= if exit then 
  127 > if b-down else b-up then true
;
: knob-vol ( -- f )
  knob2 c@  [ knob2 a, clrf, ]
  dup 0= if exit then 
  127 > if vo- else vo+ then true
;
: knob-hires ( -- f )
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
  s-old @ s.get = if exit then
  s.get dup s-old ! 1 2 dogs.cursor s.meter dogs.type
;

: app.display $rx-freq c@+ type .mode .bw $d emit ;
: term.display 
  space $rx-freq c@+ type space s-old @ s.meter type $d emit ;
: dogs.display 
  dogs.mode $buf place
  $buf c@+ $rx-freq c@+ $cat
  s"    " $cat
  1 0 dogs.cursor dogs.type
;

: chan? ( c limit -- c f ) >r [char] 1 -  dup 0 r> within ;
: short-keys ( n -- )
  modeFm? term? and
  if fm-list# chan? if fm-list 2@ newrxf 2! else drop then
  else drop
  then ;

: rx
  fl- >uart true
  offsetsInit
  begin
    if   rx-set vol-set rx-freq>$ dogs.display 
         term? if term.display else app.display then
    else s.display 
         term? ticks $ff and 0= and if term.display then
    then
    pause key? 
    if   true
         key dup [char] a - dup 0 #26 within
         if cells exec + @ex else drop then
         term? if short-keys else drop then
    else false
    then
    knob-lores or knob-vol or knob-hires or
    knob-1 or knob-2 or
  again ;

: F #100 um* newrxf 2! rx ;

: rx-init rx-reset rx ;
\ rx-init
\ ' rx-init is turnkey

