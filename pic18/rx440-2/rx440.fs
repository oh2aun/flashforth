\ pic18f26K42 and 2xSI570, NCO used as BFO
\ #sendm f/forget f/case f/2value 18f/qmath f/vt100
\ #sendm pic18/rx440-2/strcat pic18/rx440-2/i2c-base-b pic18/rx440-2/i2c2-base-b
\ #sendm pic18/rx440-2/ncok42 pic18/rx440-2/si570 
\ #sendm pic18/rx440-2/rx440

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
$fa40 constant ansela

$fce5 constant clkrcon
$fce6 constant clkrclk
$27 constant clkr
$fa10 constant rc0pps

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

5 constant lsb-list#
0 2array: lsb-list
#1.846.00. 2,
#3.699.00. 2,
#5.360.00. 2,
#7.066.00. 2,
#14.997.00. 2,

9 constant usb-list#
0 2array: usb-list
#05.363.00. 2,
#10.100.00. 2,
#14.200.00. 2,
#18.150.00. 2,
#21.150.00. 2,
#10.138.70. 2,
#14.095.60. 2,
#18.104.60. 2,
#21.094.60. 2,


9 constant am-list#
0 2array: am-list
#1.386.00. 2,
#1.089.00. 2,
#7.415.00. 2,
#9.400.00. 2,
#12.095.00. 2,
#13.755.00. 2,
#15.590.00. 2,
#17.490.00. 2,
#21.300.00. 2,
ram

eeprom
#10.730.00. 2value fmOffset
#455.00.   2value if2Offset
#170.  2value usbOffset
-#140. 2value lsbOffset
ram
2variable lo2-freq
2variable newrxf
4 2array: rxf          \ Use mode as index

eeprom
4 2array: RX0
defer mode0
defer bw0
1 value term?

ram
variable vol
variable newVol

: modeAm? mode @ modeAm = ;
: modeFm? mode @ modeFm = ;

: change? ( a a -- f ) @ swap  @ <> ;

: rxf@ ( -- d ) mode @ rxf 2@ ;  \ 10 Hz resolution
: rxf! ( d -- ) mode @ rxf 2! ;  \ 10 Hz resolution

: offsets-init ( d-- ) lo2-freq 2@ if2Offset d+ if1Offset 2! ;

: fm.ad/
  1 ansela c!
  1 trisa mset
;

: am.ad
  %0000.0100 adcon0 c!
  %0011.1111 adclk c!
  0 adref c!
  0 adpch c!
  %1000.0100 adcon0 c!  \ Fosc
;
: fm.ad
  1 ansela c!
  1 trisa mset
  %0000.0100 adcon0 c!  \ Internal osc
  %0000.0000 adpch c!   \ AN0
  %1000.0100 adcon0 c!  \ Fosc
;

: fmOn ( %1.0000 lata mset fm.ad ) ;
: amOn %0.0001 lata mclr am.ad ;

\ Initialize ports
: io.init ( -- )
  %0110 lata mclr
  %1.0000 lata mode0 modeFm? if mset else mclr then 
  %1111 latb mclr
  %1.0110 trisa mclr
  %0011.1111 trisb mclr
  %0000.1100 latb mset
  %0000.0000 trisc mclr
;

\ Turn off not used peripherals
: pmd.init ( -- ) \ pic18f26k42
  %0011.1001 $f9c0 c! \ sysclk fvr nvm clkrmd
  %0111.1101 $f9c1 c! \ nco tmr1
  %0100.0111 $f9c2 c! \ adc 
  %1111.1111 $f9c3 c!
  %1110.0000 $f9c4 c!
  %0010.0100 $f9c5 c! \ uart1 i2c2 i2c1 
  %0011.1111 $f9c6 c!
  %0000.0011 $f9c7 c!
;
: vee.init
  pps+
  clkr rc0pps c!
  pps-
  2 clkrclk c!
  $90 clkrcon c! \ 31 KHz out on RC0
;
\ 0-600 mV
\ 0-512 counts
: am.ad@ ( -- u )   \ 0-512 (4096)
  %1000.0101 adcon0 c!
  begin adcon0 c@ 1 and 0= until
  adres @
;
: fm.ad@ ( -- u )
  %1000.0101 adcon0 c!
  begin adcon0 c@ 1 and 0= until
  adres @
;
: filter3k [ latb 5 a, bsf, latb 4 a, bcf, ] ;
: filter4k ;
: filter8k [ latb 5 a, bcf, latb 4 a, bsf, ] ;


0 constant sdata
1 constant sclk
4 constant ssb-cs
8 constant am-cs


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

: det-fm ;
: det-am ( -- ) \ disconnect ssb wiper, connect am-wiper 
  ssb-cs latb mclr
  %0100.0000.0000.1000 spi!
  ssb-cs latb mset 
  am-cs latb mclr
  %0100.0000.0000.1111 spi!
  am-cs latb mset
;
: det-ssb ( -- ) \ disconnect am wiper connect ssb-wiper
  am-cs latb mclr
  %0100.0000.0000.1000 spi!
  am-cs latb mset
  ssb-cs latb mclr
  %0100.0000.0000.1111 spi!
  ssb-cs latb mset
;
: det-mute ( -- ) \ disconnect both wipers
  am-cs latb mclr
  %0100.0000.0000.1100 spi!
  am-cs latb mset
  ssb-cs latb mclr
  %0100.0000.0000.1100 spi!
  ssb-cs latb mset
;

: vol! ( u -- )
  256 umin dup
  am-cs latb mclr  spi!  am-cs latb mset
  ssb-cs latb mclr spi! ssb-cs latb mset
;

: rx-freq  ( -- ) \ Ten herz resolution
  rxf@
  modeFm? 
  if   ( fmOffset d- 2drop  fm.f )
  else 
       rxOffset 2@ d+ if1Offset 2@ d+ si1freq
  then
;

: bfo-off   0 nco1con c! ;
: bfo-freq ( Hz. -- ) \ 8 Hz resolution with 16MHz clock
  #10 ud* $80 nco1con c! nco.f ;

: amOffset mode @ modeAm = if  30. rxOffset 2!  then ;

: am  amFilter @ filter ! modeAm  newMode ! ;
: usb filter3K filter ! modeUsb newMode ! usbOffset rxOffset 2! ;
: lsb filter3K filter ! modeLsb newMode ! lsbOffset rxOffset 2! ;
: fm  ;

: det-set
  if2Offset rxOffset 2@ d+ bfo-freq
  mode @ case
    modeAm  of amOn det-am  bfo-off endof
    modeUsb of amOn det-ssb endof
    modeLsb of amOn det-ssb endof
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
    else unmute  newVol @ dup vol ! 1- vol!
    then
  then
 ;

: adjust-lo2  ( -- )
  newrxf 2@ #14.315.60. d>  newrxf 2@ #14.316.40. d< and
  if
    newrxf 2@ #14.316.00. d<
    if   44.547.50. else 44.542.50. then
  else
    newrxf 2@ #14.239.00. d>  newrxf 2@ #14.241.30. d< and
    if
      newrxf 2@ #14.240.10. d<
      if 44.547.50. else 44.542.50.  then
    else
      44.545.00.
    then
  then
  2dup lo2-freq 2@ d= 0=
  if   lo2-freq 2! offsets-init 
       lo2-freq 2@ si2freq
  else 2drop
  then
;
\ 44543 239.0-240.0
\ 44545 239.6-240.7
\ 44547 240.3-241.3
: limit-freq
  modeFm? 
  if   newrxf 2@  #87.500.00. d< if #87.500.00. newrxf 2! then 
       newrxf 2@ #108.000.00. d> if #108.000.00. newrxf 2! then 
  else newrxf 2@ 0. d< if 0. newrxf 2! then 
       newrxf 2@ #22.000.00. d> if #22.000.00. newrxf 2! then
       adjust-lo2
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
;

#07.415.00.  modeAm  RX0 2!
#07.066.00.  modeLsb RX0 2!
#14.200.00.  modeUsb RX0 2!
#101.100.00. modeFm  RX0 2!

' usb is mode0

: rx-reset ( -- )
  cr
  decimal
  pmd.init
  io.init
  i2c.init
  nco/
  clk2 si2init #44.545.00. lo2-freq 2! lo2-freq 2@ si2freq
  clk1 si1init #52.066.00. si1freq  
  am.ad
  0 s-old !
  offsets-init
  0 RX0 0 rxf #4 cells cells cmove
  true mode ! mode0
  filter8K amFilter !
  rx-set
  #256 dup  vol! newVol ! 
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
: s.mode
  mode @ case
    modeAm  of s" AM  " endof
    modeUsb of s" USB " endof
    modeLsb of s" LSB " endof
    modeFm  of s" FM  " endof
  endcase type
;

: s.add > if 1+ then ;
: ssb.s.get ( -- n ) \ 0-15  3.3 volt reference
  am.ad@  >r 0
  r@ #86  s.add \  s1   -121
  r@ #89  s.add \  s2   -115
  r@ #98  s.add \  s3   -109
  r@ #114 s.add \  s4   -103
  r@ #218 s.add \  s5   -97
  r@ #218 s.add \  s6   -91
  r@ #248 s.add \  s7   -85
  r@ #308 s.add \  s8   -79
  r@ #344 s.add \  s9   -73
  r@ #383 s.add \  +10
  r@ #417 s.add \  +20
  r@ #443 s.add \  +30
  r@ #471 s.add \  +40
  r@ #512 s.add \  +50
  r@ #548 s.add \  +60
  rdrop
;
: am.s.get ( -- n ) \ 0-15 3.3 volt reference
  am.ad@  >r 0
  r@ #147 s.add \  s1   -121
  r@ #150 s.add \  s2   -115
  r@ #161 s.add \  s3   -109
  r@ #176 s.add \  s4   -103
  r@ #210 s.add \  s5   -97
  r@ #255 s.add \  s6   -91
  r@ #285 s.add \  s7   -85
  r@ #338 s.add \  s8   -79
  r@ #368 s.add \  s9   -73
  r@ #407 s.add \  +10
  r@ #434 s.add \  +20
  r@ #459 s.add \  +30
  r@ #497 s.add \  +40
  r@ #537 s.add \  +50
  r@ #557 s.add \  +60
  rdrop
;
: fm.s.get ( -- n ) fm.ad@ #256 / ;

: s.get 
  mode @ case
    modeAm  of am.s.get  endof
    modeFm  of fm.s.get  endof
       default ssb.s.get endof
  endcase ;

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

: >uart ['] tx1 'emit ! ;

: rd ( n -- ) s>d 
  rxf@ newrxf 2@ d- or
  if   newrxf 2@ 
  else rxf@ 
  then d+ newrxf 2! 
 ;

: vo+ vol @ #15 * #10 / 2 umax #256 umin newVol ! ;
: vo- vol @ #10 * #15 / 0 umax newVol ! ;

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

: offset+- ( n -- ) s>d rxOffset 2@ d+ rxOffset 2! det-set rx-freq ;

: offset-
  mode @ case
    modeUsb of -5 endof
    modeLsb of  5 endof
    default 0 endof
  endcase offset+- ;

: offset+
  mode @ case
    modeUsb of  5 endof
    modeLsb of -5 endof
    default 0 endof
  endcase offset+- ;

: nop ;
flash create exec
   ' am , ' f+1000 , ' offset+ ,     ' lsb ,  \ abcd
  ' f-5 ,     ' fm ,     ' nop ,     ' nop ,  \ efgh
' f+500 ,    ' nop ,     ' vo- ,     ' vo+ ,  \ ijkl
  ' mem ,  ' \clsh ,   ' f-250 ,   ' f+250 ,  \ mnop
  ' f-1 , ' f+5    ,     ' usb ,   ' f-100 ,  \ qrstx
' f-500 , ' f-1000 ,     ' f+1 , ' offset- ,  \ uvwx
' f+100 ,  ' abort ,                          \ yz
ram

: s.display
  s-old @ s.get <> if s.get dup s-old ! s.meter type then
;
: app.display $rx-freq c@+ type .mode .bw $d emit ;
: term.display 
  space s.mode $rx-freq c@+ type 
  space ." KHz S:" s.display $d emit
;

: chan? ( c limit -- c f ) >r [char] 1 -  dup 0 r> within ;
: short-keys ( n -- )
  mode @ case
  modeFm  of fm-list#  chan? if fm-list  2@ newrxf 2! else drop then
          endof
  modeUsb of usb-list# chan? if usb-list 2@ newrxf 2! else drop then
          endof
  modeLsb of lsb-list# chan? if lsb-list 2@ newrxf 2! else drop then
          endof
  modeAm  of am-list#  chan? if am-list  2@ newrxf 2! else drop then
          endof
  endcase
;

: rx
  \clsh idle
  fl- >uart true
  begin
    if   rx-set vol-set rx-freq>$
         term? if term.display else app.display then
    else term? ticks $ff and 0= and if term.display then
    then
    pause key? 
    if   true
         key dup [char] a - dup 0 #26 within
         if cells exec + @ex else drop then
         short-keys
    else false
    then
  again ;

: rx-init rx-reset rx ;
\ rx-init
\ ' rx-init is turnkey

