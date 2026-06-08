\ needs task.fs
\ 01 Vdd +5V
\ 02 ra5 xtal
\ 03 ra4 xtal
\ 04 mclr
\ 05 rc5 pwm
\ 06 rc4 load
\ 07 rc3 solar
\ 08 rc6 AN8 for NTC1
\ 09 rc7 ad.i
\ 10 rb7 uart tx
\ 11 rb6 ??
\ 12 rb5 uart rx AN11 for NTC2
\ 13 rb4 ad.pb
\ 14 rc2 dac
\ 15 rc1 ad.li
\ 16 rc0 Vref+
\ 17 Vusb
\ 18 D- usb
\ 19 D+ usb
\ 20 Vss GND

single busy
fl+
-bms
marker -bms

$ffba constant refcon0
$ffbb constant refcon1
$ffbc constant refcon2
$ffc2 constant adcon0
$ffc1 constant adcon1
$ffc0 constant adcon2
$ffc3 constant adres
$ff7e constant ansel
$ff7f constant anselh
$ff8b constant latc
$ff94 constant trisc
$ffcc constant tmr2
$ffca constant t2con
$ffbd constant ccp1con
$ffb7 constant pwm1con
$ffb9 constant pstrcon
$ffb6 constant eccp1as
$ffbe constant ccpr1l
$ff9e constant pir1
$ffab constant rcsta
$2    constant tmr2if

decimal
: pwm! ( n -- ) ccpr1l c! ;
: pwm@ ccpr1l c@ ;
: charge-- pwm@ 1- 0 max pwm! ;
: charge++ pwm@ 1+ #255 min #0 max pwm! ;
: charge.on #255 pwm! ;
: charge.off #0 pwm! ;
: load.off $10 latc mclr ; \ Consumers on/off
: load.on $10 latc mset ;
: load.? $10 latc mtst ;
: solar.off $08 latc mclr ; \ Solar panel on/off
: solar.on $08 latc mset ;
: solar.? $08 latc mtst ;

: pwm/
  $4 t2con c!
  0 pir1 c!
  %0000.00.01 pstrcon c!
  %0.000.00.01 eccp1as c!
  $20 pwm1con c!
  charge.off
  begin cwd pir1 c@ tmr2if and until
  %00.00.1100 ccp1con c!
  $38 trisc mclr          \ PWM and LOAD and SOLAR outputs rc3,rc4,rc5
;
: ad.pb    \ 4V AN10 RB4
  %1011.0000 refcon0 c! %0000.0100 refcon2 c! %1010.01 adcon0 c! ;
: ad.li    \ 4V AN5 RC1
  %1011.0000 refcon0 c! %0000.0100 refcon2 c! %0101.01 adcon0 c! ;
: ad.i     \ 2V AN9 RC7
  %1010.0000 refcon0 c! %0000.1000 refcon2 c! %1001.01 adcon0 c! ;
: ad.t1    \ 5V AN8 RC6
  %1000.01 adcon0 c! [ adcon1 3 a, bcf, ] ;
: ad.t2    \ 5V AN11 RB5
  %1011.01 adcon0 c! [ adcon1 3 a, bcf, ] ;

: ad/
  %1011.0000 refcon0 c!   \ on 4.096 V
  %0000.0100 refcon2 c!   \ DAC output level
  %1110.1000 refcon1 c!   \ output DAC on RC2 AN6 P14
  %10.111.111 adcon2 c!   \ Right justify, 20 Aq time, Frc
  %0000.1000 adcon1 c!    \ ADC ref FVR, Vss
  %0010.0000 ansel  c!    \ RC1AN5 analog
  %0000.1111 anselh c!    \ AN8RC6 AN9RC7 AN10RB4 AN11RB5 analog
  ad.pb
; 

: ad.go [ adcon0 1 a, bsf, begin, adcon0 1 a, btfsc, again, ] ;
: ad@  ( -- +n ) 4 ms ad.go adres @ ;
: ad.10 0 #10 for ad@ + next ;
: ad.50 0 #50 for ad@ + next ;
eeprom 
#5005 value i.zero
#25566 value i.scale
#619 value u.pb.div
#619 value u.li.div
ram

: u.pb ad.pb ad.10 #100 u.pb.div u*/mod nip ;
: u.li ad.li ad.10 #100 u.li.div u*/mod nip ;
: i.li ad.i  ad.50 #10000 i.scale u*/mod nip ;
: t.1  ad.t1 ad@ ad>temp [ adcon1 3 a, bsf, ] ; 
: t.2  
  [ rcsta 7 a, bcf, ] ad.t2 ad@ ad>temp
  [ adcon1 3 a, bsf, rcsta 7 a, bsf, ] ;

ram 
0 value u.charge
0 value u.bat
0 value i.charge
#25 value T1
#25 value T2
2variable i.time
variable i.ticks
2variable i.samples
2variable i.acc

variable index
create i.li[] #16 cells allot
: i.li! i.li[] index @ $1e and + ! cell index +! ;
: i.li@ 0. #16 for i.li[] r@ cells + @ m+ next #16 um/mod nip i.zero - ;

0 value phase
0 constant nocharge
1 constant charging
2 constant absorbing

eeprom
#13.30 value u.l.charge
#14.00 value u.l.absorb
#04.00 value i.l.idle
#12.30 value u.disconnect
#12.50 value u.connect
#13.34 value s.connect
#13.50 value s.disconnect
#100.00 value li.capacity
#40 value t-limit
ram

: update 
  u.pb to u.charge 
  u.li to u.bat
  i.li i.li!
  i.li@ to i.charge
  t.1 to T1
  t.2 to T2
;
: refresh #16 for update next ;

\ True if valid temperature. 99 is a not valid temperature 
: temp-valid ( -- flag ) T1 99 = T2 99 = or 0= ;

\ true if temperature above limit
: temp-hi? ( -- flag )
  temp-valid if T1 t-limit > T2 t-limit > or else false then ;
\ true if temperature below reconnect temperature
: temp-co? ( -- flag )
  temp-valid if T1 t-limit 2 - < T2 t-limit 2 - < and else true then ;
\ true if temperature is below 4C
: temp-lo? ( -- flag )
  temp-valid if T1 4 < T2 4 < or else false then ;

ram variable l.ticks
: load.off?
  u.bat u.disconnect < if 1 l.ticks +! then
  l.ticks @ 20 >  load.? and  if load.off then
;

: load.on?
  u.bat u.connect >  load.? 0=  and  if load.on 0 l.ticks ! then
;

: .V 0 <# # # [char] . hold # # #> type ." V" ;
: sign+- 0< if [char] - else [char] + then hold ;
: .A 
  <# dup abs 0 # # [char] . hold # # rot sign+- #> type 
  [char] A emit ;
  
: .C
  dup 99 <> if
    <# dup abs [char] C hold 0 # # rot sign+- #>
  else drop s" ***C"
  then type
;
: lcd-c $fe tx1 ;
: lcd-bl ( n -- ) $7c tx1 $80 + tx1 ; \ 0 - 29
: lcd-at ( n -- ) lcd-c $80 + tx1 ;
: lcd/ lcd-c 8 tx1 lcd-c $c tx1 lcd-c 1 tx1 0 lcd-bl ;

: i.charged ( -- n ) \ Charge level in centiAh for 100 Ah battery
  i.acc 2@ d0< ( sign )
  i.acc 2@ dabs i.time 2@ #1000 ud/mod rot drop
  uq* i.samples 2@ uq/mod >r nip nip r>
  #3600 um/mod >r drop 
  ( sign ) if li.capacity r> - else li.capacity r> + then 
;
: .phase
   phase nocharge  = if ." Idle  " then
   phase charging  = if ." Bulk  " then
   phase absorbing = if ." Absorb" then
   space T1 .C space T2 .C
   space pwm@ 3 u.r 
;
: .li ." HOUSE:" u.bat .V space i.charge .A ;
: .pb ." START:" u.charge .V ;
: .level 
  ." LEVEL:
    i.charged <# dup abs false # # [char] . hold # # # rot sign+- #> type 
    ." Ah"
;
: .load load.? if s" L1" else s" L0" then type ;
: .solar solar.? if s" S1" else s" S0" then type ;
: i.accumulate
  i.time 2@ ticks i.ticks @ - m+ i.time 2!   ticks i.ticks !
  i.acc 2@ i.charge m+ i.acc 2!   i.samples 2@ 1 m+ i.samples 2!
;
: sm
    temp-hi? if nocharge to phase then
    nocharge phase = if
      charge.off
      u.bat u.l.charge < u.charge #13.30 > and temp-co? and temp-lo? 0= and
      if  charging to phase charge.on refresh then
    then
    charging phase = if
      u.bat u.l.absorb > 
      if  absorbing to phase #64 pwm! then
    then
    absorbing phase = if
      u.bat u.l.absorb >
      if   charge-- 
      else charge++
      then
      pwm@ #74 umin pwm!
      update
      i.charge i.l.idle < if
        nocharge to phase charge.off  
        ticks i.ticks ! 0. i.time 2! 0. i.acc 2! 0. i.samples 2!
      then
    then
    
    i.charge i.l.idle <   u.charge #10 + u.bat <   or
    if  nocharge to phase charge.off  then
    i.charge abs #00.05 > if \ Ignore zero error
      i.accumulate
    then
    load.off?
    load.on?
    u.bat s.connect < temp-co? and temp-lo? 0= and if solar.on then
    u.bat s.disconnect > temp-hi? or temp-lo? or if solar.off then
;
: bms
  ['] tx1 'emit ! ['] rx1 'key ! ['] rx1? 'key !
  ad/ pwm/ lcd/ decimal
  nocharge to phase refresh
  ticks i.ticks ! 0. i.time 2! 0. i.acc 2! 1. i.samples 2!
  begin
    update
    busy
    #00 lcd-at .phase 
    #20 lcd-at .li
    #64 lcd-at .pb
    #84 lcd-at .level
    space .load .solar
    idle
    sm
  again ;

: display
  decimal
  begin
    #1000 ms
    cr .phase
    cr .li cr .pb
    cr .load .solar cr i.charged .
    key?
  until key drop ;

0 $20 $20 0 task: bms-task
: bms/ fl- ['] bms bms-task tinit bms-task run ;
\ ' bms/ to turnkey
warm

