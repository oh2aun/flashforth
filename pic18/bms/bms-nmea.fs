\ BMS LCD display data to NMEA2000 conversion.
\ PB starter battery and LFP house battery
\ PIC18F26K83+ MCP2562+serial converter
\ RX2 receives 9600 baud LCD display formatted data
\ System clock 2 MHz
\ RX1 PC.7 TX1 PC.6 38400 baud
\ RX2 PB.7 TX2 PB.6 9600 baud
\ 
\ #sendm f/case f/forget 18f/see 18f/task pic18/bms/bms-nmea
fl+ busy decimal single
64MHz
( increase likelyhood of uart sync. )
-bms-nmea
marker -bms-nmea
hex
fdf5 constant u1brg
fddd constant u2brg
f9df constant oscfrq
: 64MHz 8 oscfrq c! #415 u1brg ! $681 u2brg ! ;
: 2MHz  1 oscfrq c! #0012 u1brg ! $033 u2brg ! ;
64MHz 
64MHz

$006e as2 movwf,        ( f a -- )
$0100 as1 movlb,        ( k -- )
$0e00 as1 movlw,        ( k -- )

ff91 constant  ecancon
ff90 constant  comstat
ff8f constant  cancon
ff8e constant  canstat

ff80 constant  rxb0con

f7ed constant  txb0d7
f7ec constant  txb0d6
f7eb constant  txb0d5
f7ea constant  txb0d4
f7e9 constant  txb0d3
f7e8 constant  txb0d2
f7e7 constant  txb0d1
f7e6 constant  txb0d0
f7e5 constant  txb0dlc
f7e4 constant  txb0eidl
f7e3 constant  txb0eidh
f7e2 constant  txb0sidl
f7e1 constant  txb0sidh
f7e0 constant  txb0con

f705 constant  brgcon3
f704 constant  brgcon2
f703 constant  brgcon1

fa50 constant anselb
ffc3 constant trisb
ffbb constant latb

faed constant canrxpps \ rb0 can rx  8 canrxpps c!
fa0b constant rb3pps   \ rb3 can tx0 $33 rb3pps c!

eeprom 77 value nmea.src   \ NMEA2000 source address
ram
decimal

: mcp2562.standby [ latb 4 0 bsf, ] ;
: mcp2562.normal  [ latb 4 0 bcf, ] ;
: can.sleep
  $30 cancon c!      \ CAN sleep mode, abort tx
  mcp2562.standby
  begin #100 ms $e0 canstat mtst 0= until \ BUS connected ?
  mcp2562.normal
  $80 cancon c!      \ Request configuration mode
  begin canstat c@ $80 and until
  $00 cancon c!
  begin $e0 canstat mtst 0= until
;
: nmea.pgn! ( d -- )
  \ eid 21-28 pgn 13-21
  3 lshift over #13 rshift or txb0sidh c!
  \ eid 16-17 exide eid 18-20 pgn 8-12
  dup 8 rshift 3 and over 5 rshift $e0 and or 8 or txb0sidl c! 
  \ eid 8-15 pgn 0-7
  txb0eidh c!
;

: nmea.src! ( c -- ) txb0eidl c! ;

: can.tx.done 
  begin
    8 txb0con mtst 0=  txb0con c@ $f >  or
  until
;

: can.tx [ $37 movlb, 8 movlw, txb0dlc 1 movwf, txb0con 3 1 bsf, ] ;

: nmea.tx.init ( pgn src -- ) nmea.src! nmea.pgn! ;
: nmea.tx.sp ( -- )          \ 8 byte short packet
  can.tx can.tx.done
;

\ DC detailed status
: nmea.tx.dcd ( stateOfCharge batteryinstance -- )
  #127506. nmea.src nmea.tx.init
  #0000 txb0d0 c!    \ Fast packet, sequence 0, frame 0
  #0011 txb0d1 c!    \ payload size
  1     txb0d2 c!    \ SID instance
        txb0d3 c!    \ battery instance
  0     txb0d4 c!    \ dcType battery
  dup   txb0d5 c!    \ StateOfCharge 0-100
  #100  txb0d6 c!    \ SoH
  0     txb0d7 c!    \ timeRemaining low byte
  nmea.tx.sp
  
  #127506. nmea.src nmea.tx.init
  #0001 txb0d0 c!    \ Fast packet, sequence 0, frame 1
  0     txb0d1 c!    \ time remaining high byte
  0     txb0d2 !     \ Ripple voltage
        txb0d4 !     \ Remaining capacity
  nmea.tx.sp
;

\ Charger status
: nmea.tx.cs ( operatingState batteryinstance instance -- )
  #127507. nmea.src nmea.tx.init
  txb0d0 c!        \ instance
  txb0d1 c!        \ battery instance
  txb0d2 c!        \ operatingState(NotCharging=0,Bulk=1,Absorption=2)
  1 txb0d3 c!      \ Operatingstate=On
  0 txb0d4 c!
  0 txb0d5 !
;
\ Battery status
: nmea.tx.bst ( temperature current volts instance -- )
  #127508. nmea.src nmea.tx.init
  txb0d0 c!                 \ instance
  txb0d1  !                 \ voltage
  txb0d3  !                 \ current
  #27315 + txb0d5 !         \ temperature
  1 txb0d7 c!               \ SID
  nmea.tx.sp
;
\ Engine RPM
: nmea.tx.rpm ( rpm -- )
  #127488. nmea.src nmea.tx.init 
  0 txb0d0 c!               \ instance
  txb0d1  !                 \ rpm
  0. txb0d3 2!
  nmea.tx.sp
;

ram
variable t1
variable t2
variable u.lfp
variable i.lfp
variable u.pb 
variable i.level

: init.bms
  20.00 t1 ! 21.00 t2 ! 13.31 u.lfp ! 19.2 i.lfp ! 
  12.69 u.pb ! 88 i.level !
;
: display
  cr ." t1   :" t1 @ .
  cr ." t2   :" t2 @ .
  cr ." u.lfp:" u.lfp @ .
  cr ." i.lfp:" i.lfp @ .
  cr ." u.pb :" u.pb @ .
  cr ." level:" i.level @ .
;
: >uart1
  ['] rx1 'key !
  ['] rx1? 'key? !
  ['] tx1 'emit !
;
: >uart2
  ['] rx2 'key !
  ['] rx2? 'key? !
  ['] drop 'emit !
;

\ Wait for the setcursor LCD command
: wait-xy ( -- xy )
  begin
    begin
      key $fe =
    until
    key dup $80 and
    if $7f and exit else drop then
  until
;
: >source ( buf len >in -- ) >r 'source 2! r> >in ! ;
: line    ( len >in -- ) >r >r tib dup r> accept r> >source ;
: >temp   #20 0 line 
          #7 >in !
          [char] C word number? if 100 * t1 ! else drop then
          #12 >in !
          [char] C word number? if 100 * t2 ! else drop then ;
          
: >ui.lfp #20 6 line
          [char] V word number? if u.lfp ! else drop then
          #13 >in !
          [char] A word number? if 10 / i.lfp ! else drop then ;

: >level  #20 6 line
          [char] A word number? if #100 / i.level ! else drop then ;

: >u.pb   #12 6 line
          [char] V word number? if u.pb ! else drop then  ;

: bms.task
  >uart2
  begin
    wait-xy case
    #00 of >temp  endof
    #20 of >ui.lfp endof
    #64 of >u.pb   endof
    #84 of >level  endof
    endcase
  again
;

\ 2 MHz internal FRC, 250 KHz CAN speed.
: can/
  2MHz
  pps+                  \ Enable pin mapping
  0 anselb c!           \ digital portb
  8 canrxpps c!         \ canrx rb0
  8 latb mset
  $18 trisb mclr        \ rb3 rb4 output
  $33 rb3pps c!         \ cantx rb3
  pps-                  \ Disable pin mapping
  mcp2562.normal 
  0 ecancon c!          \ CAN mode 0
  $80 cancon c!         \ Request configuration mode
  begin canstat c@ $80 and until
  0 brgcon1 c!
  %10.000.000 brgcon2 c!
  %00.000.000 brgcon3 c!
  $00 cancon c!          \ normal mode
  $20 rxb0con c!         \ RX ignore NMEA2000 messages
;

: can.drop.rx  $80 rxb0con mtst if $20 rxb0con c! then ;
: can.bus?     $32 comstat mtst if can.sleep then ;
eeprom 20 value tx.delay
ram
: can.task
  can/
  begin
    can.bus?
    can.drop.rx
    tx.delay ms
    t1 @ t2 @ max i.lfp @ u.lfp @ 0 nmea.tx.bst   
    tx.delay ms
    0 0 u.pb @ 1 nmea.tx.bst
    tx.delay ms

    i.level @ 0 nmea.tx.dcd
  again
;
$20 $20 $20 0 task: bms.tcb
$00 $20 $20 0 task: can.tcb
: main
  init.bms
  fl-
  2MHz
  ['] bms.task bms.tcb tinit bms.tcb run
  ['] can.task can.tcb tinit can.tcb run
;
\ main

