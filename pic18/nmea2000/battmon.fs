\ Battery monitor NMEA2000.
\ PB starter battery and LFP house battery
\ PIC18F26K83+ MCP2562+MCP609+serial converter
\ Internal clock 2 MHz
\ 
\ include task.txt
busy single
64MHz
( increase likelyhood of uart sync. )
-can
marker -can
hex
fdf5 constant u1brg
f9df constant oscfrq
: 64MHz 8 oscfrq c! #1665 u1brg ! ;
: 2MHz  1 oscfrq c! #51 u1brg ! ;
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
f7eb constant  txb0d5
f7e9 constant  txb0d3
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

fefb constant adcon3
fefa constant adcon2
fef9 constant adcon1
fef8 constant adcon0
fefd constant adref
fef1 constant adpch
feef constant adres
fec1 constant fvrcon

fe9e constant dac1con0
fe9c constant dac1con1

fa50 constant anselb
ffc3 constant trisb
ffbb constant latb

faed constant canrxpps \ rb0 can rx  8 canrxpps c!
fa0b constant rb3pps   \ rb3 can tx0 $33 rb3pps c!

eeprom 77 value nmea.src   \ NMEA2000 source address



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
  dup 8 rshift 3 and over 5 rshift e0 and or 8 or txb0sidl c! 
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

: nmea.tx.bst ( temperature current volts instance -- )
  #127508. nmea.src nmea.tx.init
  txb0d0 c!                 \ instance
  txb0d1  !                 \ voltage
  txb0d3  !                 \ current
  #27315 + txb0d5 !         \ temperature
  1 txb0d7 c!               \ SID
  nmea.tx.sp
;
: nmea.tx.rpm ( rpm -- )
  #127488. nmea.src nmea.tx.init 
  0 txb0d0 c!               \ instance
  txb0d1  !                 \ rpm
  0. txb0d3 2!
  nmea.tx.sp
;

: ad@
  0 #16 for
     [ $3e movlb, adcon0 0 1 bsf, ]
     [ begin, adcon0 0 1 btfsc, again, ]
     adres @ +
  next
;
: u.ref2v [ $3e movlb, fvrcon 0 1 bcf, ] ;
: u.ref4v [ $3e movlb, fvrcon 0 1 bsf, ] ;
: temp@ ( -- n )
  u.ref2v [ #60 adpch C! ]
  0 #32 for
     [ $3e movlb, adcon0 0 1 bsf, ]
     [ begin, adcon0 0 1 btfsc, again, ]
    adres @ #2048 -  +
  next
  #5 rshift #500 um* #34 um/mod nip negate #10300 + 
;
: u.ref@ ( -- n ) u.ref2v [ 3 adpch C! ] ad@ ;
: u.pb@  ( -- n ) u.ref4v [ 1 adpch C! ] ad@ #8 #306 u*/mod nip ;
: u.lfp@ ( -- n ) u.ref4v [ 4 adpch C! ] ad@ #8 #309 u*/mod nip ;
: i.lfp@ ( -- n ) u.ref2v [ 0 adpch C! ] ad@ #32350 - 16 / ;

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
: ad/
  #60 adpch c!
  $fa fvrcon c!         \ 2048 mV output on both references
  $a8 dac1con0 c!       \ DAC1 output on RA2
  $10 dac1con1 c!       \ 16=1025 mV
  $03 adref c!
  $94 adcon0 c!
;

: can.drop.rx  $80 rxb0con mtst if $20 rxb0con c! then ;
: can.bus?     $32 comstat mtst if can.sleep then ;
: can.main
  ad/ can/
  begin
    can.bus?
    can.drop.rx
    #1000 ms
    temp@ i.lfp@ u.lfp@ 0 nmea.tx.bst
    #1000 ms
    temp@ 0 u.pb@ 1 nmea.tx.bst
    #4567 nmea.tx.rpm
  again
;
ram 0 $20 $20 0 task: can.task
: main.run
  pmd/
  ['] can.main can.task tinit can.task run
;
main.run

