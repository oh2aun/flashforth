\ *********************************************************************
\    Interrupts example for FlashForth                                *
\    Filename:      int14k50.txt                                      *
\    Date:          01.04.2020                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18F14K50                                       *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ NOTE: Always deactivate user interrupts before
\ the interrupt word is removed.
\ You must also clear any related interrupt enable bits 
\ and interrupt flags before zeroing the interrupt vector
\ NOTE: User defined interrupts must always be defined as high priority interrupts.
\ NOTE: Interrupt vector 0 is the high prority interrupt vector.
\ NOTE: UART and tick timer interrupts are always low priority interrupts.

ccp1.int.off
-user.int
marker -user.int
decimal
$ff94 constant trisc
$ff8b constant latc
$ffb1 constant t3con
$ffbd constant ccp1con inlined
$ffbe constant ccpr1 inlined
$ff9e constant pir1 inlined
$ff9d constant pie1
$0004 constant ccp1if  inlined
$0004 constant ccp1ie

ram variable ccp1/edge inlined
ram variable width.clocks inlined
ram variable ccp1.int.cnt inlined

: ccp1.init
  0 ccp1con c!         \ reset capture module
  5 ccp1con c!         \ capture rising edge
  %10001001 t3con c!   \ timer3 as capture counter prescaler=8
  %00100000 trisc mset \ ccp1 input port configured as input                     
;

: ccp1.int
  [i
  ccp1if pir1 mtst if
    ccp1if pir1 inline mclr
    ccp1con c@ 5 = if
      4 ccp1con c!
      ccpr1 @ ccp1/edge !
    else
      ccpr1 @ ccp1/edge @ - width.clocks !
      5 ccp1con c!
    then
    1 ccp1.int.cnt +!
  then
  i]
;i

: pulsewidth 
  width.clocks @ #1000 Fcy u*/mod nip u. ." us"
;

: ccp1.int.init
  0 ccp1.int.cnt !
  ccp1.init
  ['] ccp1.int 0 int!
  ccp1ie pie1 mset
; 

: ccp1.int.off
  0 0 int!
  ccp1ie pie1 mclr
;

\ TESTS
\ tests require that ccp1 input port is configured as output

: test  %00100000 trisc mclr 
        10 ms [ latc 5 a, bsf, ] 
        for next 
        [ latc 5 a, bcf, ] pulsewidth ;
: test0 %00100000 trisc mclr 
        10 ms $20 latc mset       $20 latc mclr pulsewidth ;
: test1 %00100000 trisc mclr 
        10 ms $20 latc mset 1 ms  $20 latc mclr pulsewidth ;
: test2 %00100000 trisc mclr 
        10 ms $20 latc mset 2 ms  $20 latc mclr pulsewidth ;
: test3 %00100000 trisc mclr 
        10 ms $20 latc mset 3 ms  $20 latc mclr pulsewidth ;
: test4 %00100000 trisc mclr 10 ms
        [ latc 5 a, bsf, 0 i, 0 i, 0 i, latc 5 a, bcf, ] pulsewidth ;

ccp1.int.init
test0
test1
test2
test3
test4
10 test
ccp1.int.cnt @ u.
