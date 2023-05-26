\ Three incremental encoders on PIC18F26K42
\ Interrupts on pin rising edge 
-knobi
marker -knobi

$fa45 constant iocap
$fa46 constant iocan
$fa47 constant iocaf
$fa50 constant ansela
$ffc2 constant trisa
$ffca constant porta
$fa41 constant wpua

$fa55 constant iocbp
$fa56 constant iocbn
$fa57 constant iocbf
$fa50 constant anselb
$ffc3 constant trisb
$ffcb constant portb

$fa65 constant ioccp
$fa66 constant ioccn
$fa67 constant ioccf
$f9a0 constant pir0
$f990 constant pie0
    7 constant iocif
$ffc4 constant trisc
$ffcc constant portc

$fa85 constant iocep
$fa86 constant iocen
$fa87 constant iocef
$ffce constant porte
$ffb6 constant trise
$fa81 constant wpue

\ access ram variables 18F26K42
$c046 constant timestamp
$c048 constant switch1
$c049 constant switch2
$c04a constant ioca
$c04b constant iocc
$c04c constant ioce
$c04d constant knob1
$c04e constant knob2
$c04f constant knob3

$04 as3 decf,           ( f d a -- )  
$28 as3 incf,           ( f d a -- )
1 constant b,

: banksel 8 rshift  $3f and movlb, ;

\ Interrupt on pin change
: knobi
  [i
  [ pir0 banksel  pir0 iocif b, bcf, ]
  [ iocbf banksel ]
  [ ioccf w, b, movf,  iocc a, movwf, ]
  [ ioccf b, clrf, ]
  [ iocaf w, b, movf,  ioca a, movwf, ]
  [ iocaf b, clrf, ]
  [ iocef w, b, movf,  ioce a, movwf, ]
  [ iocef b, clrf, ]
  [ iocbf w, b, movf, ]
  [ iocbf b, clrf, ]
  [ $20 andlw,  nz, if, ]
      ticks timestamp @ - #80 < if
  [     portb w, a, movf, $10 andlw,  nz, if, ]
  [       knob3 f, a, incf, ]
  [     else, ]
  [       knob3 f, a, decf, ]
  [     then, ]
      then
      \ ticks timestamp !
  [   $10 w, a, movf,  $46 a, movwf, ]
  [   $11 w, a, movf,  $47 a, movwf, ]
  [ then, ]   
  
  [ iocc w, a, movf, 8 andlw, nz, if, ]
  [   portc w, a, movf, 4 andlw, nz, if, ]
  [     knob1 f, a, decf, ]
  [   else, ]
  [     knob1 f, a, incf, ]
  [   then, ]
  [ then, ]
  
  [ iocc w, a, movf, 2 andlw, nz, if, ]
  [   portc w, a, movf, 1 andlw, nz, if, ]
  [     knob2 f, a, incf, ]
  [   else, ]
  [     knob2 f, a, decf, ]
  [   then, ]
  [ then, ]
  
  [ ioca w, a, movf, $20 andlw, nz, if, ]
  [   switch2 f, a, incf, ]
  [ then, ]
  [ ioce w, a, movf, $08 andlw, nz, if, ]
  [   switch1 f, a, incf, ]
  [ then, ]
  i]
 ;i

: knob-init
  0 knob1 c!
  0 knob2 c!
  0 knob3 c!
  0 switch1 c!
  0 switch2 c!

  %1111 anselc mclr
  %0000 ioccf c!
  %1010 ioccp c! \ Interrupt on rising edge

  %11.0000 anselb mclr
  %00.0000 iocbf c!
  %10.0000 iocbp c! \ Interrupt on rising edge

  %0000 iocef c!
  %1000 iocep c! \ Interrupt on rising edge
  %1000 wpue c!
  
  %10.0000 ansela mclr
  %10.0000 wpua c!
  %00.0000 iocaf c!
  %10.0000 iocap c! \ Interrupt on rising edge
  %00.0000 iocan c!

  ['] knobi 0 int!
  [ pie0 banksel pie0 iocif b, bsf, ]
;

: knobs
  begin
    pause
    knob1 c@ [ knob1 a, clrf, ] dup if 1 . . cr else drop then
    knob2 c@ [ knob2 a, clrf, ] dup if 2 . . cr else drop then
    knob3 c@ [ knob3 a, clrf, ] dup if 3 . . cr else drop then
    switch1 c@ [ switch1 a, clrf, ] dup if 4 . . cr else drop then
    switch2 c@ [ switch2 a, clrf, ] dup if 5 . . cr else drop then
  key? until
;

