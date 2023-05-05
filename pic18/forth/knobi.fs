\ Three incremental encoders on PIC18F26K42
\ Interrupts on pin rising edge 
-knobi
marker -knobi

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

\ access ram variables 18F26K42
$c04c constant iocc
$c04d constant knob1
$c04e constant knob2
$c04f constant knob3

$04 as3 decf,           ( f d a -- )  
$28 as3 incf,           ( f d a -- )  

: banksel 8 rshift  $3f and movlb, ;

\ Interrupt on pin change
: knobi
  [ pir0 banksel  pir0 iocif b, bcf, ]
  [ iocbf banksel ]
  [ ioccf w, b, movf,  iocc a, movwf, ]
  [ ioccf b, clrf, ]
  [ iocbf w, b, movf, ]
  [ iocbf b, clrf, ]
  [ $20 andlw,  nz, if, ]
  [   portb w, a, movf, $10 andlw,  nz, if, ]
  [     knob3 f, a, incf, ]
  [   else, ]
  [     knob3 f, a, decf, ]
  [   then, ]
  [ then, ]   
  [ iocc w, a, movf, 8 andlw, nz, if, ]
  [   portc w, a, movf, 4 andlw, nz, if, ]
  [     knob1 f, a, decf, ]
  [   else, ]
  [     knob1 f, a, incf, ]
  [   then, ]
  [ then, ]
  [ iocc w, b, movf, 2 andlw, nz, if, ]
  [   portc w, a, movf, 1 andlw, nz, if, ]
  [     knob2 f, a, decf, ]
  [   else, ]
  [     knob2 f, a, incf, ]
  [   then, ]
  [ then, ]
 ;i

: knob-init
  0 knob1 c!
  0 knob2 c!
  0 knob3 c!
  %1111 trisc mset
  %1111 anselc mclr
  %0000 ioccf c!
  %1010 ioccp c! \ Interrupt on rising edge
  %0000 ioccn c!
  %110000 trisb mset
  %110000 anselb mclr
  %00.0000 iocbf c!
  %10.0000 iocbp c! \ Interrupt on rising edge
  %00.0000 iocbn c!
  
  ['] knobi 0 int!
  [ pie0 banksel pie0 iocif b, bsf, ]
;

: knobs
  knob-init
  begin
    pause
    knob1 c@ [ knob1 a, clrf, ] dup if . else drop then
    knob2 c@ [ knob2 a, clrf, ] dup if . else drop then
    knob3 c@ [ knob3 a, clrf, ] dup if . else drop then
  key? until
;

