-nco
marker -nco
$ff3f constant nco1clk
$ff3e constant nco1con
$ff3d constant nco1incu
$ff3c constant nco1inch
$ff3b constant nco1incl
$fa01 constant ra1pps
$fa40 constant ansela
$ffc2 constant trisa

eeprom
#8.000.948. 4 ud* 2value xtal_pic
ram
variable xtalOffset

: nco! ( d -- )
  nco1incu c!
  nco1incl !
;

: nco@ ( -- d )
  nco1incl @
  nco1incu c@
;

: nco/
  %0111.1101 $f9c1 c! \ nco tmr1
  0 xtalOffset !
  pps+
  $26 ra1pps c!
  pps-
  %10 trisa mclr
  0 ansela c!
  $80 nco1con c!
  0 nco1clk c!
;

: nco.value ( d -- d ) $20.0000. uq* xtal_pic uq/ ;

: nco.f ( d -- ) nco.value nco! ;

: nco.real ( -- d )  nco@ xtal_pic uq* $20.0000. uq/ ;

