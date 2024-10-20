-nco
marker -nco
$ff3f constant nco1clk
$ff3e constant nco1con
$ff3d constant nco1incu
$ff3c constant nco1inch
$ff3b constant nco1incl
$fa00 constant ra0pps
$fa40 constant ansela
$ffc2 constant trisa

eeprom
#22.272.410. 2value xtal_ee
ram
variable xtalOffset

: xtal xtal_ee xtalOffset @ m+ ;

: nco! ( d -- )
  nco1incu c!
  nco1incl !
;

: nco@ ( -- d )
  nco1incl @
  nco1incu c@
;

: nco/
  0 xtalOffset !
  pps+
  $26 ra0pps c!
  pps-
  01 trisa mclr
  0 ansela c!
  $80 nco1con c!
;

: uq/ uq/mod rot drop rot drop ;

: nco.value ( d -- d ) $20.0000. uq* xtal uq/ ;

: nco.f ( d -- ) nco.value nco! ;

: nco.real ( -- d )  nco@ xtal uq* $20.0000. uq/ ;

