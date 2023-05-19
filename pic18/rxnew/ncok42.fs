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

: nco! ( d -- )
  nco1incu c!
  nco1incl !
;

: nco/
  pps+
  $26 ra0pps c!
  pps-
  01 trisa mclr
  0 ansela c!
  $80 nco1con c!
;

eeprom
#15.999.300. 2value xtal
ram
: nco.f ( d -- )
  $20.0000. uq* xtal uq/mod nco! 2drop 
;



