-nco
marker -nco
$ff3f constant nco1clk
$ff3e constant nco1con
$ff3d constant nco1incu
$ff3c constant nco1inch
$ff3b constant nco1incl
$fa12 constant rc2pps
$fa60 constant anselc
$ffc4 constant trisc

: nco! ( d -- )
  nco1incu c!
  nco1incl !
;

: nco/
  pps+
  $26 rc2pps c!
  pps-
  4 trisc mclr
  0 anselc c!
  $80 nco1con c!
;



