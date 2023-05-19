-ad9833
marker -ad9833
\ Assembler
$e2 constant cc,    \ bc
$ffed constant Sminus
$ffef constant Srw
$ffe1 constant Tp
$34 as3 rlcf,           ( f d a -- )  
$0e00 as1 movlw,        ( k -- )  
$6e as2 movwf,          ( f a -- )    
$04 as3 decf,           ( f d a -- )  
$70 as3 btg,            ( f b a -- )

\ Register definitions
$ff8a constant latb
$ff8b constant latc
$ff93 constant trisb
$ff94 constant trisc
$ffab constant rcsta1

6 constant vcocs     \ latc.6
2 constant sdata     \ latb.2
3 constant sclk      \ latb.3

\ The ad9834 control register
\ DAC enabled
\ Always update 28 bits of data at a time.
\ fedcba9876543210
%0010.0000.0000.1000 constant f0p0
%0010.1100.0000.1000 constant f1p1
%0010.0001.0000.1000 constant reset
%0010.0000.1100.1000 constant sleep

$3fff constant fsel
$4000 constant fsel0
$8000 constant fsel1
$0fff constant psel
$c000 constant psel0
$e000 constant psel1

: dds-ports-init ( -- )
  \ UART off
  0 rcsta1 c!
  %0000.1100 trisb mclr
  %0100.0000 trisc mclr
;

: spi! ( u -- )
  [ #2 movlw, ]
  [ Tp a, movwf, ]
  [ begin, ]
    [ #8 movlw, ]
    [ Tp 1+ a, movwf, ]
    [ begin, ]
      [ latb sclk a, bsf, ]
      [ Srw f, a, rlcf, ]
      [ cc, if, latb sdata a, bsf, else, latb sdata a, bcf, then, ]
      [ latb sclk a, bcf, ]
    [ Tp 1+ f, a, decf,  ]
    [ z, until, ]
    [ Sminus w, a, movf, ]
  [ Tp f, a, decf,  ]
  [ z, until,  ]
  [ latb sclk a, bsf, ]
;


: vco-dds! ( u -- )
  [ latb sclk a, bsf, ] \ clock high before chip select
  [ latc vcocs a, bcf, ]
  spi!
  [ latc vcocs a, bsf, ] ;
;

: vco-phase ( u psel -- )
  swap psel and swap or vco-dds!
;

: vco! ( ud -- )
  swap dup
  fsel and fsel0 or vco-dds!
  swap d2* d2*
  fsel and fsel0 or vco-dds!
  drop
;
#25.000.000. 2constant ad9833ref
: vco.f ( d -- )
  $1000.0000.  uq* ad9833ref 2@ uq/mod
  vco! f0p0 vco-dds! 2drop ;
  

: ad9833.init ( -- )
  dds-ports-init
  reset vco-dds!
  0 psel0 vco-phase
  0 psel1 vco-phase
  f0p0 vco-dds!
;

