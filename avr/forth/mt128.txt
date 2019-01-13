-mt128
marker -mt128

$34  constant ddrc
$35  constant portc

2 constant pin-e
1 constant pin-rw
0 constant pin-rs

$01 constant l.clr
$02 constant l.home
$04 constant l.<
$06 constant l.>
$08 constant l.off
$0c constant l.on
$0e constant l.under
$0f constant l.blink
$80 constant l.line1
$c0 constant l.line2

: >io $20 - ;
: tosl #24 ;
: 1ms 1 ms ;

: l.rs.0 [ ddrc >io pin-rs sbi, portc >io pin-rs cbi, ] ;
: l.rs.1 [ ddrc >io pin-rs sbi, portc >io pin-rs sbi, ] ;
: l.e.0 [ ddrc >io pin-e sbi, portc >io pin-e cbi, ] ;
: l.e.1 [ ddrc >io pin-e sbi, portc >io pin-e sbi, ] ;
: l.rw.0 [ ddrc >io pin-rw sbi, portc >io pin-rw cbi, ] ;
: l.rw.1 [ ddrc >io pin-rw sbi, portc >io pin-rw sbi, ] ;

: l.port/
  [ #16 #0 ldi, ]
  [ portc >io #16 out, ]
  [ #16 %1111.0111 ldi, ]
  [ ddrc >io #16 out, ]
;

: l.e 1 us l.e.1 1 us l.e.0 ;

: l.cmd.hi ( c -- ) [ tosl $f0 andi, portc >io tosl out, ] l.e drop ;
: l.cmd.lo ( c -- ) [ tosl swap, ] l.cmd.hi ;

: l.put.hi ( c -- )
  [ tosl $f0 andi, portc >io tosl out, ] l.rs.1 
  l.e drop ;
: l.put.lo [ tosl swap, ] l.put.hi l.rs.0 ;

: l.cmd  ( c -- ) 40 us dup l.cmd.hi l.cmd.lo ;
: l.emit ( c -- ) 40 us dup l.put.hi l.put.lo ;

: l.type ( a n -- ) for c@+ l.emit next drop ;

: l/
  l.port/
  #15 ms  %0011.0000 portc c! l.e 5 ms l.e 1ms l.e 
  #05 ms  %0010.0000 portc c! l.e \ 4 bit mode
  1ms %0010.1000 l.cmd \ 2 lines
  1ms %0000.1000 l.cmd \ Display, cursor blink Off
  1ms %0000.0001 l.cmd \ Clear screen
  1ms %0000.0110 l.cmd \ Cursor to right, don't shift screen
;

: l.app l/ l.blink l.cmd ;

' l.app to turnkey


: app2
  l/ l.on l.cmd false
  begin
   #199 ms
   decimal l.line1 l.cmd
   dup false <# # # # # # #> l.type
   hex l.line2 l.cmd
   dup false <# # # # # # #> l.type
   1+
  again ;
