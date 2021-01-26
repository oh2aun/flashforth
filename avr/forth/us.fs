\ microseconds delay for Atmega
-us
marker -us

\ Opcode only to flash
: op: ( opcode -- ) flash create , ram does> @ i, ;

\ Atmega wdr instruction
$95a8 op: wdr,

\ clear watchdog
: cwd [ wdr, ] ; inlined 
 
\ Clear watchdog (wdr instruction) takes one clock cycle
\ Adjust the number of CWD to achieve a one us delay
\ 9 CWD is needed @ 16MHz for ATmega 328 and 2560.
: us ( u -- ) \ busy wait for u microseconds
  begin
    cwd cwd  cwd cwd cwd cwd cwd cwd cwd
    1- dup 
  0= until
  drop 
;

\ Helper word for calibrating the us loop
-us-cal
marker -us-cal
: us-cal ( u -- ) \ give target delay in ms
  ticks >r
  for #1000 us next 
  ticks r> - #1000 um* 
  cr d. ."  microseconds"
;

decimal
1000 us-cal

