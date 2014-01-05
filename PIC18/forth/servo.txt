\ *********************************************************************
\    Servo controller for FlashForth                                  *
\    Filename:      servo.txt                                         *
\    Date:          05.01.2014                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ This interrupt example is a servo control routine
\ It use timer0 to generate servo pulses for 4 servos
\ on portb pins 0..3
\ It is not written for speed but for clarity(?) and
\ to prove that Forth interrupt routines really works.
\

tmr0ie intcon mclr  \ disable the tmr0 irq to prevent crash
0 0 int!            \ zero the interrupt vector
-servo
marker -servo
hex ram

\ The needed PIC registers
$ff8a constant latb      
$ff93 constant trisb     
$ffd5 constant t0con     
$ffd6 constant tmr0l
$fff2 constant intcon

\ intcon bits
$0020 constant tmr0ie
$0004 constant tmr0if

\ Pulse lengths in timerclocks for 4 channels
: array create cells allot does> swap 2* + ; 
ram 8 array timeout

\ Current servo id
ram variable segment    \ The current timeout segment
ram variable sbit       \ The current servo bit

Fcy #1000 /          constant clk/us
0 #1500 clk/us *  -  constant midhi
0 #500 clk/us * -    constant midlo
0 #12000 clk/us * -  constant endlo
0 #2000 clk/us * -   constant stime

\ Servo control interrupt routine
: sirq
  [i
  tmr0if intcon mtst      \ Clear the interrupt flag
  if
    segment @ timeout @ 
    tmr0l !               \ Restart timer
    tmr0if intcon mclr    \ clear the timer 0 interrupt flag
    sbit @                \ get servo bit
    segment @ 1 and       \ Check for hi or lo segment
    if 
      dup latb mclr       \ reset servo output
      2* sbit !           \ Move to next servo output
    else 
      latb mset           \ set servo output 
    then
    segment @ 1+ segment !
    segment @ 7 > if 0 segment ! 1 sbit ! endlo tmr0l ! then
  then
  i]
;i


\ Initialise the servo controller
: sinit
  $0f trisb mclr             \ PORTB 0..3 set to output
  0 segment !                \ Set current segment to 0
  1 sbit !                   \ Set output mask to bit 0
  4 for 
      midhi r@ 2* timeout ! 
      midlo r@ 2* 1+ timeout ! 
  next
  %10001000 t0con c!         \ TMR0 16 bit, no prescaler
  ['] sirq 0 int!            \ Store the interrupt vector
  tmr0if intcon mclr         \ clear the timer 0 interrupt flag
  tmr0ie intcon mset         \ set the timer 0 interrupt enable
;


\ Set a new servo position
: spos ( servo pos -- )  \ pos +499/0/-499 us, servo: 0 1 2 3
  2dup abs #499 u> swap 3 u> or if 2drop exit then
  clk/us * midhi + dup      \ servo hitime hitime
  stime - 0 swap -          \ servo hitime lotime
  >r over 2* timeout di ! ei
  r> swap 2* timeout cell+ di ! ei
;
