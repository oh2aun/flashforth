\ *******************************************************************
\                                                                   *
\    Filename:      task-test.txt                                   *
\    Date:          25.11.2016                                      *
\    FF Version:    5.0                                             *
\    MCU:           PIC 24 33                                       *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ It seems a nop is required between two consecutive
\ read modify instructions on PORTB
single
-t1
marker -t1
ram hex
\ Registers for dsPIC33FJ128GP802. Change if needed.
$032c constant adpcfg
$02cc constant latb
$02ca constant portb
$02c8 constant trisb
ram variable delay
: led9off  [ #9 portb bclr,  ] ; inlined
: led9on   [ #9 portb bset,  ] ; inlined
: led10off [ #10 portb bclr, ] ; inlined
: led10on  [ #10 portb bset, ] ; inlined
: nop      [ flash 0 , ram ]   : inlined
40 30 30 0 task: t1
: tloop 
  $3f adpcfg c!
  100 delay !
  [ #9 trisb bclr, ] 
  [ #10 trisb bclr, ] 
  begin 
    delay @ ms 
    [ #10 portb btst,   ]
    [ z, if,          ]
        led9off
        nop
        led10on
    [ else,           ] 
        led9on
        nop
        led10off
    [ then,           ]
  again
;

: t1go ['] tloop t1 tinit t1 run ;

' t1go is turnkey
warm

