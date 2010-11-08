\ *******************************************************************
\                                                                   *
\    Filename:      task-test.fth                                   *
\    Date:          10.10.2010                                      *
\    FF Version:    30F4.7                                          *
\    MCU:           dsPIC30F                                        *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
single
-t1
marker -t1
ram hex
\ Registers for dsPIC33FJ128GP802. Change if needed.
$032c con adpcfg
$02cc con latb
$02ca con portb
$02c8 con trisb
ram variable delay
: led4off [   #9 portb bclr, ] ;
: led4on  [   #9 portb bset, ] ;
: led5off [   #10 portb bclr, ] ;
: led5on  [   #10 portb bset, ] ;

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
        led4off
        led5on
    [ else,           ] 
        led4on
        led5off
    [ then,           ]
  again
;

: t1go ['] tloop t1 tinit t1 run ;

' t1go is turnkey
warm