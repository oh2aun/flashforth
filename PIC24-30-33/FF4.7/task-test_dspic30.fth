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
$02a8 con adpcfg
$02cb con latb
$02c8 con portb
$02c6 con trisb
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
hex ram
warm
