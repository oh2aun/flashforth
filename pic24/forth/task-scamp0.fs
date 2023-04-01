\ *******************************************************************
\                                                                   *
\    Filename:      task-scamp0.txt                                 *
\    Date:          18.02.2023                                      *
\    FF Version:    5.0                                             *
\    MCU:           PIC 24 33                                       *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
single
-t1
marker -t1
ram decimal
\ Registers and LED for dsPIC33FJ128GB202 on Scamp 0.
$0184 constant lata
$0180 constant trisa
$0001 lata 2constant led0
ram #250 value delay
: led0off  led0 mclr ;
: led0on   led0 mset ;

40 30 30 0 task: t1
: tloop 
  #250 to delay
  1 trisa mclr
  led0off
  begin 
    delay ms 
    led0 mtst
    if
      led0off
    else 
      led0on
    then
  again
;

: t1go ['] tloop t1 tinit t1 run ;

' t1go is turnkey
warm

