\ *********************************************************************
\    Filename:      irqtest.txt                                       *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-irqtest
marker -irqtest

\ Test multiplication and division in interrupts.
: irqtest ( -- )
  [i
    #10000 #10000 um* 2dup #100000000. d= 0= if warm then
    #20000 um/mod #5000 = 0= if warm then drop
  i]
;i

: irqtest+ ['] irqtest 0 int! ;
' irqtest+ to turnkey

irqtest+  \ Start the interrupt routine

\ Test interrupt capability of * and /
\ Test ends by pressing anykey
: */test ( -- )
  begin
    #10000 #20000 um* 2dup #200000000. d= abort" err1"
    #40000 um/mod #5000 = abort" err2" drop
    key?
  until
  key .
;
