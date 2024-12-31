\ *********************************************************************
\    Filename:      irqtest.txt                                       *
\    Date:          20.01.2020                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18F26K42                                       *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-irqtest
marker -irqtest

$f990 constant pie0
$f9a0 constant pir0

\ Test multiplication and division in interrupts.
: irqtest ( -- )
  [i
    1 pir0 mclr
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
    pause 1 pir0 mset
    #10000 #20000 um* 2dup #200000000. d= abort" err1"
    #40000 um/mod #5000 = abort" err2" drop
    key?
  until
  key drop
;
