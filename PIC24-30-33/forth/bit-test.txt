\ *********************************************************************
\                                                                     *
\    Filename:      bit-test.txt                                      *
\    Date:          08.04.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC24-30-33                                       *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Test words for manipulating bits in ram
\ Needs bit.txt
-bit-test
marker -bit-test

$032c constant adpcfg
$3f adpcfg c!         \ Define pins to digital

$02c8 constant trisb
$02ca constant portb
$0009 constant bit9

trisb bit9 bit0: pb9out inlined
trisb bit9 bit1: pb9in  inlined
portb bit9 bit1: pb9on  inlined
portb bit9 bit0: pb9off inlined
portb bit9 bit?: pb9?   inlined


ram variable bitstat
$0002 constant bit2

bitstat bit2 bit1: bitstat2on  inlined
bitstat bit2 bit0: bitstat2off inlined
bitstat bit2 bit?: bitstat2?   inlined

