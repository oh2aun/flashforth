\ *********************************************************************
\                                                                     *
\    Filename:      bit-test.txt                                      *
\    Date:          30.03.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Test words for manipulating bits in Access Ram and in Banked ram
\ Needs bit.txt
-bit-test
marker -bit-test

$ff82 constant portc
$ff94 constant trisc
$0000 constant bit0

trisc bit0 bit0: pc0out inlined
trisc bit0 bit1: pc0in  inlined
portc bit0 bit1: pc0on  inlined
portc bit0 bit0: pc0off inlined
portc bit0 bit?: pc0?


$f400 constant bitstat
$0002 constant bit2

bitstat bit2 bit1: bitstat2on  inlined
bitstat bit2 bit0: bitstat2off inlined
bitstat bit2 bit?: bitstat2?  

