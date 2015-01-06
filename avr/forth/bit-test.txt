\ *********************************************************************
\                                                                     *
\    Filename:      bit-test.txt                                      *
\    Date:          06.01.2015                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega                                            *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Test words for manipulating bits in ram and in IO registers
\ Needs bit.txt
-bittest
marker -bittest

\ BIT addressable IO register
\ $20 - $3f
$22 constant porta

\ IN OUT addressable IO register
$4a constant gpior1

\ LDS STS addressable IO register
$124 constant tcnt5l

porta 2 bit0: porta2off
porta 2 bit1: porta2on
porta 2 bit?: porta2?

gpior1 0 bit0: gpio0off
gpior1 0 bit1: gpio0on
gpior1 0 bit?: gpio0?

tcnt5l 7 bit0: tcnt5l7off
tcnt5l 7 bit1: tcnt5l7on
tcnt5l 7 bit?: tcnt5l7?
 
-1 porta c!
porta2off porta c@ . porta2? .
porta2on porta c@ . porta2? .
0 porta c!
porta2on porta c@ . porta2? . 
porta2off porta c@ . porta2? .

-1 gpior1 c!
gpio0off gpior1 c@ . gpio0? .
gpio0on gpior1 c@ . gpio0? .
0 gpior1 c!
gpio0off gpior1 c@ . gpio0? .
gpio0on gpior1 c@ . gpio0? .
0 gpior1 c!
-1 tcnt5l c!
tcnt5l7off tcnt5l c@ . tcnt5l7? .
tcnt5l7on tcnt5l c@ . tcnt5l7? .
0 tcnt5l c!
tcnt5l7on tcnt5l c@ . tcnt5l7? .
tcnt5l7off tcnt5l c@ . tcnt5l7? .
