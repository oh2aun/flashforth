\ *********************************************************************
\    Blinker for FlashForth                                           *
\    Filename:      blink.txt                                         *
\    Date:          06.01.2014                                        *
\    File Version:  5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
-blink
marker -blink
hex ram

$ff82 constant portc
$ff94 constant trisc
$ff8b constant latc

: blink ( n -- )
        [ trisc 1 0 bcf, ]
        begin
                [ portc 1 a, bcf, ]
                dup ms
                [ portc 1 a, bsf, ]
                dup ms
                key?
        until
        key 2drop 
;
ram
: 99blink 99 blink ;
' 99blink is turnkey
warm


\ Test the toggling speed of a few implementations
-speed
marker -speed
: speed ( -- )  \ 833 KHz @ 12 MHz
    [ 
        trisc 2 0 bcf,
        begin,
                portc 2 0 bcf,
                portc 2 0 bsf,
        again,
    ]
;

: set0 [ portc 2 0 bcf, ] ;
: set1 [ portc 2 0 bsf, ] ;

: speed2 ( -- ) \ 277 KHz@12 MHz
        4 trisc mclr
        begin
                set1
                set0
        again
;

: speed3 ( -- ) \ 32,2 Khz @ 12Mhz
        4 trisc mclr
        begin
                4 portc mset
                4 portc mclr
        again
;
