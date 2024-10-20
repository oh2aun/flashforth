\                                                                     *
\    Filename:      qmath.fs                                          *
\    Date:          20.11.2022                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Quad math words
\ Temporary values are kept in the first 16 bytes of access memory
\ This area is reserved for application use.
\ This versions works all PIC18 access memory layouts.


ram decimal
\ Assembler definitions
1 constant f,     \ Destination File

\ registers for parameter stack handling
$ffed constant Sminus
$ffec constant plusS
$ffef constant Srw

\ Assembler words
$24 as3 addwf,          ( f d a -- )
$20 as3 addwfc,         ( f d a -- )
$6a as2 clrf,           ( f a -- )
$04 as3 decf,           ( f d a -- )
$6e as2 movwf,          ( f a -- )  
$34 as3 rlcf,           ( f d a -- )
$5c as3 subwf,          ( f d a -- )
$58 as3 subwfb,         ( f d a -- )
$0e00 as1 movlw,        ( k -- )
$e2 constant cc,        \ bc



\ Divide a 64 bit unsigned number with a 32 bit unsigned number
\ The result is a 32 bit remainder and 32 bit quotient
: uq/mod ( qu du -- du-rem du-quot )
\  [ Sminus divisor 3 + movff,  ]
  [ Sminus w, a, movf,  11 a, movwf, ] \ divisor
  [ Sminus w, a, movf,  10 a, movwf, ]
  [ Sminus w, a, movf,  9 a, movwf, ]
  [ Sminus w, a, movf,  8 a, movwf, ]
  [ Sminus w, a, movf,  7 a, movwf, ]  \ dividend 
  [ Sminus w, a, movf,  6 a, movwf, ] 
  [ Sminus w, a, movf,  5 a, movwf, ]
  [ Sminus w, a, movf,  4 a, movwf, ]
  [ Sminus w, a, movf,  3 a, movwf, ]
  [ Sminus w, a, movf,  2 a, movwf, ]
  [ Sminus w, a, movf,  1 a, movwf, ]
  [ Sminus w, a, movf,  0 a, movwf, ] 
  [ 32 movlw,  ]
  [ 12 a, movwf,  ]
  [ begin,  ]
  [   13 a, clrf,  ]
  [   $ffd8 0 a, bcf, ]  \ Clear carry in status register 
  [   0 f, a, rlcf,  ]
  [   1 f, a, rlcf,  ]
  [   2 f, a, rlcf,  ]
  [   3 f, a, rlcf,  ]
  [   4 f, a, rlcf,  ]
  [   5 f, a, rlcf,  ]
  [   6 f, a, rlcf,  ]
  [   7 f, a, rlcf,  ]
  [   13 f, a, rlcf,  ]
  [   8 w, a, movf,  ]
  [   4 w, a, subwf,  ]
  [   9 w, a, movf,  ]
  [   5 w, a, subwfb,  ]
  [   10 w, a, movf,  ]
  [   6  w, a, subwfb,  ]
  [   11 w, a, movf,  ]
  [   7  w, a, subwfb,  ]
  [   0 movlw,  ]
  [   13 w, a, subwfb,  ]
  [   cc, if, ]
  [      8 w, a, movf,  ]
  [      4 f, a, subwf,  ]
  [      9 w, a, movf,  ]
  [      5 f, a, subwfb,  ]
  [      10 w, a, movf,  ]
  [      6 f, a, subwfb,  ]
  [      11 w, a, movf,  ]
  [      7 f, a, subwfb,  ]
  [      0 0  a, bsf, ]
  [   then,  ]
  [   12 f, a, decf,  ]
  [ z, until,  ]
  [ 4 w, a, movf,  plusS a, movwf, ]
  [ 5 w, a, movf,  plusS a, movwf, ]
  [ 6 w, a, movf,  plusS a, movwf, ]
  [ 7 w, a, movf,  plusS a, movwf, ]
  [ 0 w, a, movf,  plusS a, movwf, ]
  [ 1 w, a, movf,  plusS a, movwf, ]
  [ 2 w, a, movf,  plusS a, movwf, ]
  [ 3 w, a, movf,  plusS a, movwf, ]
;

\ extend a double number to a quad number
: d>q ( d -- q )
  [   0 movlw,  ]
  [   Srw 7 a, btfsc,  ]
  [   $ff movlw,       ]
  [   plusS a, movwf,  ]
  [   plusS a, movwf,  ]
  [   plusS a, movwf,  ]
  [   plusS a, movwf,  ]
;

\ add a double number to quad number
: qm+ ( q d -- q )
  [ 0 movlw,               ]
  [ Srw 7  a, btfsc,       ]
  [ $ff movlw,             ]
  [ 8 a, movwf,     ]
  [ Sminus w, a, movf, 3 a, movwf, ]
  [ Sminus w, a, movf, 2 a, movwf, ]
  [ Sminus w, a, movf, 1 a, movwf, ]
  [ Sminus w, a, movf, 0 a, movwf, ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ 0 w, a, movf,          ]
  [ plusS f, a, addwf,     ]
  [ 1 w, a, movf,          ]
  [ plusS f, a, addwfc,    ]
  [ 2 w, a, movf,          ]
  [ plusS f, a, addwfc,    ]
  [ 3 w, a, movf,          ]
  [ plusS f, a, addwfc,    ]
  [ 8 w, a, movf,          ]
  [ plusS f, a, addwfc,    ]
  [ plusS f, a, addwfc,    ]
  [ plusS f, a, addwfc,    ]
  [ plusS f, a, addwfc,    ]
;

\ multiply two double numbers to a quad result.
: uq* ( ud ud -- uq )
  [ Sminus w, a, movf,  11 a, movwf, ] 
  [ Sminus w, a, movf,  10 a, movwf, ]
  [ Sminus w, a, movf,  9 a, movwf, ] 
  [ Sminus w, a, movf,  8 a, movwf, ]
  [ Sminus w, a, movf,  15 a, movwf, ] 
  [ Sminus w, a, movf,  14 a, movwf, ] 
  [ Sminus w, a, movf,  13 a, movwf, ] 
  [ Sminus w, a, movf,  12 a, movwf, ] 
  [ 8 w, a, movf,  plusS a, movwf, ]
  [ 9 w, a, movf,  plusS a, movwf, ]
  [ 12 w, a, movf,  plusS a, movwf, ]
  [ 13 w, a, movf,  plusS a, movwf, ]
  um*
  [ 10 w, a, movf,  plusS a, movwf, ]
  [ 11 w, a, movf,  plusS a, movwf, ]
  [ 14 w, a, movf,  plusS a, movwf, ]
  [ 15 w, a, movf,  plusS a, movwf, ]
  um*
  [ Sminus w, a, movf,  3 a, movwf, ] 
  [ Sminus w, a, movf,  2 a, movwf, ] 
  [ Sminus w, a, movf,  1 a, movwf, ] 
  [ Sminus w, a, movf,  0 a, movwf, ] 
  [ 8 w, a, movf,  plusS a, movwf, ]
  [ 9 w, a, movf,  plusS a, movwf, ]
  [ 14 w, a, movf,  plusS a, movwf, ]
  [ 15 w, a, movf,  plusS a, movwf, ]
  um*
  [ Sminus w, a, movf,  7 a, movwf, ] 
  [ Sminus w, a, movf,  6 a, movwf, ]
  [ Sminus w, a, movf,  5 a, movwf, ] 
  [ Sminus w, a, movf,  4 a, movwf, ] 
  [ Sminus w, a, movf,    ]
  [ 4  w, a, movf,    ]
  [ Srw    f, a, addwf,   ]
  [ 5  w, a, movf,    ]
  [ plusS  f, a, addwfc,  ]
  [ 6  w, a, movf,    ]
  [ 0  f, a, addwfc,  ]
  [ 7  w, a, movf,    ]
  [ 1  f, a, addwfc,  ]
  [ 0        movlw,   ]
  [ 2  f, a, addwfc,  ]
  [ 3  f, a, addwfc,  ]
  [ 10 w, a, movf,  plusS a, movwf, ]
  [ 11 w, a, movf,  plusS a, movwf, ]
  [ 12 w, a, movf,  plusS a, movwf, ]
  [ 13 w, a, movf,  plusS a, movwf, ]
   um*
  [ Sminus w, a, movf,  7 a, movwf, ] 
  [ Sminus w, a, movf,  6 a, movwf, ]
  [ Sminus w, a, movf,  5 a, movwf, ] 
  [ Sminus w, a, movf,  4 a, movwf, ]
  [ Sminus w, a, movf,    ]
  [ 4  w, a, movf,    ]
  [ Srw    f, a, addwf,   ]
  [ 5  w, a, movf,    ]
  [ plusS  f, a, addwfc,  ]
  [ 6  w, a, movf,    ]
  [ 0  f, a, addwfc,  ]
  [ 7  w, a, movf,    ]
  [ 1  f, a, addwfc,  ]
  [ 0        movlw,   ]
  [ 2  f, a, addwfc,  ]
  [ 3  f, a, addwfc,  ]
  [ 0 w, a, movf,  plusS a, movwf, ]
  [ 1 w, a, movf,  plusS a, movwf, ]
  [ 2 w, a, movf,  plusS a, movwf, ]
  [ 3 w, a, movf,  plusS a, movwf, ]
;


