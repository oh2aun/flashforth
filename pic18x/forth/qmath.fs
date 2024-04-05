\                                                                     *
\    Filename:      qmath.fs                                          *
\    Date:          05.04.2024                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18X                                            *
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
1 constant b,

\ registers for parameter stack handling
$ffdd constant Sminus
$ffdc constant plusS
$ffdf constant Srw

\ Assembler words
$6a as2 clrf,           ( f a -- )  
$68 as2 setf,           ( f a -- )  
$24 as3 addwf,          ( f d a -- )
$20 as3 addwfc,         ( f d a -- )
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
  [ plusS w, a, movf,  2 b, movwf, ] \ divisor
  [ plusS w, a, movf,  3 b, movwf, ]
  [ plusS w, a, movf,  0 b, movwf, ]
  [ plusS w, a, movf,  1 b, movwf, ]
  [ 32   movlw,  ]
  [ 4 b, movwf,  ]
  [ begin,  ]
  [   5     b, clrf, ]
  [   $d8 0 a, bcf,  ]  \ Clear carry in status register 
  [   7  f, a, rlcf, ]
  [   8  f, a, rlcf, ]
  [   5  f, a, rlcf, ]
  [   6  f, a, rlcf, ]
  [   3  f, a, rlcf, ]
  [   4  f, a, rlcf, ]
  [   1  f, a, rlcf, ]
  [   2  f, a, rlcf, ]
  [   5 f, b, rlcf,  ]
  
  [   0  w, b, movf,   ]
  [   3  w, a, subwf,  ]
  [   1  w, b, movf,   ]
  [   4  w, a, subwfb, ]
  [   2  w, b, movf,   ]
  [   1  w, a, subwfb, ]
  [   3  w, b, movf,   ]
  [   2  w, a, subwfb, ]
  [   0        movlw,  ]
  [   5 w, b, subwfb,  ]
  
  [   cc, if, ]
  [      0 w, b, movf,   ]
  [      3 f, a, subwf,  ]
  [      1 w, b, movf,   ]
  [      4 f, a, subwfb, ]
  [      2 w, b, movf,   ]
  [      1 f, a, subwfb, ]
  [      3 w, b, movf,   ]
  [      2 f, a, subwfb, ]
  [      7 0  a, bsf,    ]
  [   then,  ]
  
  [   4 f, b, decf,  ]
  [ z, until,  ]
  2swap
;

\ extend a double number to a quad number
: d>q ( d -- q )
  [   0 movlw,  ]
  [   2 7 a, btfsc,    ]
  [   $ff movlw,       ]
  [   Sminus a, movwf,  ]
  [   Sminus a, movwf,  ]
  [   Sminus a, movwf,  ]
  [   Sminus a, movwf,  ]
;

\ add a double number to quad number
: qm+ ( q d -- q )
  [ 0     b, clrf,      ]
  [ 2  7  a, btfsc,     ]
  [ 0     b, setf,      ]
  [ 3  w, a, movf,      ]
  [ 11 f, a, addwf,     ]
  [ 4  w, a, movf,      ]
  [ 12 f, a, addwfc,    ]
  [ 1  w, a, movf,      ]
  [ 9  f, a, addwfc,    ]
  [ 2  w, a, movf,      ]
  [ 10 f, a, addwfc,    ]
  [ 0  w, b, movf,      ]
  [ 7  f, a, addwfc,    ]
  [ 8  f, a, addwfc,    ]
  [ 5  f, a, addwfc,    ]
  [ 6  f, a, addwfc,    ]
  2drop
;

\ multiply two double numbers to a quad result.
: uq* ( ud1 ud2 -- uq )
  [ 1 w, a, movf,  10 b, movwf, ] 
  [ 2 w, a, movf,  11 b, movwf, ] \ ud2h
  [ 3 w, a, movf,   8 b, movwf, ]  
  [ 4 w, a, movf,   9 b, movwf, ] \ ud2l
  [ 5 w, a, movf,  14 b, movwf, ] 
  [ 6 w, a, movf,  15 b, movwf, ] \ ud1h
  [ 7 w, a, movf,  12 b, movwf, ] 
  [ 8 w, a, movf,  13 b, movwf, ] \ ud1l
   
  [ 9  w, b, movf,  Sminus a, movwf, ]
  [ 8  w, b, movf,  Sminus a, movwf, ]
  [ 13 w, b, movf,  Sminus a, movwf, ]
  [ 12 w, b, movf,  Sminus a, movwf, ]
  um*                                 \ ud2l*ud1l ( ll )
  [ plusS w, a, movf,  8 a, movwf, ]
  [ plusS w, a, movf,  8 a, movwf, ]
  [ plusS w, a, movf,  8 a, movwf, ]
  [ plusS w, a, movf,  8 a, movwf, ]    \ lacc = ll
  
  [ 11 w, b, movf,  Sminus a, movwf, ]
  [ 10 w, b, movf,  Sminus a, movwf, ]
  [ 15 w, b, movf,  Sminus a, movwf, ]
  [ 14 w, b, movf,  Sminus a, movwf, ]
  um*                                 \ ud2h*ud1h ( ll hh )
  [ plusS w, a, movf,  4 a, movwf, ]
  [ plusS w, a, movf,  4 a, movwf, ]
  [ plusS w, a, movf,  4 a, movwf, ]
  [ plusS w, a, movf,  4 a, movwf, ]    \ hacc = hh

  [ 9 w, b, movf,  Sminus a, movwf, ]
  [ 8 w, b, movf,  Sminus a, movwf, ]   \ ud2l
  [ 15 w, b, movf,  Sminus a, movwf, ]
  [ 14 w, b, movf,  Sminus a, movwf, ]  \ ud1h
  um*                                  \ ud2l* ud1h
\ r2 r3 r0 r1 q6 q7 q4 q5 q2 q3 q0 q1
\ 1  2  3  4  5  6  7  8  9  10 11 22
  [ 3   w, a, movf,    ]
  [ 11  f, a, addwf,   ]
  [ 4   w, a, movf,    ]
  [ 12  f, a, addwfc,  ]
  [ 1   w, a, movf,    ]
  [ 5   f, a, addwfc,  ]
  [ 2   w, a, movf,    ]
  [ 6   f, a, addwfc,  ]
  [ 0         movlw,   ]
  [ 7   f, a, addwfc,  ]
  [ 8   f, a, addwfc,  ]

  [ 10 w, b, movf,  1 a, movwf, ]
  [ 11 w, b, movf,  2 a, movwf, ] \ ud2h
  [ 12 w, b, movf,  3 a, movwf, ]
  [ 13 w, b, movf,  4 a, movwf, ] \ ud1l
   um*
\ r3 r4 r1 r2 q6 q7 q4 q5 q2 q3 q0 q1
\ 1  2  3  4  5  6  7  8  9  10 11 22
  [ 3   w, a, movf,    ]
  [ 11  f, a, addwf,   ]
  [ 4   w, a, movf,    ]
  [ 12  f, a, addwfc,  ]
  [ 1   w, a, movf,    ]
  [ 5   f, a, addwfc,  ]
  [ 2   w, a, movf,    ]
  [ 6   f, a, addwfc,  ]
  [ 0         movlw,   ]
  [ 7   f, a, addwfc,  ]
  [ 8   f, a, addwfc,  ]
  2drop
;


