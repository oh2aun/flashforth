\                                                                     *
\    Filename:      qmath.txt                                         *
\    Date:          05.05.2016                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Quad math words
\ NOTE: temporary values are kept in the flash buffer


ram hex
\ Assembler definitions
1 constant f,     \ Destination File

\ registers for parameter stack handling
$ffed constant Sminus
$ffec constant plusS
$ffef constant Srw
$ffd8 constant status

\ Assembler words
: as2 ( opcode "name" -- ) ( f a -- )
  co:
  does> rot ic, or ic, ;

$24 as3 addwf,          ( f d a -- )  
$20 as3 addwfc,         ( f d a -- )  
$6a as2 clrf,           ( f a -- )  
$04 as3 decf,           ( f d a -- )  
: movff,                ( fs fd -- )
  swap $0fff and $c000 or i, $0fff and $f000 or i, ;    
$6e as2 movwf,          ( f a -- )    
$34 as3 rlcf,           ( f d a -- )  
$5c as3 subwf,          ( f d a -- )  
$58 as3 subwfb,         ( f d a -- )  
$0e00 as1 movlw,        ( k -- )  
$e2 constant cc,    \ bc


\ These variables are overlapping the flash buffer
\ The flash buffer is written to flash before
\ the variables are used.
$f000 constant dividend
$f008 constant divisor
$f00c constant dcnt
$f00d constant tmp

\ Divide a 64 bit unsigned number with a 32 bit unsigned number
\ The result is a 32 bit remainder and 32 bit quotient
: uq/mod ( qu du -- du-rem du-quot )
  iflush \ empty the flash buffer before using it for temp data
  [ Sminus divisor 3 + movff,  ]
  [ Sminus divisor 2 + movff,  ]
  [ Sminus divisor 1 + movff,  ]
  [ Sminus divisor 0 + movff,  ]
  [ Sminus dividend 7 + movff,  ]
  [ Sminus dividend 6 + movff,  ]
  [ Sminus dividend 5 + movff,  ]
  [ Sminus dividend 4 + movff,  ]
  [ Sminus dividend 3 + movff,  ]
  [ Sminus dividend 2 + movff,  ]
  [ Sminus dividend 1 + movff,  ]
  [ Sminus dividend 0 + movff,  ]
  [ $20 movlw,  ]
  [ dcnt a, movwf,  ]
  [ begin,  ]
  [   tmp a, clrf,  ]
  [   status 0 a, bcf,  ]
  [   dividend 0 + f, a, rlcf,  ]
  [   dividend 1 + f, a, rlcf,  ]
  [   dividend 2 + f, a, rlcf,  ]
  [   dividend 3 + f, a, rlcf,  ]
  [   dividend 4 + f, a, rlcf,  ]
  [   dividend 5 + f, a, rlcf,  ]
  [   dividend 6 + f, a, rlcf,  ]
  [   dividend 7 + f, a, rlcf,  ]
  [   tmp          f, a, rlcf,  ]
  [   divisor  0 + w, a, movf,  ]
  [   dividend 4 + w, a, subwf,  ]
  [   divisor  1 + w, a, movf,  ]
  [   dividend 5 + w, a, subwfb,  ]
  [   divisor  2 + w, a, movf,  ]
  [   dividend 6 + w, a, subwfb,  ]
  [   divisor  3 + w, a, movf,  ]
  [   dividend 7 + w, a, subwfb,  ]
  [   0 movlw,  ]
  [   tmp w, a, subwfb,  ]
  [   cc, if, ]
  [      divisor  0 + w, a, movf,  ]
  [      dividend 4 + f, a, subwf,  ]
  [      divisor  1 + w, a, movf,  ]
  [      dividend 5 + f, a, subwfb,  ]
  [      divisor  2 + w, a, movf,  ]
  [      dividend 6 + f, a, subwfb,  ]
  [      divisor  3 + w, a, movf,  ]
  [      dividend 7 + f, a, subwfb,  ]
  [      dividend 0 + 0  a, bsf, ]
  [   then,  ]
  [   dcnt f, a, decf,  ]
  [ z, until,  ]
  [ dividend 4 + plusS movff,  ]
  [ dividend 5 + plusS movff,  ]
  [ dividend 6 + plusS movff,  ]
  [ dividend 7 + plusS movff,  ]
  [ dividend 0 + plusS movff,  ]
  [ dividend 1 + plusS movff,  ]
  [ dividend 2 + plusS movff,  ]
  [ dividend 3 + plusS movff,  ]
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
  iflush
  [ 0 movlw,               ]
  [ Srw 7  a, btfsc,       ]
  [ $ff movlw,             ]
  [ tmp 8 +  a, movwf,     ]
  [ Sminus tmp 3 + movff,  ]
  [ Sminus tmp 2 + movff,  ]
  [ Sminus tmp 1 + movff,  ]
  [ Sminus tmp 0 + movff,  ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ Sminus w, a, movf,     ]
  [ tmp 0 + w, a, movf,    ]
  [ plusS f, a, addwf,     ]
  [ tmp 1 + w, a, movf,    ]
  [ plusS f, a, addwfc,    ]
  [ tmp 2 + w, a, movf,    ]
  [ plusS f, a, addwfc,    ]
  [ tmp 3 + w, a, movf,    ]
  [ plusS f, a, addwfc,    ]
  [ tmp 8 + w, a, movf,    ]
  [ plusS f, a, addwfc,    ]
  [ plusS f, a, addwfc,    ]
  [ plusS f, a, addwfc,    ]
  [ plusS f, a, addwfc,    ]
;

\ multiply two double numbers to a quad result.
: uq* ( ud ud -- uq )
  iflush
  [ Sminus $f00b movff, Sminus $f00a movff, ] 
  [ Sminus $f009 movff, Sminus $f008 movff, ] 
  [ Sminus $f00f movff, Sminus $f00e movff, ] 
  [ Sminus $f00d movff, Sminus $f00c movff, ] 
  [ $f008 plusS movff, $f009 plusS movff, ] 
  [ $f00c plusS movff, $f00d plusS movff, ] 
  um*
  [ $f00a plusS movff, $f00b plusS movff, ] 
  [ $f00e plusS movff, $f00f plusS movff, ]
  um*
  [ Sminus $f003 movff, Sminus $f002 movff, ] 
  [ Sminus $f001 movff, Sminus $f000 movff, ] 
  [ $f008 plusS movff, $f009 plusS movff, ]
  [ $f00e plusS movff, $f00f plusS movff, ] 
  um*
  [ Sminus $f007 movff, Sminus $f006 movff, ] 
  [ Sminus $f005 movff, Sminus $f004 movff, ] 
  [ Sminus w, a, movf,    ]
  [ $f004  w, a, movf,    ]
  [ Srw    f, a, addwf,   ]
  [ $f005  w, a, movf,    ]
  [ plusS  f, a, addwfc,  ]
  [ $f006  w, a, movf,    ]
  [ $f000  f, a, addwfc,  ]
  [ $f007  w, a, movf,    ]
  [ $f001  f, a, addwfc,  ]
  [ 0            movlw,   ]
  [ $f002  f, a, addwfc,  ]
  [ $f003  f, a, addwfc,  ]
  [ $f00a plusS movff, $f00b plusS movff, ]
  [ $f00c plusS movff, $f00d plusS movff, ] 
  um*
  [ Sminus $f007 movff, Sminus $f006 movff, ] 
  [ Sminus $f005 movff, Sminus $f004 movff, ] 
  [ Sminus w, a, movf,    ]
  [ $f004  w, a, movf,    ]
  [ Srw    f, a, addwf,   ]
  [ $f005  w, a, movf,    ]
  [ plusS  f, a, addwfc,  ]
  [ $f006  w, a, movf,    ]
  [ $f000  f, a, addwfc,  ]
  [ $f007  w, a, movf,    ]
  [ $f001  f, a, addwfc,  ]
  [ 0            movlw,   ]
  [ $f002  f, a, addwfc,  ]
  [ $f003  f, a, addwfc,  ]
  [ $f000 plusS  movff, $f001 plusS  movff, ]
  [ $f002 plusS  movff, $f003 plusS  movff, ]
;


