\ *********************************************************************
\    Interrupts example for FlashForth                                *
\    Filename:      int14k50asm.txt                                   *
\    Date:          01.04.2020                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18F14K50                                       *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ NOTE: Always deactivate user interrupts before
\ the interrupt word is removed.
\ You must also clear any related interrupt enable bits 
\ and interrupt flags before zeroing the interrupt vector
\ NOTE: User defined interrupts must always be defined as high priority interrupts.
\ NOTE: Interrupt vector 0 is the high prority interrupt vector.
\ NOTE: UART and tick timer interrupts are always low priority interrupts.

ccp1.int.off
-user.int
marker -user.int
decimal
$ff94 constant trisc
$ff8b constant latc
$ffb1 constant t3con
$ffbd constant ccp1con
$ffbe constant ccpr1
$ff9e constant pir1
$ff9d constant pie1
$0004 constant ccp1if
$0002 constant ccp1ifb
$0004 constant ccp1ie

\ Variables in access ram
\ $f000 to $f00f is free to use by application
$f008 constant ccp1/
$f00a constant width
$f00c constant ccp1.int.cnt
$f007 constant temp

: ccp1.init
  0 ccp1con c!         \ reset capture module
  5 ccp1con c!         \ capture rising edge
  %10001001 t3con c!   \ timer3 as capture counter prescaler=1
  %00100000 trisc mset \ ccp1 input port configured as input                     
;

\ : as2 ( opcode "name" -- ) ( f a -- )
\   co:
\  does> rot ic, or ic, ;

1 constant f,     \ Destination File
$6e as2 movwf,          ( f a -- )    
$5c as3 subwf,          ( f d a -- )  
$58 as3 subwfb,         ( f d a -- )
$24 as3 addwf,          ( f d a -- ) 
$20 as3 addwfc,         ( f d a -- ) 
$0e00 as1 movlw,        ( k -- )
$0800 as1 sublw,        ( k -- )

: ccp1.int.asm
  [ pir1                  w, a, movf,   ]
  [ ccp1if                      andlw,  ] \ ccp1if pir1 mtst
  [ z, not, if,                         ] \ if
  [     pir1 ccp1ifb         a, bcf,    ]
  [     ccp1con           w, a, movf,   ]
  [     5 sublw,                        ] \ ccp1con c@ 5
  [     z, if,                          ] \ = if
  [             ccp1con 0    a, bcf,    ] \ 4 ccp1con c!
  [             ccpr1 1+  w, a, movf,   ]
  [             ccp1/ 1+     a, movwf,  ]
  [             ccpr1     w, a, movf,   ]
  [             ccp1/        a, movwf,  ] \ ccpr1 @ ccp1/ !
  [     else,                           ]
  [             ccp1con 0    a, bsf,    ] \ 5 ccp1con c!
  [             ccpr1 1+  w, a, movf,   ] 
  [             temp         a, movwf,  ]
  [             ccp1/     w, a, movf,   ]
  [             ccpr1     w, a, subwf,  ]
  [             width        a, movwf,  ]
  [             ccp1/ 1+  w, a, movf,   ]
  [             temp      w, a, subwfb, ]
  [             width 1+     a, movwf,  ] \ ccpr1 @ ccp1/ @ - 
                                          \ width !
  [     then,                           ]
  [     1 movlw,                        ]
  [     ccp1.int.cnt      f, a, addwf,  ]
  [     0 movlw,                        ]
  [     ccp1.int.cnt 1+   f, a, addwfc, ] \ 1 ccp1.int.cnt +!
  [ then,                               ]
;i

: pulsewidth 
  width @ #1000 Fcy u*/mod nip u. ." us"
;

: ccp1.int.init
  0 ccp1.int.cnt !
  ccp1.init
  ['] ccp1.int.asm 0 int!
  ccp1ie pie1 mset
; 

: ccp1.int.off
  0 0 int!
  ccp1ie pie1 mclr
;

\ TESTS
\ tests require that ccp1 input port is configured as output
: test  %00100000 trisc mclr 
        10 ms [ latc 5 a, bsf, ] 
        for next 
        [ latc 5 a, bcf, ] pulsewidth ;
: test0 %00100000 trisc mclr 
        10 ms $20 latc mset       $20 latc mclr pulsewidth ;
: test1 %00100000 trisc mclr 
        10 ms $20 latc mset 1 ms  $20 latc mclr pulsewidth ;
: test2 %00100000 trisc mclr 
        10 ms $20 latc mset 2 ms  $20 latc mclr pulsewidth ;
: test3 %00100000 trisc mclr 
        10 ms $20 latc mset 3 ms  $20 latc mclr pulsewidth ;
: test4 %00100000 trisc mclr 10 ms
        [ latc 5 a, bsf, 0 i, 0 i, 0 i, latc 5 a, bcf, ] pulsewidth ;
ccp1.int.init

ccp1.int.cnt @ u.
test0
test1
test2
test3
test4
10 test
ccp1.int.cnt @ u.

