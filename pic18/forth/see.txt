\ PIC18 disassembler
-see
marker -see

flash create xm0
[ $fe00 ,                    ]
[ %0110.1010 c, ," clrf"     ]
[ %0110.0010 c, ," cpfseq"   ]
[ %0110.0100 c, ," cpfsgt"   ]
[ %0110.0000 c, ," cpfslt"   ]
[ %0110.1110 c, ," movwf"    ]
[ %0110.0010 c, ," mulwf"    ]
[ %0110.1100 c, ," negf"     ]
[ %0110.1000 c, ," setf"     ]
[ %0110.0110 c, ," tstfsz" 0 ,

flash create xm1
[ $fc00 ,                    ]
[ %0010.0100 c, ," addwf"    ]
[ %0010.0000 c, ," addwfc"   ]
[ %0001.0100 c, ," andwf"    ]
[ %0001.1100 c, ," comf"     ]
[ %0000.0100 c, ," decf"     ]
[ %0010.1100 c, ," decfsz"   ]
[ %0100.1100 c, ," dcfsnz"   ]
[ %0010.1000 c, ," incf"     ]
[ %0011.1100 c, ," incfsz"   ]
[ %0100.1000 c, ," infsnz"   ]
[ %0001.0000 c, ," iorwf"    ]
[ %0101.0000 c, ," movf"     ]
[ %0011.0100 c, ," rlcf"     ]
[ %0100.0100 c, ," rlncf"    ]
[ %0011.0000 c, ," rrcf"     ]
[ %0100.0000 c, ," rrncf"    ]
[ %0101.0100 c, ," subfwb"   ]
[ %0101.1100 c, ," subwf"    ]
[ %0101.1000 c, ," subwfb"   ]
[ %0011.1000 c, ," swapf"    ]
[ %0001.1000 c, ," xorwf" 0 ,

flash create xm2
[ $f000 ,                    ]
[ %1100.0000 c, ," movff"    ]
[ %1001.0000 c, ," bcf"      ]
[ %1000.0000 c, ," bsf"      ]
[ %1011.0000 c, ," btfsc"    ]
[ %1010.0000 c, ," btfss"    ]
[ %0111.0000 c, ," btg" 0 ,

flash create xm3
[ $ff00 ,                    ]
[ %1110.0010 c, ," bc"       ]
[ %1110.0110 c, ," bn"       ]
[ %1110.0011 c, ," bnc"      ]
[ %1110.0111 c, ," bnn"      ]
[ %1110.0101 c, ," bnov"     ]
[ %1110.0001 c, ," bnz"      ]
[ %1110.0100 c, ," bov"      ]
[ %1110.0000 c, ," bz" 0 ,

flash create xm4
[ $ff00 ,                     ]
[ %0000.1111 c, ," addlw"     ]
[ %0000.1011 c, ," andlw"     ]
[ %0000.1001 c, ," iorlw"     ]
[ %0000.1110 c, ," movlw"     ]
[ %0000.1101 c, ," mullw"     ]
[ %0000.1100 c, ," retlw"     ]
[ %0000.1000 c, ," sublw"     ]
[ %0000.0001 c, ," movlb"     ]
[ %0000.1010 c, ," xorlw" 0 ,

flash create xm5
[ $f800 ,                     ]
[ %1101.1000 c, ," rcall"     ]
[ %1101.0000 c, ," bra" 0 ,

flash create xm6
[ $ff00 ,                     ]
[ %1110.1100 c, ," call"      ]
[ %1110.1111 c, ," goto" 0 ,

flash create regs
[ $d9 c, ," rpl" ]
[ $da c, ," rph" ]
[ $db c, ," (rp+w)" ]
[ $dc c, ," (+rp)" ]
[ $dd c, ," (rp-)" ]
[ $de c, ," (rp+)" ]
[ $df c, ," (rp)" ]
[ $e1 c, ," tpl" ]
[ $e2 c, ," tph" ]
[ $e3 c, ," (tp+w)" ]
[ $e4 c, ," (+tp)" ]
[ $e5 c, ," (tp-)" ]
[ $e6 c, ," (tp+)" ]
[ $e7 c, ," (tp)"  ]
[ $e9 c, ," spl" ]
[ $ea c, ," sph" ]
[ $eb c, ," (sp+w)" ]
[ $ec c, ," (+sp)" ]
[ $ed c, ," (sp-)" ]
[ $ee c, ," (sp+)" ]
[ $ef c, ," (sp)" ]
[ $f9 c, ," pcl" ]
[ $fa c, ," pclath" ]
[ $fd c, ," tosl" ]
[ $fe c, ," tosh" 0 ,

: Dup dup ;
: ?again Dup postpone 0= postpone until ; immediate
: *@ Dup @ ;
: x.$ c@+ type space ;
: Next c@+ + aligned ;
: Skip 2* over + 2+ ;
: And and ;
: u.2 $ff And 2 u.r ;
: u.4 4 u.r ;
: x.s ( a symtab -- a' code )
  over @ !p>r         \ P instruction
  @+ >r               \ R mask
  begin               \ a s
    c@+ 8 lshift Dup        \ a s n n
  while
    Dup @p r@ And xor \ a s n flag
  while
    drop Next         \ a s
  repeat              \ a s n
    over x.$          \ a s n
  then                \ a s n
  nip rdrop r>p
;
: x.a ( opc -- ) $0100 And if [char] b else [char] a then emit ;
: x.d ( opc -- ) $0200 And if [char] f else [char] w then emit space ;
: x.b ( opc -- ) $0e00 And 9 rshift u. ;

: x.r ( opc -- opc )
  >r regs
  begin
    c@+ Dup
  while
    $1ff r@ And xor
  while
    Next
  repeat
    Dup x.$
  then 
  0= if drop r@ u.2 then
  r>
;
: x.0 ( a -- a' flag ) xm0 x.s Dup >r if @+ x.r x.a then r> ;

: x.1 ( a -- a' ) xm1 x.s if *@ x.r Dup x.d x.a then ;

: x02 $fff And Dup $f5f > if $ff And x.r drop else u.4 then ;

: x.2
  xm2 x.s Dup
  if   $c000 =
       if   @+ x02 *@ x02
       else *@ Dup u.2 Dup x.b x.a
       then
  else drop
  then ;
 
: x.3
  xm3 x.s
  if *@ $ff And Dup 80 And
     if   $ff00 or
     then Skip u.4
  then ;

: x.4 xm4 x.s if *@ u.2 then ;

: .(s ['] (s" = if space 2+ c@+ 2dup type + aligned 2- then ;

: x.5
  xm5 x.s Dup >a
  if
       *@ $7ff And Dup $400 And
       if   $f800 or 
       then Skip
       a> $d800 =
       if   Dup c>n .id .(s
       else u.4
       then
  then ;

: x.6
  xm6 x.s Dup >a
  if @+ $ff And over @ 8 lshift or 2* Dup  c>n .id
      a> $ec00 =
      if   .(s
      else drop
      then
  then a> $ef00 = if cr abort then ;

: x.7 *@ $0012 = if ." return" cr abort then ;

: see
  hex '
  begin
    cr Dup u.4 Dup @ u.4
    x.0 ?again
    x.1 x.2 x.3 x.4 x.5 x.6 x.7 
    2+
 again ;

