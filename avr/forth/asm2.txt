\ *********************************************************************
\    Filename:      asm2.txt                                          *
\    Date:          16.10.2017                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega                                            *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Table driven assembler for Atmega chips
-as
marker -as
hex
: ar: ( n "name" -- ) create does> swap 2* 2* + ;
: ri! ( index n -- ) here swap - dup c@ rot 4 lshift or swap c! ;

flash ar: rules
\ d mask.shift, r mask.shift
[ 000.0 , 000.0 , ] \ 00 xxxx.xxxx.xxxx.xxxx ret sleep wdr
[ 1f0.4 , 00f.0 , ] \ 01 xxxx.xxxd.dddd.rrrr ld x+ -x y+ -y z+ -z
[ 1f0.4 , 00f.0 , ] \ 02 xxxx.xxxd.dddd.rrrr st x+ -x y+ -y z+ -z
[ 030.4 , 0cf.2 , ] \ 03 xxxx.xxxx.kkpp.kkkk adiw sbiw
[ 0f8.3 , 007.0 , ] \ 04 xxxx.xxxx.aaaa.abbb cbi sbi sbic sbis
[ 1f0.4 , 60f.5 , ] \ 05 xxxx.xaad.dddd.aaaa in
[ 1f0.4 , 60f.5 , ] \ 06 xxxx.xaad.dddd.aaaa out
[ 1f0.4 , 000.0 , ] \ 07 xxxx.xxxd.dddd.xxxx lds
[ 1f0.4 , 000.0 , ] \ 08 xxxx.xxxd.dddd.xxxx sts
[ 1f0.4 , 000.0 , ] \ 09 xxxx.xxxd.dddd.xxxx pop push com neg
                    \                        swap inc asr lsr ror dec
[ 0f0.4 , f0f.4 , ] \ 0a xxxx.kkkk.dddd.kkkk cpi sbci subi ori andi ldi
[ 0f0.4 , 00f.0 , ] \ 0b xxxx.xxxx.dddd.rrrr movw
[ 1f0.4 , 20f.5 , ] \ 0c xxxx.xxrd.dddd.rrrr cpc cp sbc sub add adc cpse
                    \                        and eor or mov mul
                    \                        ( rol lsl tst clr ser )
[ 1f0.4 , 007.0 ,   \ 0d xxxx.xxxd.dddd.0rrr bld bst sbrc sbrs
\ 000.0 , 000.0 ,   \ 0f if then begin until again

\ 126 opcodes opcode name ruleindex namelen
flash create opcodes
[ 9508 , ," ret"     0 4 ri! ]
[ 9588 , ," sleep"   0 6 ri! ]
[ 0000 , ," nop"     0 4 ri! ]
[ 9000 , ," ld"      1 4 ri! ]
[ 9200 , ," st"      2 4 ri! ]
[ 9600 , ," adiw"    3 6 ri! ]
[ 9700 , ," sbiw"    3 6 ri! ]
[ 9800 , ," cbi"     4 4 ri! ]
[ 9900 , ," sbic"    4 6 ri! ]
[ 9a00 , ," sbi"     4 4 ri! ]
[ 9b00 , ," sbis"    4 6 ri! ]
[ b000 , ," in"      5 4 ri! ]
[ b800 , ," out"     6 4 ri! ]
[ 9000 , ," lds"     7 4 ri! ]
[ 9200 , ," sts"     8 4 ri! ]
[ 900f , ," pop"     9 4 ri! ]
[ 920f , ," push"    9 6 ri! ]
[ 9400 , ," com"     9 4 ri! ]
[ 9401 , ," neq"     9 4 ri! ]
[ 9402 , ," swap"    9 6 ri! ]
[ 9403 , ," inc"     9 4 ri! ]
[ 9405 , ," asr"     9 4 ri! ]
[ 9406 , ," lsr"     9 4 ri! ]
[ 9407 , ," ror"     9 4 ri! ]
[ 940a , ," dec"     9 4 ri! ]
[ 3000 , ," cpi"     a 4 ri! ]
[ 4000 , ," sbci"    a 6 ri! ]
[ 5000 , ," subi"    a 6 ri! ]
[ 6000 , ," ori"     a 4 ri! ]
[ 7000 , ," andi"    a 6 ri! ]
[ e000 , ," ldi"     a 4 ri! ]
[ 0100 , ," movw"    b 6 ri! ]
[ 9c00 , ," mul"     c 4 ri! ]
[ 0400 , ," cpc"     c 4 ri! ]
[ 0800 , ," sbc"     c 4 ri! ]
[ 0c00 , ," add"     c 4 ri! ]
[ 1000 , ," cpse"    c 6 ri! ]
[ 1400 , ," cp"      c 4 ri! ]
[ 1800 , ," sub"     c 4 ri! ]
[ 1c00 , ," adc"     c 4 ri! ]
[ 2000 , ," and"     c 4 ri! ]
[ 2400 , ," eor"     c 4 ri! ]
[ 2800 , ," or"      c 4 ri! ]
[ 2c00 , ," mov"     c 4 ri! ]
[ f800 , ," bld"     d 4 ri! ]
[ fa00 , ," bst"     d 4 ri! ]
[ fc00 , ," sbrc"    d 6 ri! ]
[ fe00 , ," sbrs"    d 6 ri! ]
[ 0000 , ," if"      f 4 ri! ]
[ 0002 , ," then"    f 6 ri! ]
[ 0004 , ," begin"   f 6 ri! ]
[ 0006 , ," until"   f 6 ri! ]
[ 0008 , ," again"   f 6 ri! ]
[ ffff ,
ram

flash create sy1
hex
[ 1 , ," z+" 2 , ," -z" ]
[ 9 , ," y+" a , ," -y" ]
[ d , ," x+" e , ," -x" ]
[ $ffff ,
ram

flash create sy2
[ f400 , ," cs" ]
[ f400 , ," lo" ]
[ f401 , ," eq" ]
[ f402 , ," mi" ]
[ f403 , ," vs" ]
[ f404 , ," lt" ]
[ f405 , ," hs" ]
[ f406 , ," ts" ]
[ f407 , ," ie" ]
[ f000 , ," cc" ]
[ f000 , ," sh" ]
[ f001 , ," ne" ]
[ f002 , ," pl" ]
[ f003 , ," vc" ]
[ f004 , ," ge" ]
[ f005 , ," hc" ]
[ f006 , ," tc" ]
[ f007 , ," id" ]
[ ffff ,
ram
hex
\
: dsm  ( index -- shift mask ) @ dup f and swap 4 rshift ;
: msi ( code index -- code)   rules dsm >r lshift r> and ;
: split ( code index -- code )
  rules 2+ dsm >r over swap lshift fff0 and or r> and ;

: asm ( opc index d/b r/k/a/b -- asm )
  rot >r swap
  r@  msi                     \ dest shifted and masked
  swap r> split               \ resource splitted and masked
  or or ;                     \ opc n2 n1 combined

: sy? ( word table -- address )
  begin
    @+ 1+
  while
    2dup n=
    if   c@+ 7 and + aligned
    else nip 2- exit
    then
  repeat
  drop c@+ type ." ?" abort ;

: op? ( word table -- opc index ) sy? dup @ swap 2+ c@ 4 rshift ;

: bw bl word ;
: N# number? 1- 0= abort" ?" ;
: n# bw N# ;
: d# bw sy1 sy? @ ;
: r# bw dup 1+ dup c@ 4f - swap c! N# 1f and ;
: c# bw sy2 sy? @ ;

: as1 2+ - 2/ 3 lshift 3f8 and ;
:noname ;                                 \ again
:noname c# >r ihere as1 r> or i, ;        \ until
:noname ihere ;                           \ begin
:noname ihere over as1 over @ or swap ! ; \ then
:noname c# i, ihere 2- ;                  \ if
flash create ask , , , , , ram

:noname r# 2/ r# 2/ asm ;           \ movw
:noname r# n# asm ;
:noname r# false asm ;              \ one param
:noname n# >r r# false asm i, r> ;  \ sts
:noname r# n# >r false asm i, r> ;  \ lds
:noname n# r# swap asm ;            \ out
:noname r# n# asm ;                 \ in
:noname n# n# asm ;                 \ sbic 0-31, 0-7
:noname r# 2/ n# asm ;              \ adiw sbiw r24 r26 r28 r30
:noname d# r# swap asm ;            \ st
:noname r# d# asm ;                 \ ld
:noname drop ;                      \ no params
flash create ass , , , , , , , , , , , , ram

: as: ( -- )
  bw opcodes op?
  dup f - 0= 
  if drop ask + @ex            \ handle flow control
  else
    dup $c <
    if   dup 2* ass + @ex
    else r# r# asm             \ two params
    then i,
  then
; immediate

