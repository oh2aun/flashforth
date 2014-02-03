\ *********************************************************************
\    Filename:      asm.txt                                           *
\    Date:          03.02.2014                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega                                            *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ FlashForth assembler for Atmega chips
-as
marker -as
hex

\ Combine the opcode with the operand fields
: mask! ( dest1 opcode mask -- instruction )
    rot over invert and rot rot and or ; \ dest1&!mask src&mask or

\ Fetch src and mask from addr and append to dictinary via mask
: mask,  ( dest1 addr -- )
    2@ swap mask! i, ;

\ Create name and data fields for opcode and mask in flash
: create,, flash create , , ram ;

: Rd,Rr:    ( Rd Rr opcode mask -- xxxz.xxrd.dddd.rrrr )
  create,,
  does> >r
    $1f and dup $5 lshift or $20f and  \ -- Rd r00000rrrr
    swap $4 lshift $1f0 and            \ -- rr 0ddddd0000
    or r> 2@ mask!              \ -- ddrr opcode mask mask!
    dup $fc07 and $9000 =
    if $efff and then i, ;             \ if Z or Y then z=0

: Rd:        ( Rd opcode mask -- xxxx.xxxd.dddd.xxxx )
  create,,
  does> >r 
    $4 lshift $1f0 and                 \ -- 0ddddd0000
    r> mask, ;

 \ Operands Rd,constant 8bit
: Rd,k:     ( Rd k8 opcode mask -- xxxx.kkkk.dddd.kkkk )
  create,,
  does> >r 
    $ff and dup $4 lshift or $f0f and  \ -- Rd kkkk0000kkkk
    swap $4 lshift $f0 and             \ -- kk dddd0000
    or r> mask, ;              \ kkdd opcode mask mask! to flash

\ Operands Rd,Rr,constant 6bit
: Rd,Rr+q:  ( Rd Rr k6 opcode mask -- xxkx.kkxd.dddd.rkkk )
  create,,
  does> >r 
    $3f and dup $7 lshift                \ -- Rd Rr k6 xkkkkkkxxxxxxx
    dup $1000 and $1 lshift or or $2c07 and  \ -- Rd Rr kxkkxxxxxxxkkk
    rot $4 lshift $1f0 and              \ -- Rr kk ddddd0000
    or swap 8 and                       \ -- kkdd rxxx
    or r> mask, ;                 \ kkddrr opcode mask mask! to flash


\ Operands Rw pair,constant 6bit
: Rw,k:     ( Rw k6 opcode mask -- xxxx.xxxx.kkww.kkkk )
  create,,
  does> >r 
    $3f and dup $2 lshift $c0 and      \ -- Rw k6 kk000000
    swap $f and or                     \ -- Rw kk00kkkk
    swap $4 lshift $30 and             \ -- kk ww0000
    or r> mask, ;                      \ kkww opcode mask mask! to flash

\ Operands P-port,bit
: P,b:      ( P b opcode mask -- xxxx.xxxx.PPPP.Pbbb )
  create,,
  does> >r 
    $7 and swap $3 lshift              \ -- 0bbb PPPPP000
    or r> mask, ;                      \ PPbb opcode mask mask! to flash

\ Operands Rd,P-port
: Rd,P:     ( Rd P opcode mask -- xxxx.xPPd.dddd.PPPP )
  create,,
  does> >r 
    $3f and dup $5 lshift or $60f and  \ -- Rd PP00000PPPP
    swap $4 lshift $1f0 and            \ -- PP 00ddddd0000
    or r> mask, ;                      \ ddPP opcode mask mask! to flash


\ Operand k16 k6
: k22:      ( k16 k6 opcode mask -- k16 xxxx.xxxk.kkkk.xxxk )
  create,,
  does> >r 
    dup $1 and swap $3 lshift          \ -- 000k kkkkkk000
    or r> mask,  i, ;                  \ k16 kk opcode mask mask! to flash

\ Opcode only to flash
: op:       ( opcode -- )
  flash create , ram does> @ i, ;


0100 ff00 Rd,Rr: movw_ 
: movw,   1 rshift swap        \ R0:1,R2:3,R4:5,..R30:31
          1 rshift swap        \ 0 2 movw, R0:1<--R2:3
          movw_ ;              \ Rd Rr --
9c00 fc00  Rd,Rr: mul,         \ Rd Rr --
0200 ff00  Rd,Rr: muls,        \ Rd Rr --
0300 ff88  Rd,Rr: mulsu,       \ Rd Rr --
0308 ff88  Rd,Rr: fmul,        \ Rd Rr --
0380 ff88  Rd,Rr: fmuls,       \ Rd Rr --
0388 ff88  Rd,Rr: fmulsu,      \ Rd Rr --
0400 fc00  Rd,Rr: cpc,         \ Rd Rr --
0800 fc00  Rd,Rr: sbc,         \ Rd Rr --
0c00 fc00  Rd,Rr: add,         \ Rd Rr --
1000 fc00  Rd,Rr: cpse,        \ Rd Rr --
1400 fc00  Rd,Rr: cp,          \ Rd Rr --
1800 fc00  Rd,Rr: sub,         \ Rd Rr --
1c00 fc00  Rd,Rr: adc,         \ Rd Rr --
2000 fc00  Rd,Rr: and,         \ Rd Rr --
2400 fc00  Rd,Rr: eor,         \ Rd Rr --
2800 fc00  Rd,Rr: or,          \ Rd Rr --
2c00 fc00  Rd,Rr: mov,         \ Rd Rr --

3000 f000  Rd,k: cpi,          \ Rd k --
4000 f000  Rd,k: sbci,         \ Rd k --
5000 f000  Rd,k: subi,         \ Rd k --
6000 f000  Rd,k: ori,          \ Rd k --
: sbr,    ori, ;               \ Rd k --
7000 f000  Rd,k: andi,         \ Rd k --
: cbr,    invert andi, ;
e000 f000  Rd,k: ldi,

 
8000 d200  Rd,Rr+q: ldd, ( Rd Rr q -- ) \ Rr={Z+,Y+}, 2 Y+ 3F ldd,
8200 d200  Rd,Rr+q: std, ( Rr Rd q -- ) \ Rd={Z+,Y+}, Y+ 3F 2 std,

9000 fe00  Rd,Rr: ld,  ( Rd Rr -- ) \ Rr={Z+,-Z,Y+,-Y,X+,-X,X,Y,Z}
9000 fe0f  Rd: lds_
: lds,     swap lds_ i, ;       \ Rd k16 -- )

9004 fe0f  Rd,Rr: lpm,  ( Rd Rr -- ) \ Rr={Z,Z+}, 2 Z+ lpm,
9006 fe0e  Rd,Rr: elpm,  ( Rd Rr -- ) \ Rr={Z,Z+}
9200 fe00  Rd,Rr: st,  ( Rr Rd -- ) \ Rd={Z+,-Z,Y+,-Y,X+,-X,X,Y,Z}

9200 fe0f  Rd: sts_
: sts, sts_ i,  ;        ( k16 Rd -- ) \ FFFF 2 sts, adr(FFFF)<--R2

: lsl,    dup add, ;           \ Rd --
: rol,    dup adc, ;           \ Rd --
: tst,    dup and, ;           \ Rd --
: clr,    dup eor, ;           \ Rd --
: ser,    $ff ldi, ;           \ Rd --

900f fe0f  Rd: pop,            \ Rd --
920f fe0f  Rd: push,           \ Rd --
9400 fe0f  Rd: com,            \ Rd --
9401 fe0f  Rd: neg,            \ Rd --
9402 fe0f  Rd: swap,           \ Rd --
9403 fe0f  Rd: inc,            \ Rd --
9405 fe0f  Rd: asr,            \ Rd --
9406 fe0f  Rd: lsr,            \ Rd --
9407 fe0f  Rd: ror,            \ Rd --
9408 ff8f  Rd: bset,           \ Rd --
9488 ff8f  Rd: bclr,           \ Rd --
940a fe0f  Rd: dec,            \ Rd --

0000 op: nop,                  \ --
9508 op: ret,                  \ --
9518 op: reti,                 \ --
9588 op: sleep,                \ --
9598 op: break,                \ --
95a8 op: wdr,                  \ --
9409 op: ijmp,                 \ --
9419 op: eijmp,                \ --
9509 op: icall,                \ --
9519 op: eicall,               \ --

9488 op: clc,                  \ --
94d8 op: clh,                  \ --
94d8 op: cli,                  \ --
94a8 op: cln,                  \ --
94c8 op: cls,                  \ --
94e8 op: clt,                  \ --
94b8 op: clv,                  \ --
9498 op: clz,                  \ --
9408 op: sec,                  \ --
9458 op: seh,                  \ --
9478 op: sei,                  \ --
9428 op: sen,                  \ --
9448 op: ses,                  \ --
9468 op: set,                  \ --
9438 op: sev,                  \ --
9418 op: sez,                  \ --

9600 ff00  Rw,k: adiw,   ( Rw k6 -- ) \ 3 3F adiw, ZLH=ZLH+#3F
9700 ff00  Rw,k: sbiw,
9800 ff00   P,b: cbi,   \ P b --
9900 ff00   P,b: sbic,  \ P b --
9a00 ff00   P,b: sbi,   \ P b --
9b00 ff00   P,b: sbis,  \ P b --

b000 f800  Rd,P: inn,   \ Rd P --
b800 f800  Rd,P: out,   \ Rr P --

f800 fe08  Rd,Rr: bld,  \ Rd b --
fa00 fe08  Rd,Rr: bst,  \ Rd b --
fc00 fe08  Rd,Rr: sbrc, \ Rd b --
fe00 fe08  Rd,Rr: sbrs, \ Rd b --

940c fe0e   k22: jmp,   ( k16 k6 -- ) \ k6=0 for 16b addr
940e fe0e   k22: call,  ( k16 k6 -- ) \ k6=0 for 16b addr
: rjmp,   c000 f000  mask! i, ; ( k12 -- )
: rcall,  d000 f000  mask! i, ; ( k12 -- )


f008 constant cs,   \ if/until carry set
f008 constant lo,   \ if/until lower
f009 constant eq,   \ if/until zero
f00a constant mi,   \ if/until negative
f00b constant vs,   \ if/until no overflow
f00c constant lt,   \ if/until less than
f00d constant hs,   \ if/until half carry set
f00e constant ts,   \ if/until T flag set
f00f constant ie,   \ if/until interrupt enabled

: not, 0400 xor ;   \ Invert the condition code

: if, ( cc -- addr) i, [ ' if #8 + pfl - zfl d2/ jmp, ] ;
: else, postpone else ;
: then, postpone then ;
: begin, postpone begin ;
: until, ( addr cc -- ) i, postpone again ;
: again, ( addr -- ) postpone again ;

$00 constant Z
$01 constant Z+
$02 constant -Z
$08 constant Y
$09 constant Y+
$0a constant -Y
$0c constant X
$0d constant X+
$0e constant -X

00  constant R0
01  constant R1
02  constant R2
03  constant R3
04  constant R4
05  constant R5
06  constant R6
07  constant R7
08  constant R8
09  constant R9
0a  constant R10
0b  constant R11
0c  constant R12
0d  constant R13
0e  constant R14
0f  constant R15
10  constant R16
11  constant R17
12  constant R18
13  constant R19
14  constant R20
15  constant R21
16  constant R22
17  constant R23
18  constant R24
19  constant R25
1a  constant R26
1b  constant R27
1c  constant R28
1d  constant R29
1e  constant R30
1f  constant R31
1a  constant XL
1b  constant XH
1c  constant YL
1d  constant YH
1e  constant ZL
1f  constant ZH
01  constant XH:XL  \ XH:XL 3F adiw, sbiw,
02  constant YH:YL
03  constant ZH:ZL
