\ *********************************************************************
\    Assembler for FlashForth                                         *
\    Filename:      asm.txt                                           *
\    Date:          31.12.2017                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ 1.10.2007  Totally rewritten as more normal Forth prefix assembler
\ The structured assembler is in the FlashForth core,
\ so normal conditionals are commented away.
\ 
\ If you are short of memory, just compile
\ the assembler parts you really need.

\ Since FlashForth refuses to redefine words,
\ oneline definitions may safely be copied to the
\ file that uses the assembler defintions. 

-as
marker -as
hex ram

\ microchip assembler arguments
\ f = file
\ d = destination
\ a = access
\ b = bit
\ k = literal

0 constant w,     \ Destination W register
1 constant f,     \ Destination File
0 constant a,     \ Force Access Bank
1 constant b,     \ Force Bank Select Register

\ registers for parameter stack handling
$0000 constant Sreg     \ For lfsr
$ffed constant (sp-)
$ffec constant (+sp)

\ Temporary pointer
$0001 constant Treg     \ For lfsr
$ffe5 constant (tp-)
$ffe4 constant (+tp)

: as0 ( opcode "name" -- ) ( -- )
  co:
  does> i, ;

: as2 ( opcode "name" -- ) ( f a -- )
  co:
  does> rot ic, or ic, ;

\ Not needed for structured conditionals
\ : br1 ( opcode "name" -- ) ( rel-addr -- ) \ bcc
\   co:
\   does> swap $1ff br? or i, ;

$24 as3 addwf,          ( f d a -- )  
$20 as3 addwfc,         ( f d a -- )  
$14 as3 andwf,          ( f d a -- )  
$6a as2 clrf,           ( f a -- )  
$1c as3 comf,           ( f d a -- )  
$62 as2 cpfseq,         ( f a -- )  
$64 as2 cpfsgt,         ( f a -- )  
$60 as2 cpfslt,         ( f a -- )  
$04 as3 decf,           ( f d a -- )  
$2c as3 decfsz,         ( f d a -- )  
$4c as3 dcfsnz,         ( f d a -- )  
$28 as3 incf,           ( f d a -- )  
$3c as3 incfsz,         ( f d a -- )  
$48 as3 infsnz,         ( f d a -- )  
$10 as3 iorwf,          ( f d a -- )  
$50 as3 movf,           ( f d a -- )  
: movff,                ( fs fd -- )
  swap $0fff and $c000 or i, $0fff and $f000 or i, ;    
$6e as2 movwf,          ( f a -- )    
$02 as2 mulwf,          ( f a -- )    
$6c as2 negf,           ( f a -- )    
$34 as3 rlcf,           ( f d a -- )  
$44 as3 rlncf,          ( f d a -- )  
$30 as3 rrcf,           ( f d a -- )  
$40 as3 rrncf,          ( f d a -- )  
$68 as2 setf,           ( f d a -- )  
$54 as3 subfwb,         ( f d a -- )  
$5c as3 subwf,          ( f d a -- )  
$58 as3 subwfb,         ( f d a -- )  
$38 as3 swapf,          ( f d a -- )  
$66 as2 tstfsz,         ( f a -- )    
$18 as3 xorwf,          ( f d a -- )  

\ **************************************
\ bit-oriented file register operations
\ **************************************
$90 as3 bcf,            ( f b a -- )  
$80 as3 bsf,            ( f b a -- )  
$b0 as3 btfsc,          ( f b a -- )  
$a0 as3 btfss,          ( f b a -- )  
$70 as3 btg,            ( f b a -- )  
\ **************************************
\ literal operations 
\ **************************************
$0f00 as1 addlw,        ( k -- )  
$0d00 as1 andlw,        ( k -- )  
$0900 as1 iorlw,        ( k -- )  
: lfsr,    ( k f -- )
  4 lshift over 8 rshift $f and or $ee00 or i, $ff and $f000 or i, ;  
$0100 as1 movlb,        ( k -- )  
$0e00 as1 movlw,        ( k -- )  
$0d00 as1 mullw,        ( k -- )  
$0800 as1 sublw,        ( k -- )  
$0a00 as1 xorlw,        ( k -- )

$d000 br2 bra,          ( n -- )  
$ec00 br3 call,         ( n -- )  
$0004 as0 clrwdt,       ( -- )    
$0007 as0 daw,          ( -- )    
$ef00 br3 goto,         ( n -- )  
$0006 as0 pop,          ( -- )    
$0005 as0 push,         ( -- )    
$d800 br2 rcall,        ( n -- )  
$00ff as0 reset,        ( -- )    
$0010 as0 retfie,       ( -- )    
$0c00 as1 retlw,        ( k -- )  
$0012 as0 return,       ( -- )    
$0003 as0 sleep,        ( -- )    
$f000 as0 nop,          ( -- )    
\ **************************************
\ data memory <-> program memory operations 
\ **************************************
$0008 as0 tblrd*,       ( -- )  
$0009 as0 tblrd*+,      ( -- )  
$000a as0 tblrd*-,      ( -- )  
$000b as0 tblrd+*,      ( -- )  
$000c as0 tblwt*,       ( -- )  
$000d as0 tblwt*+,      ( -- )  
$000e as0 tblwt*-,      ( -- )  
$000f as0 tblwt+*,      ( -- )  

\ ***********************************************
\ structured conditions for  if, while, and until,
\ ************************************************
$e2 constant cc,    \ bc
$e6 constant mi,    \ bn
$e0 constant z,     \ bz
$e4 constant ov,    \ bov
: not, 1 xor ;      \ Invert condition

\ **************************************
\ control operations
\ **************************************
\ NOT needed, use structured conditonals instead
\ $e200 br1 bc,         ( n -- )  
\ $e600 br1 bn,         ( n -- )  
\ $e300 br1 bnc,        ( n -- )  
\ $e700 br1 bnn,        ( n -- )  
\ $e500 br1 bnov,       ( n -- )  
\ $e000 br1 bz,         ( n -- )  
\ $e100 br1 bnz,        ( n -- )  
\ $e400 br1 bov,        ( n -- )  

