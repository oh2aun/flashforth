\ *********************************************************************
\                                                                     *
\    Filename:      pll.fs                                            *
\    Date:          11.02.2025                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

\ ******************************************
\ si5351 i2c words for a 45 MHz - 65 MHz VFO
\ ******************************************
\ Does not work on Atmega since qmath is not (yet) available
\ needs i2c-base.fs qmath.fs(PIC18) 
-si5351
marker -si5351
decimal

\ i2c device address
$60 constant si5351

: si5351.c! ( c reg -- )
  si5351 i2c.write i2c.2c! ;

: si5351.! ( n reg -- )
  si5351 i2c.write i2c.c!!  ; 

: si5351.c!! ( n c reg -- )
  si5351 i2c.write i2c.c!c!!  ; 

: si5351.c@ ( reg -- c )
  si5351 i2c.rc@ ;

: si5351.pll! ( n c n c reg -- )
  si5351 i2c.write
  7 i2c1cnt c! i2c1txb c! i2c.start 
  i2c.txbe i2c1txb c! i2c.txbe dup 8 rshift i2c1txb c! i2c.txbe i2c1txb c!
  i2c.txbe i2c1txb c! i2c.txbe dup 8 rshift i2c1txb c! i2c.txbe i2c1txb c!
  i2c.stop 
;

#03 constant SI_OE_CONTROL
#16 constant SI_CLK0_CONTROL			\ Register definitions
#17 constant SI_CLK1_CONTROL
#18 constant SI_CLK2_CONTROL
#26 constant SI_SYNTH_PLL_A
#34 constant SI_SYNTH_PLL_B
#42 constant SI_SYNTH_MS_0
#50 constant SI_SYNTH_MS_1
#58 constant SI_SYNTH_MS_2
#177 constant SI_PLL_RESET
#0 constant SI_R_DIV_1
#0 constant SI_CLK_SRC_PLLA
$20 constant SI_CLK_SRC_PLLB
$10 constant SI_CLK_INVERT

eeprom 2variable clock
#27.001.575. clock 2!
#14 value divider
$4c value lo1-level
ram
2variable lo2-freq
2variable pllb-freq
2variable bfo-freq

variable mult
2variable num
#1.048.575. 2constant denom
2variable P1
2variable P2

:  pll! ( reg -- )
   >r
   num 2@ #128 ud* d>q denom uq/mod
   #128 mult @ um* d+ #512. d- P1 2! 2drop 

   num 2@ #128 ud*
   num 2@ #128 ud* d>q denom uq/mod denom uq* 2drop d- P2 2! 2drop

   denom drop r@ si5351.!
   P1 2@ r@ 2 + si5351.c! r@ 3 +  si5351.!
   P2 2@ $f and denom nip 4 lshift or r@ 5 + si5351.c! 
   r> 6 + si5351.!
;

: msynt-int! ( req -- )
  >r
  #128 divider * #512 - P1 !
  1 r@ si5351.!
  00 r@ 2 + si5351.c!
  P1 @ r@ 3 + si5351.!
  0 r@ 5 + si5351.c!
  0 r@ 6 + si5351.!
  rdrop
;

: msynt-frac! ( reg -- )  pll! ;

\ direct coupling (2mA=6dBm)
: lo.init
  %1111.1000 SI_OE_CONTROL si5351.c! 
  SI_SYNTH_MS_0 msynt-int!
  SI_SYNTH_MS_2 msynt-int!
  $4c SI_CLK_SRC_PLLB or SI_CLK2_CONTROL si5351.c! \ LO2 44.545 MHz
  $4c SI_CLK_SRC_PLLB or SI_CLK1_CONTROL si5351.c! \ BFO 453.5 & 456.5 KHz
  lo1-level SI_CLK_SRC_PLLA or SI_CLK0_CONTROL si5351.c! \ LO1 45-67 MHz
  $a0 SI_PLL_RESET si5351.c!
;

\ 45-67 MHz output, multisynth divider is 14. vco 630-910
: lo1 ( ud -- )
  divider ud* d>q clock 2@ uq/mod drop mult !
  denom uq* clock 2@ uq/mod num 2! 2drop
  SI_SYNTH_PLL_A  pll!
;
\  44545 KHz 2nd LO PLL B fixed frequency
: lo2 ( -- )
  lo2-freq 2@ divider ud* 2dup pllb-freq 2!
  d>q clock 2@ uq/mod drop mult !
  denom uq* clock 2@ uq/mod num 2! 2drop
  SI_SYNTH_PLL_B  pll!
;
\ 455 KHz BFO uses multisynth fractional division from PLL B
: bfo ( ud -- )
  bfo-freq 2! pllb-freq 2@ d>q bfo-freq 2@ uq/mod drop mult !
  denom uq* bfo-freq 2@ uq/mod num 2! 2drop
  SI_SYNTH_MS_1 msynt-frac!
;
: bfo-off $ec $11 si5351.c! ;
: bfo-on  $6c $11 si5351.c! ;

