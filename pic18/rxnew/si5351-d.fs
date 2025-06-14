\ *********************************************************************
\                                                                     *
\    Filename:      si5351-d.fs                                       *
\    Date:          22.10.2023                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************

\ ******************************************
\ si5351 i2c words for a 45 MHz - 65 MHz VFO
\ ******************************************
\ For PIC18F26K42 and other chips with the same I2C peripheral.
\ needs i2c-base-b.fs qmath.fs 
fl+
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
  
: si5351.dump ( n -- )
  0 swap
  for
    dup #10 u/mod drop 0= if cr dup 3 decimal u.r [char] : emit then  
    dup si5351.c@ hex 2 u.r 1+
  next cr drop decimal ;


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

eeprom
$4c value clk2
$4c value clk1

ram
#14 value divider
#16 value clk0_divider

variable mult
2variable num
$f.ffff. 2constant denom

:  pll! ( reg_base -- )
   >r
   num 2@ #128 ud* 2dup 2dup >r >r
   d>q denom uq/mod rot drop rot drop
   denom uq* 2drop d- \ P2
   $f0 or             \ P3.19:16
   r> r> d>q denom uq/mod rot drop rot drop
   #128 mult @ um* d+ #512. d- \ P1

   r> 2+ si5351.pll!
;

: msynt! ( divider reg_base -- ) \ b = 1, c = $ffffff, a = divider
  >r
  1 r@ si5351.!  \ P3(15:0)
  0 r@ 2+ si5351.c!  \ P1(17:16)
  ( divider ) #128 * #512 - r@ 3 + si5351.! \ P1(15:0)
  0. r> 5 + si5351.c!! \ P3(19:16)P2(19:0)
;

\ FM vfo. 76.8 - 97.3 MHz, AM fixed 44.545 MHz
: fm.init
  #8 to clk0_divider
  %1111.1101 SI_OE_CONTROL si5351.c!   \ clk0 on
  clk0_divider SI_SYNTH_MS_1 msynt!
  $cc SI_CLK_SRC_PLLA or SI_CLK2_CONTROL si5351.c! \ Power down
  $4c SI_CLK_SRC_PLLB or SI_CLK1_CONTROL si5351.c! 
  $cc SI_CLK_SRC_PLLB or SI_CLK0_CONTROL si5351.c! \ Power down
  $ffff SI_SYNTH_PLL_B si5351.!
  $a0 SI_PLL_RESET si5351.c!
;
: fm.f ( ud -- )
  clk0_divider ud* d>q xtal uq/mod drop mult !
  denom uq* xtal uq/mod num 2! 2drop
  SI_SYNTH_PLL_B  pll!
;

\ Use CLK2 and CLK1 with 180 deg phase shift as push pull
\ via a 4:1 wideband transformer. Iout=4mA -> output 11 dBm
: am.init ( u -- )
  to divider
  %1111.1010 SI_OE_CONTROL si5351.c!  \ clk0, clk1, clk2 on 
  divider SI_SYNTH_MS_2 msynt!
  clk2 SI_CLK_SRC_PLLA or SI_CLK2_CONTROL si5351.c! \ Normal output
  $ec  SI_CLK_SRC_PLLB or SI_CLK1_CONTROL si5351.c! \ Off
  $ffff SI_SYNTH_PLL_A si5351.!
  
  #16 to clk0_divider
  clk0_divider SI_SYNTH_MS_0 msynt!
  $4c SI_CLK_SRC_PLLB or SI_CLK0_CONTROL si5351.c! \ Normal output
  $ffff SI_SYNTH_PLL_B si5351.!
  #44.545.000. fm.f

  $a0 SI_PLL_RESET si5351.c!
;

\ AM VFO 45-67 MHz output, multisynth divider is 14-20. vco 900 MHz
: am.f ( ud -- )
  divider ud* d>q xtal uq/mod drop mult !
  denom uq* xtal uq/mod num 2! 2drop
  SI_SYNTH_PLL_A  pll!
;

