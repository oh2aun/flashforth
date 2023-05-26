\ *********************************************************************
\                                                                     *
\    Filename:      si5351-c.fs                                         *
\    Date:          06.10.2022                                        *
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
  si5351 i2c.write i2c.c!
  si5351 i2c.read i2c.c@.nack ;

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

eeprom 2variable clock
#25.002.388. clock 2!
ram
#14 value divider

variable mult
2variable num
#1048575. 2constant denom
2variable P1
2variable P2

:  pll! ( reg_base -- )
   >r
   num 2@ #128 ud* d>q denom uq/mod
   #128 mult @ um* d+ #512. d- P1 2! 2drop 

   num 2@ #128 ud*
   num 2@ #128 ud* d>q denom uq/mod denom uq* 2drop d- P2 2! 2drop

   $ffff r@ si5351.!
   P1 2@ r@ 2 + si5351.c!!
   P2 2@ $f0 or r@ 5 + si5351.c!!
   rdrop
;

: msynt! ( reg_base -- ) \ b = 1, c = $ffffff, a = divider
  >r
  1 r@ si5351.!  \ P3(15:0)
  0 r@ 2 + si5351.c!  \ P1(17:16)
  #128 divider * #512 - r@ 3 + si5351.! \ P1(15:0)
  0. r@ 5 + si5351.c!! \ P3(19:16)P2(19:0)
  rdrop
;

\ Use CLK2 and CLK1 with 180 deg phase shift as push pull
\ via a 4:1 wideband transformer. Iout=4mA -> output 11 dBm
: a.init ( u -- )
  to divider
  %1111.1001 SI_OE_CONTROL si5351.c!  \ clk1 and clk2
  SI_SYNTH_MS_2 msynt!
  SI_SYNTH_MS_1 msynt!
  $5d SI_CLK_SRC_PLLA or SI_CLK2_CONTROL si5351.c! \ Inverted output
  $4d SI_CLK_SRC_PLLA or SI_CLK1_CONTROL si5351.c! \ Normal output
  $cc SI_CLK_SRC_PLLB or SI_CLK0_CONTROL si5351.c! \ Power down
  $a0 SI_PLL_RESET si5351.c!
;

\ AM VFO 45-67 MHz output, multisynth divider is 16-24. vco 1100 MHz
: a.f ( ud -- )
  divider ud* d>q clock 2@ uq/mod drop mult !
  denom uq* clock 2@ uq/mod num 2! 2drop
  SI_SYNTH_PLL_A  pll!
;

\ FM vfo. 76.8 - 97.3 MHz
: b.init
  #12 to divider
  %1111.1110 SI_OE_CONTROL si5351.c!   \ clk0
  SI_SYNTH_MS_0 msynt!
  $dd SI_CLK_SRC_PLLA or SI_CLK2_CONTROL si5351.c! \ Power down
  $cd SI_CLK_SRC_PLLA or SI_CLK1_CONTROL si5351.c! \ Power down
  $4c SI_CLK_SRC_PLLB or SI_CLK0_CONTROL si5351.c!
  $a0 SI_PLL_RESET si5351.c!
;
: b.f ( ud -- )
  divider ud* d>q clock 2@ uq/mod drop mult !
  denom uq* clock 2@ uq/mod num 2! 2drop
  SI_SYNTH_PLL_B  pll!
;

