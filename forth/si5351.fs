\ *********************************************************************
\                                                                     *
\    Filename:      si5351.fs                                         *
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
\ Does not work on Atmega since qmath is not (yet) available
\ needs i2c-base.fs qmath.fs(PIC18) 
-si5351
marker -si5351
decimal

\ i2c device address
$60 constant si5351

: si5351.c! ( c reg -- )
  si5351 i2c.write i2c.c! i2c.c! i2c.stop ;

: si5351.! ( n reg -- )
  si5351 i2c.write i2c.c! dup 8 rshift i2c.c! i2c.c! i2c.stop ; 

: si5351.c@ ( reg -- c )
  si5351 i2c.write i2c.c! i2c.stop 
  si5351 i2c.read i2c.c@.nack i2c.stop ;

: si5351.dump
  0
  #40 for
    dup #10 u/mod drop 0= if cr dup 3 decimal u.r [char] : emit then  
    dup si5351.c@ hex 2 u.r 1+
  next cr drop ;



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

eeprom 2variable clock
flash 2variable clock 
ram
variable mult
2variable num
#1048575. 2constant denom
#14 constant divider
2variable P1
2variable P2

:  pll! ( reg -- )
   >r
   num 2@ #128 ud* d>q denom uq/mod
   #128 mult @ um* d+ #512. d- P1 2! 2drop 

   num 2@ #128 ud*
   num 2@ #128 ud* d>q denom uq/mod denom uq* 2drop d- P2 2! 2drop

   denom swap r@ si5351.! drop
   P1 2@ r@ 2 + si5351.c! r@ 3 +  si5351.!
   P2 2@ $f0 or r@ 5 + si5351.c! r@ 6 + si5351.!
   rdrop
;

: msynt! 
  >r
  #128 divider * #512 - P1 !
  1 r@ si5351.!
  00 r@ 2 + si5351.c!
  P1 @ r@ 3 + si5351.!
  0 r@ 5 + si5351.c!
  0 r@ 6 + si5351.!
  rdrop
;

: si5351.init
  #25.000.000. clock 2!
  SI_SYNTH_MS_1   msynt!
  $a0 SI_PLL_RESET si5351.c!
  $4c SI_CLK_SRC_PLLA or SI_CLK1_CONTROL si5351.c!
;

\ 45-65 MHz output, multisynth divider is 14. vco 630-910
: si5351.freq ( ud -- )
  divider ud* d>q clock 2@ uq/mod drop mult !
  denom uq* clock 2@ uq/mod num 2! 2drop
  SI_SYNTH_PLL_A  pll!
;

