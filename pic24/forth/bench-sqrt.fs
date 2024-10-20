( 16-bit Newton Raphson Fixed-Point Testing )
-sqrt
marker -sqrt
decimal
( Fixed-Point Divide, in Q7.8 format )
: fpmul ( n1 n2 -- n3 ) 256 */ ;
: fpdiv ( n1 n2 -- n3 ) 256 swap */ ;

( fpadd and fpsub are the same as for integers )


: fpsqrt ( n1 -- n2 ) 
  dup 1 rshift 4 for over over fpdiv + 128 fpmul next swap drop ;

( Or fully expanded )
 : fpsqrt-opt ( n1 -- n2 )
  dup 1 inline rshift 
    4 for over over 256 inline swap inline */ + 128 256 inline */
  next inline swap drop ;

( Generic test loop

( Bench with timing for ESP32forth )
: bench ( iters -- ) 
  ticks swap for 512 fpsqrt drop next 
  ticks swap - . ." ms" ;
  
: bench-opt ( iters -- ) 
  ticks swap for 512 fpsqrt-opt drop next 
  ticks swap - . ." ms" ;


\ ================ Results ================
\ ESP32forth on ESP32: 82,781 iters/s (604 ms for 50,000 iters)
\ ESP32forth on ESP32-C3: 55,555 iters/s (450 ms for 25,000 iters)
\ ESP32forth on ESP32-C3 (optimised): 60,240 iters/s (415 ms for 25,000 iters)
\ Mecrisp-stellaris on FRDM-MKL25Z128V: 2,512 iters/s (397.973 ms for 1,000 iters)
\ Swapforth on J1a (icestick): ~6,500 iters/s

\ FlashForth on dsPIC33@64MHz using */ :  84000 iters/s
\ FlashForth on Udamonic Scamp PIC24@32MHz using */ : 42400 iter/s
\ FlashForth on Arduino UNO Atmega328@16MHz using u/*mod : 4435 iters/s
\ FlashForth on PIC18@48MHz using u/*mod : 3249 iter/s 
\ FlashForth on dsPIC33CK@200MHz : 260163 iter/s
\ FlashForth on dsPIC33CK@200MHz optimized 504931 iter/s

\ Here are the numbers normalized to 21 MHz.
\ FlashForth on dsPIC33 : 27562 iters/s
\ FlashForth on Udamonic Scamp PIC24 : 27825 iter/s
\ FlashForth on Udamonic Scamp PIC24 optimized : 37866 iter/s
\ FlashForth on Arduino UNO Atmega328 : 5820 iters/s
\ FlashForth on PIC18 : 1421 iter/s
\ FlashForth on dsPIC33CK@21MHz : 27317 iters/s
\ FlashForth on dsPIC33CK@21MHz optimized : 53017 iters/s

