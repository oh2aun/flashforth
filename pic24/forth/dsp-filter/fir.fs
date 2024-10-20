\ dsPIC33FJ128GP802
-fir
marker -fir
decimal

 #127 constant numTaps

$2700 constant tapsBase    \ X memory
$2800 constant delayBase   \ Y memory

ram create firContext #14 allot

: initFilter ( filterTaps -- )
  tapsBase numTaps wmove
  
  numTaps             firContext !
  tapsBase            firContext 2 + !
  tapsBase numTaps 2* +  firContext 4 + !
  $ff00               firContext 6 + ! \ taps in Y RAM
  delayBase           firContext 8 + !
  delayBase numTaps 2* + firContext #10 + !
  delayBase           firContext #12 + !
  delayBase numTaps 2* erase
;

