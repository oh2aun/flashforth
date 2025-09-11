\ *********************************************************************
\ 
\    Filename:       vt100-test.txt
\    Date:           02.12.2018
\    FF Version:     5.0 
\    Author:         Attila Herman
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Frequently used commands for VT100 compatible terminals
\ Unfortunately there is some differencies between terminal emulators!
\ Maybe some words doesn't work, or does another function depending on
\ terminal.

-vt100-test
marker -vt100-test

: vt100-test
  \cls \c+
  2 8 \cp
  \bri ." *** "
  \res  \rev ."  Terminal test " \res
  \bri ."  ***"
  6 8 \cp \unl ." ^^^^^^^^^^^^^^^^^^^^^^"
  8 8 \cp \res ." Press any key to quit! "
  \res
  0
  begin
   \bri \rev 
   4 #14 \cp bl emit
   dup 5 u.r 1+ ." sec "
   \res 
   #1000 ms
   key?
  until
  key 2drop \res  5 \cdn \nl \c+
;

