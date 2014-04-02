\ *********************************************************************
\ 
\    Filename:       vt100-test.txt
\    Date:           02.03.2014
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
  \cls \c-
  2 8 \cp
  \bri s" *** " type
  \res  \rev s"  Terminal test " type \res
  \bri s"  ***" type
  6 8 \cp \unl s" ^^^^^^^^^^^^^^^^^^^^^^" type
  8 8 \cp \res s"  Press any key to quit! " type
  \res \bri \rev
  0
  begin
   4 #16 \cp bl emit
   dup . 1+  s" sec " type
   #1000 ms
   key?
  until
  key 2drop \res  5 \cdn \nl \c+
;

