\ Inductance meter
\ PIC18F25K50
\ needs qmath.fs dogs164.fs task.fs
-l-meter
marker -l-meter

\ PIC18f25K50
$ffb1 constant t3con
$ffb2 constant tmr3
$ffb4 constant t3gcon
$ffba constant t2con
$ffbb constant pr2

eeprom 2variable cal ram
#575.000.000.  cal 2!

\ 12 MHz Fcy.
\ Use timer 2 to gate timer3 that counts oscillator pulses.
: l.init
  #249 pr2 c!
  7 t2con c!
  %1000.0111 t3con c!
  %1011.0001 t3gcon c!
;

: l.freq ( -- ud ) \ Frequency in kiloherz
  0 tmr3 !
  %1011.1001 t3gcon c!
  2 ms tmr3 @  3 um*
;
: nup ( x1 x2 x3 -- x2 x3 ) rot drop ;

\ Colpitts oscillator with 44.1 pF parallell capacitance
: l.inductance ( -- d ) \ Inductance in nanoHenries.
  cal 2@ 1000. uq* 
  l.freq uq/mod nup nup
  d>q l.freq uq/mod nup nup
;
: l.format ( d -- )
  dogs.home
  ['] dogs.data 'emit !
  ." L="
  <# # # # [char] . hold # # # # #> type 
  ."  uH"
  l.freq
  0 2 dogs.cursor
  ." F=" d. ."  KHz   "
  ['] txu 'emit !
;
: l.display
  dogs.init l.init decimal
  begin
    l.inductance l.format
    #1000 ms
  again
;
$10 $40 $40 0 task: l.task
: l.start ['] l.display l.task tinit l.task run ;
' l.start to turnkey

