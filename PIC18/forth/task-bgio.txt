\ *********************************************************************
\                                                                     *
\    Filename:      task-bgio.txt                                     *
\    Date:          06.01.2014                                        *
\    FF Version:    5.0                                               *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\
\ Test I/O redirection in a background task.
\ Requires 1 USB-CDC serial connection and 1 UART connection in FF5.0
\ Compilation, write to flash/eeprom is only allowed
\ in the operator task. 
\ QUIT and INTERPRET do not work in a background task.
\ ACCEPT works in a background task.
-bgio
marker -bgio
ram hex

: bgio_loop
  decimal
  ['] rx1 'key !
  ['] tx1 'emit !
  ['] rx1? 'key? !
  cr ." Starting BG task test !" cr
  begin
    tib dup $20 accept type cr
  again
;

\ Define the task area 
\ tibSize paramStackSize returnStackSize userAreaAdditionalSize --
$20 $20 $20 $0 task: bgio_task


\ initalise and run the BG task
: bgio
  ['] bgio_loop bgio_task tinit
  bgio_task run
;

bgio
