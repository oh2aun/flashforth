single
-bg.terminal
marker -bg.terminal

90 80 80 0 task: bg.task

: bg.terminal
   ['] rx1  'key  !
   ['] rx1? 'key? !
   ['] tx1  'emit !
  decimal
  abort
;

: run.bg.terminal
  ['] bg.terminal bg.task tinit
  bg.task run ;


