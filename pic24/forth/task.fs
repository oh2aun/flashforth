\ *******************************************************************
\                                                                   *
\    Filename:      task.txt                                        *
\    Date:          07.06.2015                                      *
\    FF Version:    5.0                                             *
\    MCU:           PIC 24 30 33                                    *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
\ TASK leaves the userarea address on the stack.
\ The basic size of a task is decimal 26 bytes.
\ The return stack, the parameter stack and the tib buffer areas 
\ are in addition to that. 
\ These are allocated at the end (high address) of the user area.
\ Own user varibles can be allocated from offset 2 upwards,
\ below the return stack. Addsize must reflect any additonal
\ user variables that are used.
\ uareasize = 32 + rsize + tibsize + ssize + addsize
\
\ The operator task is predefined.
\ flash decimal 72 72 72 0 task: operator
\ 
\ A background task with a 12 cell return stack and a 
\ 12 cell parameter stack and no tib.
\ flash decimal 0 24 24 0  task: bg1
\
\ A background task with also one extra user variable.
\ flash decimal 0 24 24 2  task: bg2
\ ram decimal 2 user bg2_cnt

\ Do not use user variables as task specific variables
\ User variables are needed by _words_common_to_several_tasks_
\ which need some task specific data storage.

-task
marker -task
hex ram

\ Near definition saves memory !
: up! up ! ;
: up@ up @ ;
: op@ operator @ ;
: ul@ ulink @ ;
: ul! ulink ! ;
: op! op@ up! ;
: cell+ 2+ ; inlined
\ access user variables of other task
: his ( task-addr var-addr -- addr )
  up@ - swap @ + 
;

\ Define a new task
\ A new task must be defined in the flash memory space
: task: ( tibsize stacksize rsize addsize -- )
  flash create 
  up@ s0 - dup          \ Basic size     ts ss rs as bs bs
  ram here flash + ,    \ User pointer   ts ss rs as bs
  4 for
    over , +
  next
  cell+                 \ Task size
  ram allot
;

\ Initialise a user area and link it to the task loop
\ May only be executed from the operator task
: tinit ( taskloop-addr task-addr -- )
  \ use task user area
  @+ up!                          \ a addsize-addr
  ul@ if                 \ ? Already running
    2drop
  else
    \ Pointer to task area
    dup 2- task ! 0 task 2+ !
    \ rsave = uarea+cell+addsize+cell
    @+ up@ + cell+ rot >xa over
                            \ rsize-addr rbuf task-adr rbuf
    \ Store taskloop-addr in the return stack
    !                 \ rsize-addr rbuf
    0 over cell+ !    \ store the upper word
    \ Save r0
    dup s0 cell+ !            \ rsize-addr rbuf
    \ Point to  free returns stack cell
    cell+ cell+ rsave !       \ rsize-addr
    \ s0 = ssave = rsave + rsize 
    @+ rsave @ + dup dup s0 ! ssave ! \ ssize-addr s0
    swap @ + tiu !
    0 ul!
    0 task 2+ !        \ set the cr flag
    decimal            \ Set the base to decimal
  then
  op!                \ run the operator task again
;

\ Insert a new task after operator in the linked list.
\ May only be executed from the operator task
: run ( task-addr -- )
  @ up! ul@ 0=                 \ Already running ?
  if
    up@                        \ task-uarea
    op! ul@                    \ task-uarea operator-ulink
    over ul!      
    swap up! ul! 
  then
  op!                          \ run operator task
;

\ End a task by linking it out from the linked list
\ May only be executed from the operator task
: end ( task-addr -- )  
  @ up! ul@
  if
    up@
    op!
    begin                   \ find the uarea in the linked list
      dup ul@ <>            \ uarea flag
    while
      ul@ up!               \ uarea
    repeat
    up@                     \ uarea prev-uarea
    swap up!                \ prev-uarea
    ul@                     \ prev-uarea next-uarea
    0 ul!                   \ ulink of a ended task is zero
    swap up!                \ next-uarea
    ul!                     \ 
  then
  op!
;

\ End all tasks except the operator task
\ May only be executed from the operator task
: single ( -- )
  ul@ op@ <>                \ Are there any running tasks
  if 
    ul@ op@ ul!             \ link operator to himself
    up!                     \ move to next user area
    begin
      ul@ op@ <>            \ is this the last linked user area
    while
      ul@ 0 ul!             \ write zero to ulink
      up!                   \ and move to next user area
    repeat
    0 ul!
    op!
  then
;

\ List all running tasks
: tasks ( -- )
  up@ op!
  begin
    up@ 
    task @ 6 - op! c>n .id space
    up!
    ul@ op@ -
  while
    ul@ up!
  repeat
  up!
;

