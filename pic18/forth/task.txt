\ *********************************************************************
\                                                                     *
\    Filename:      task.txt                                          *
\    Date:          01.01.2018                                        *
\    FF Version:    5.0                                               *
\    MCU:           PIC18                                             *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ TASK leaves the userarea address on the stack.
\ The basic size of a task is decimal 36 bytes.
\ The return stack, the parameter stack and the tib buffer areas 
\ are in addition to that. 
\ These are allocated at the end (high address) of the user area.
\ Own user varibles can be allocated from offset 2 upwards,
\ below the return stack. Addsize must reflect any additonal
\ user variables that are used.
\ The end of TIB is used as the HOLD area. When numeric output is
\ used, the tibsize should be at least 10.
\ QUIT accepts tibsize-10 characters. 
\
\ uareasize = 36 + rsize + tibsize + ssize + addsize
\
\ The operator task is predefined.
\ flash decimal 72 62 96 0 task operator
\ 
\ A background task with a 12 cell return stack and a 
\ 12 cell parameter stack and no tib.
\ flash decimal 0 24 24 0  task bg1
\
\ A background task with also one extra user variable.
\ flash decimal 0 24 24 2  task bg2
\ ram decimal 2 user bg2_cnt

\ Do not use user variables as task specific variables
\ User variables are needed by _words_common_to_several_tasks_
\ which need some task specific data storage.
single
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

\ access user variables of other task
: his \ task-addr var-addr -- addr 
  up@ - swap @ + 
;

\ Define a new task
\ A new task must be defined in the flash memory space
: task: \ tibsize stacksize rsize addsize --
  flash create 
  up@ s0 - dup          \ Basic size     ts ss rs as bs bs
  ram here flash
  + ,                   \ User pointer   ts ss rs as bs
  4 for
    over , +
  next
  cell+                 \ Task size
  ram allot             \ The user area is in ram
;

\ Initialise a user area and link it to the task loop
\ May only be executed from the operator task
: tinit \ taskloop-addr task-addr -- 
  \ use task user area
  @+ up!                            \ a t+2
  ul@ if 
    2drop                           \ Already running
  else
    dup 2- task !        \ Pointer to task area
    \ save area = uarea+cell+addsize
    @+ up@ + cell+ !p>r         \ a t+3
    \ s0 = rsave + ssize - 1 
    @+ @p + 1- dup s0 !         \ a t+4
    p! p2+                      \ s0 to save area
    p2+                         \ dummy P to save area
    \ tib = rsave + rsize
    @ s0 @ + 1+ tiu !           \ a
    p! p2+         \ taskloop-addr to save area
    1 pc!          \ return stack size to save area
    @p up@ !       \ end of save area to restore pointer
    false ul!      \ no link yet
    decimal        \ Set the base to decimal
    r>p
  then
  op!                     \ run the operator task
;

\ Insert a new task after operator in the linked list.
\ May only be executed from the operator task
\ task-addr -- 
: run
  @ up! ul@ 0= if    \ t Already running ?
    up@              \ t t-up
    op! ul@          \ o t-up o-ul
    over up! dup     \ t t-up o-ul o-ul
    if   ul!
    else drop op@ ul!
    then op! ul!
  then
  op!                \ run operator task
;

\ End a task by linking it out from the linked list
\ May only be executed from the operator task
: end ( task-addr -- )  
  @ up! ul@ if
    ul@ up@             \ t t-ul t-up
    false ul!
    op!                 \ o t-ul t-up
    begin               \ find the uarea in the linked list
      dup ul@ <>        \ t t-ul t-up flag
    while
      ul@ up!           \ t' t-ul t-up
    repeat
    drop dup op@ =
    if   drop false     \ Only operator task left
    then ul!
  then
  op!
;

\ End all tasks except the operator task
\ May only be executed from the operator task
: single ( -- )
  begin
    ul@            \ is this the last linked user area
  while
    ul@ false ul!  \ write zero to ulink
    up!            \ and move to next user area
  repeat
  op!
;

\ List all running tasks
: tasks ( -- )
  begin
    up@
      task @ 6 - c>n
      op! .id space
    up!
    ul@ op@ <> ul@ and
  while
    ul@ up!
  repeat
  op!
;

hex ram
