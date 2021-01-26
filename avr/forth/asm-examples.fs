\ needs asm.txt

-asmexamples
marker -asmexamples

\ Top of stack is always cached in R24:R25

\ duplicate top of stack
\ identical to DUP on FlashForth
: _dup ( n -- n n )
  [ R25 -Y st, ]
  [ R24 -Y st, ]
; inlined

\ drop top of stack
\ identical to DROP on FlashForth
: _drop ( n -- )
  [ R24 Y+ ld, ]
  [ R25 Y+ ld, ]
; inlined

\ Load constant $1234 to top of stack  
: a-number ( -- 1234 )
  dup                  \ Make space for new TOS value
  [ R24 $34 ldi, ]
  [ R25 $12 ldi, ]
;

\ Pop the top of stack to registers R18:R19
\ R18 and R19 are free to use unless DO..LOOP is used
: tos-to-r18-r19 ( n -- )
  [ R18 R24 movw, ]  \ Move TOS to R18:R19
  drop               \ load R24:R25 with new TOS
;


