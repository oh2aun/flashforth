\ needs avr/forth/task.txt
-task-dump
marker -task-dump
hex ram

: his@ his @ ;
: word. dup if c>n .id else . then ;
: task-dump ( task-addr -- )
  hex >r
  cr ." up:" r@ @ u.
  cr ." ulink:" r@ ulink his@ u.
  cr ." rsave:" r@ rsave his@ u.
  cr ." hp:" r@ hp his@ u.
  cr ." rp:" r@ @ @ u.
  cr ." r0:" r@ r0 his@ 1+ dup u. r@ 4 + @ - r@ 4 + @ dump
  cr ." s0:" r@ s0 his@ 1+ dup u. r@ 6 + @ - r@ 6 + @ dump
  cr ." tib:" r@ tiu his @ u.
  cr ." 'key:" r@ 'key his@ word.
  cr ." 'key?:" r@ 'key? his@ word.
  cr ." 'emit:" r@ 'emit his@ word.
  cr ." base:" r@ base his@ u.
  r> drop cr
;