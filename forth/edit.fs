\ Experimental code to for a block editor
\ Requires a vt100 capable terminal program, e.g. minicom
\ #sendm forth/core forth/case forth/vt100 xxxx/forth/block forth/edit
-edit
marker -edit
decimal

#08 constant k-bs      \ Backspace
#09 constant k-ins     \ C-i
#06 constant k-flush   \ C-f
#03 constant k-exit    \ C-c
#27 constant k-esc      \ esc
#25 constant k-ins-line \ C-y
#11 constant k-del-line \ C-k
#13 constant k-crlf     \ enter C-m
#02 constant k-sl       \ C-b to beginning of line
#05 constant k-el       \ C-e to end of line


\ leftarrow  27 91 68
\ rightarrow 27 91 67
\ uparrow    27 91 65
\ downarrow  27 91 66
\ delete     27 91 51

ram
variable e.x             \ cursor screen position
variable e.y             \ cursor screen position
create e.buf #cols allot \ Kill buffer

: e.addr ( -- addr ) blk-buffer e.y @ #cols * + e.x @ + ;

: e.store ( c -- c ) \ store char to cursor and move right
  dup e.addr c! dup emit
  e.x @ #cols 1- < 
  if 1 e.x +! else \cb then 
  update
;
: e.line ( addr1 -- addr2 )
  #cols for c@+ >pr emit next [char] | emit
;
: e.cp e.y @ 1+ e.x @ 5 + \cp \c+ ;

\ update screen
: e.redraw
  \h \c-
  blk-buffer
  #lines for #lines r@ - s>d <# bl hold [char] | hold # # #> type
    e.line
    cr
  next drop 
  e.cp
;
: e.redraw-line
  \c- e.y @ 1+ 5 \cp
  blk-buffer e.y @ #cols * + 
  e.line drop
  e.cp
;
: e.up    e.y @ 0 > if -1 e.y +! \cu then ;
: e.down  e.y @ #lines 1- < if  1 e.y +! \cd then ;
: e.left  e.x @ 0 > if -1 e.x +! \cb then ;
: e.right e.x @ #cols 1- < if 1 e.x +! \cf then ;

: n1- swap 1- swap ;
: cmove> ( a1 a2 u -- ) 
  dup >r + swap r@ + swap r>
  for  
     inline over inline over c! 1- n1- 
  next 2drop
;

: e.ins ( -- )
  e.addr dup 1+ #cols e.x @ - 1- cmove>
  bl e.store e.left drop
;
: e.del ( -- )
  e.addr 1+ dup 1- #cols e.x @ - 1- cmove
  bl blk-buffer e.y @ 1+ #cols * 1-  + c! update
;
: e.addr.y ( -- a ) blk-buffer e.y @ #cols * + ;
: e.ins-line ( -- )
  e.addr.y dup #cols + 
  blk-buffer blksize + over - cmove>
  e.buf blk-buffer e.y @ #cols * + #cols cmove \ insert e.buf
  e.redraw update
;
: e.del-line ( -- )
  e.addr.y e.buf #cols cmove      \ save line to e.buf
  e.addr.y dup #cols + swap over
  blk-buffer blksize + swap - cmove 
  blk-buffer blksize + #cols - #cols blanks
  e.redraw update
;
: e.s 2dup \cp \el \cp .s e.cp ;
: e.esc
  \ 9 1 e.s
  2 ms key? if key #91 <> if exit then else exit then
  2 ms key? if 
    key case
    #68 of e.left  endof
    #67 of e.right endof
    #65 of e.up    endof
    #66 of e.down  endof
    #51 of e.del key drop e.redraw-line endof
    #50 of e.ins key drop e.redraw-line endof
    endcase
    \ 10 1 e.s
  then
;
: e.bs e.x @ 0 > if drop e.left bl e.store e.left then ;
: e.crlf e.y @ 0 #lines 1- within if 1 e.y +! 0 e.x ! then e.cp ;
: e.sl 0 e.x ! e.cp ;
: e.el #cols 1- e.x ! e.cp ;

: edit ( block --- )  \ Start editing a block
  block-init
  e.buf #cols blanks
  block drop 0 e.x ! 0 e.y ! \cls e.redraw
  begin
    key dup case
      k-esc   of e.esc  endof
      k-bs    of e.bs   endof
      k-crlf  of e.crlf endof
      k-sl    of e.sl   endof
      k-el    of e.el   endof
      k-ins   of e.ins  endof
      k-flush of flush  endof
      k-ins-line of e.ins-line endof
      k-del-line of e.del-line endof
      k-exit  of drop #lines 1+ 1 \cp exit endof
      default dup $20 $7e within if e.store then endof
    endcase
    drop
  again
;

