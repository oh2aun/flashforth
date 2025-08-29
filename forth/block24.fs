\ Experimental code for blocks
\ simple block wordset, PIC24 version
\ single buffer management.
\ needs core.fs ( evaluate )
-block
marker -block
\ blocksize 512 to save memory.
#1024 constant blksize \ bytes in flash
#512  constant blklen  \ words in flash
#64 constant #cols
blksize #cols / constant #lines

#2 constant #blk
fsize #blk blklen 2* um* d- 2constant  blk-start ( absolute flash address )
ram
variable scr
variable blk 
variable blk-dirty
create blk-buffer blksize allot

\ for turnkey
: block-init
  true blk !
  0 blk-dirty !
;
: blk-limit? 0 #blk within abort" ERROR:blk-limit" ;

: load-buffer ( buf-addr u -- ) 
  swap !p>r blksize um* blk-start d+ blklen
  for
    2dup x@ drop p! 2 m+ p2+
  next r>p 2drop ;

: save-buffer ( buf-addr u -- )
  swap !p>r blksize um* blk-start d+ blklen
  for 
    2dup >r >r p@ 0 r> r> x! 2 m+ p2+
  next r>p 2drop ;

: update -1 blk-dirty ! ;
: updated? ( u -- f ) 
  blk @ = 
  if   blk-dirty @ 
  else false 
  then
;

\ reloads the block only if the blocknumber differs
: block ( u -- a-addr )
   dup blk-limit?
   dup blk @ = 
   if   drop 
   else blk @ updated? 
        if   blk-buffer blk @ save-buffer
        then blk-buffer swap dup blk ! load-buffer
        false blk-dirty !
   then
   blk-buffer
;

\ a buffer is an un-initialized block.
: buffer ( u -- a-addr )  block ;

: save-buffers
  blk @ updated? if
    blk-buffer blk @ save-buffer
  then
  false blk-dirty !
;

: empty-buffers
  true  blk !
  false blk-dirty !
;

: flush save-buffers empty-buffers ;

: wipe ( -- ) blk-buffer blksize blanks update ;
: wipe-all ( -- ) #blk for r@ block drop wipe flush next ;
: list      ( blk -- )                  \ list selected screen
   dup blk-limit?
   dup scr  !
   dup cr  ." Listing of screen " .  ." ("
   dup updated? 0= if ." not " then ." modified)" cr
   buffer !p>r
   #lines
   for
     cr  #lines r@ - s>d <# [char] | hold # # #>  type  space
	 @p #cols type #cols p++
	 [char] |  emit
   next
   r>p
   cr
 ;
 
: load ( n -- )
  block !p>r dp>
  #lines for
    cr @p #cols 2dup type evaluate
    #cols p++
    state 0= if iflush >dp then
  next r>p
  postpone [ iflush >dp
;

\ load blocks u1 thru u2
: thru ( u1 u2 -- ) over - 1+ for load blk @ 1+ next drop ;


