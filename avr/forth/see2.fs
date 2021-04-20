\ Requires asm2.txt

-see
marker -see
ram 0 value d.ind    \ current index

: u.4 4 u.r ;
: u.2 cell u.r ;
: d.t c@+ $7 and type space ;
: d.opcode ( -- )
  opcodes
  begin
    @+ dup 1+
  while                                     \ addr code
    over c@ 4>> to d.ind                    \ addr code
    d.ind rules @+ 4>> swap @ 4>> or invert \ addr code mask 
    @p and =
    if dup c@ $f0 and $f0 = if drop else d.t then exit then
    sy+
  repeat
  2drop
;

: d.dr ( d/r -- shift masked )
  d.ind rules + dsm     \ shift mask
  @p and                \ shift masked
;
: d.dest ( -- destination )
  false d.dr swap rshift
;
: d.res ( -- resource )
  cell d.dr
  dup $f and             \ shift masked masked-lo
  swap rot rshift or
;
: d.sy ( code table -- )
  swap >r 
  begin @+ dup 1+
  while r@ = if d.t rdrop exit then
        sy+
  repeat
  2drop r> .
;

: d.rg [char] r emit decimal u.2 hex ;
: .ldssts ;
:noname d.rg d.rg ;           \ $d
:noname d.rg 7 and u.2 ;      \ $c
:noname 2* d.rg 2* d.rg ;     \ $b
:noname $10 or d.rg u.2 ;     \ $a
:noname d.rg drop ;           \ $9  pop push com neg ...
:noname 2drop ;               \ $8  (sts)
:noname 2drop ;               \ $7  (lds)
:noname swap u.2 d.rg ;       \ $6  out
:noname d.rg u.2 ;            \ $5  in
:noname u.2 u.2 ;             \ $4  cbi ...
:noname 2* $18 + d.rg u.2 ;   \ $3  adiw ...
:noname swap dup if sy1 d.sy else drop >r @+ u.4 r> then d.rg ; \ $2 st sts
:noname d.rg dup if sy1 d.sy else drop @+ u.4 then ;       \ $1  ld lds
:noname 2drop ;               \ $0

flash create d.posts , , , , , , , , , , , , , , ram
 
: d.post ( resource destination -- )
  d.ind
  as: cpi r24 $e
  as: if lt
    2* d.posts + @ex exit
  as: then
  drop 2drop
; 
: rel>abs ( addr --- addr abs )
  as: movw  r30 r20   \ r20:21 is P register
  as: add   r30 r30
  as: adc   r31 r31
  as: andi  r31 $1f
  as: sbrc  r31 4
  as: ori   r31 $e0
  dup
  as: add   r24 r30
  as: adc   r25 r31
;
: d.cid c>n .id ;
: d.adr  @+ xa> ( dup u.4 ) ;

: d.rcall  ( addr -- addr )
  as: movw r30 r20
  as: andi r31 $f0
  as: cpi r31 $d0
  as: if eq  
        ." rcall "  rel>abs d.cid
  as: then
;

: d.rjmp    ( addr -- addr )
  as: movw r30 r20
  as: andi r31 $f0
  as: cpi r31 $c0
  as: if eq   
        ." rjmp " rel>abs u.4
  as: then
;
: d.call ( addr -- addr )
  as: movw r30 r20
  as: andi r30 $fe
  as: subi r30 $0e
  as: sbci r31 $94
  as: if eq   
      d.adr ." call  " d.cid
      -@ xa> ['] (s" = 
      if 
        space 2+ c@+ 2dup type + aligned
      else
        2+
      then
  as: then
;
: d.jmp  ( addr -- addr )
  as: movw r30 r20
  as: andi r30 $fe
  as: subi r30 $0c
  as: sbci r31 $94
  as: if eq
        d.adr ." jmp   " d.cid
  as: then
;

: d.bra ( addr -- addr )
  as: movw r30 r20
  as: andi r31 $f8
  as: cpi  r31 $f0
  as: if eq
        ." br" @p $f407 and sy2 d.sy
        @p 3 rshift $7f and
        dup $40 and if $ffc0 or then
        2* over + u.4
  as: then 
;
: see ( -- )
  '
  hex
  begin
    cr dup u.4
    @+ dup u.4 !p  \ the opcode is held in the P register

    d.call d.rcall d.rjmp d.bra d.jmp

    d.opcode space d.res d.dest d.post
    
    false
    as: movw r16 r20
    as: subi r16 $8
    as: sbci r17 $95
    as: if eq
    as:   adiw r24 1     \ return
    as: then
    as: movw r16 r20
    as: subi r17 $94
    as: if eq
    as:   cpi r16 $09
    as:   if eq
    as:     adiw r24 1     \ ijmp
    as:   then
    as:   andi r16 $fe     \ m2560
    as:   cpi r16 $0c
    as:   if eq
    as:     adiw r24 1     \ jmp
    as:   then
    as: then
    
  until
  drop
;

\ Hide the intermediate words
\ ' -as  c>n 2- @ ' as: c>n 2- !
\ ' -dis c>n 2- @ ' see c>n 2- !

