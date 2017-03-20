
-asmtest
marker -asmtest

: qq
  as: if eq
  as: nop
  as: then
;

: ww
  as: begin
  as: nop
  as: until eq
;
\ square root of unsigned cell.
: sqrt ( u --- u )
  as: adiw r24 1
  as: ldi r16 $00
  as: ldi r17 $80
  as: begin
  as:   eor r16 r17
  as:   mul r16 r16
  as:   cp  r0  r24
  as:   cpc r1  r25
  as:   if  sh
  as:     eor r16 r17
  as:   then
  as:   lsr r17
  as: until eq
  as: movw  r24 r16
;