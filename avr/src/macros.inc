; Register definitions
  .def upl = r2         ; not in interrupt 
  .def uph = r3         ; not in interrupt
  .def r_zero = r5      ; read only zero
  .def r_one = r6       ; read only one
  .def r_two = r7       ; read only two
  .def t8 = r8          ; Not in interrupt
  .def wflags  = r9     ; not in interrupt

  .def loadreg0 = r4    ;
  .def loadreg1 = r12
  .def loadreg2 = r13


  .def ibasel=r10       ; Not in interrupt
  .def ibaseh=r11       ; Not in interrupt
  .def ms_count  = r14  ; Not in interrupt
  .def ms_count1 = r15  ; Not in interrupt
  .def t0 = r16
  .def t1 = r17
  .def t2 = r0          ; Not in interrupt
  .def t3 = r1          ; Not in interrupt

  .def al = r18
  .def ah = r19
  .def pl = r20         ; P Register and FOR..LOOP INDEX variable
  .def ph = r21

  .def FLAGS1 = r22     ; Not in interrupt
  .def FLAGS2 = r23     ; Not in interrupt
  .def tosl = r24
  .def tosh = r25
;  xl = r26
;  xh = r27
;  yl = r28  ; StackPointer Ylo
;  yh = r29  ; StackPointer Yhi
;  zl = r30
;  zh = r31
  .def t4 = r26
  .def t5 = r27
  .def t6 = r30
  .def t7 = r31

		; Macros
.macro poptos 
    ld tosl, Y+
    ld tosh, Y+
.endmacro

.macro pushtos
    st -Y, tosh
    st -Y, tosl
.endmacro

.macro in_
.if (@1 < $40)
  in @0,@1
.else
  lds @0,@1
.endif
.endmacro

.macro out_
.if (@0 < $40)
  out @0,@1
.else
  sts @0,@1
.endif
.endmacro

.macro sbi_
.if (@0 < $40)
  sbi @0,@1
.else
  in_ r16,@0
  ori r16,exp2(@1)
  out_ @0,r16
.endif
.endmacro

.macro cbi_
.if (@0 < $40)
  cbi @0,@1
.else
  in_ r16,@0
  andi r16,~(exp2(@1))
  out_ @0,r16
.endif
.endmacro

.macro lpm_
.if (FLASHEND < 0x8000) ; Word address
        lpm @0,@1
.else
        elpm @0,@1
.endif
.endmacro

.macro sub_pflash_z
.if (PFLASH > 0)
        subi    zh, high(PFLASH)
.endif
.endmacro

.macro add_pflash_z
.if (PFLASH > 0)
        subi    zh, high(0x10000-PFLASH)
.endif        
.endmacro

.macro sub_pflash_tos
.if (PFLASH > 0)
        subi    tosh, high(PFLASH)
.endif
.endmacro

.macro add_pflash_tos
.if (PFLASH > 0)
        subi    tosh, high(0x10000-PFLASH)
.endif        
.endmacro

.macro rampv_to_c
.if (FLASHEND >= 0x8000)
        bset    0
.else
        bclr    0
.endif
.endmacro

.macro fdw
  .dw ((@0<<1)+PFLASH)
.endmacro

.macro m_pop_zh
.if (FLASHEND > 0xffff)
        pop     zh
.endif
.endmacro
.macro m_pop_xh
.if (FLASHEND > 0xffff)
        pop     xh
 .endif
.endmacro
.macro m_pop_t0
.if (FLASHEND > 0xffff)
        pop     t0
 .endif
.endmacro
.macro m_push_t0
.if (FLASHEND > 0xffff)
        push    t0
 .endif
.endmacro
.macro mijmp
.if (FLASHEND > 0xffff)
        eijmp
.else
        ijmp
.endif
.endmacro
