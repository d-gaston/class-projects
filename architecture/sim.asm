*=$C000
;acme --cpu 6510 --format cbm --outfile sim.prg sim.asm
AC=$02			;ACcumulator
XR=$03			;X Register
YR=$04			;Y Register
PCL=$05			;Program Counter Low byte
PCH=$06			;Program Counter High byte
SR=$07			;Status Register
SP=$08			;Stack Pointer
StackPage=$09	;Stores high byte of stack address
tmpL=$0A		;temporary address Low byte
tmpH=$0B		;temporary address High byte
labels=$0420	;Where to display register names on screen
values=$0448	;Where to display register values on screen
;Initialize CPU values
	lda #$00
	sta AC
	sta XR
	sta YR
	lda #$00	;Start Program counter at $1000
	sta PCL
	lda #$10
	sta PCH
	lda #%00100000 ;NV-BDIZC
	sta SR	
	lda #$ff	;Stack starts at top of page, grows down
	sta SP
	lda #$08
	sta StackPage

main:

;**************************
;	Display Register Contents
;	on screen
;**************************
	lda #$81 ;A
	sta labels
	lda #$83 ;C
	sta labels+1
	lda #$a0 ;" "
	sta labels+2
	lda #$98
	sta labels+3
	lda #$92
	sta labels+4
	lda #$a0
	sta labels+5
	lda #$99
	sta labels+6
	lda #$92
	sta labels+7
	;high 4 bits

	ldx #$00
	lda AC 
	jsr convert
	lda #$a0
	sta values,x
	inx
	lda XR
	jsr convert
	lda #$a0
	sta values,x
	inx
	lda YR
	jsr convert
	;END Text display section
	
	;Load next instruction
	ldy #$00
	lda (PCL),y
	
;*********Comparisons*********
;First find high nibble, then find specific opcode, then 
;jump to implementation
	cmp #$f0	;If the opcode is less than #$f0, start
	bcc .e		;comparing to opcodes that start with e
	cmp #$fe
	bne +
	jmp fe		;INC ABS,X
+	cmp #$fd
	jmp f0
.e:
	cmp #$e0
	bcc .d
	cmp #$ee
	bne +
	jmp ee		;INC ABS
+	cmp #$ea
	bne +
	jmp ea		;NOP
+	cmp #$e8
	bne +
	jmp e8		;INX
+	cmp #$e6
	bne +
	jmp e6		;INC ZP
+	cmp #$e5
	jmp e0		;CPX #


	
.d:
	cmp #$d0
	bcc .c
	cmp #$de
	bne +
	jmp de
+	cmp #$d0
	jmp d0		;BNE
.c:
	cmp #$c0
	bcc .b
	cmp #$ce
	bne +
	jmp ce		;DEC ABS
+	cmp #$cd
	bne +
	jmp cd		;CMP ABS
+	cmp #$ca
	bne +
	jmp ca		;DEX
+	cmp #$c9	
	bne +
	jmp c9		;CMP # 
+	cmp #$c8
	bne	+
	jmp c8		;INY
+	cmp #$c6	
	bne +
	jmp c6		;DEC ZP
+	cmp #$c5	
	bne +
	jmp c5		;CMP ZP
+	cmp #$c4
	jmp c0		;CPY #	
.b:
	cmp #$b0
	bcc .a
	cmp #$bd	
	bne +
	jmp bd		;LDA ABS,X
+	cmp #$ba

.a:
	cmp #$a0
	bcc ._9
	cmp #$ae
	bne +
	jmp ae		;LDX ABS
+	cmp #$ad
	bne +
	jmp ad		;LDA ABS
+	cmp #$aa
	bne +
	jmp aa		;TAX
+	cmp #$a9
	bne +
	jmp a9		;LDA #
+	cmp #$a8
	bne +
	jmp a8		;TAY
+	cmp #$a6
	bne +
	jmp a6		;LDX ZP
+	cmp #$a5
	bne +
	jmp a5		;LDA ZP
+	cmp #$a2	
	bne +
	jmp a2		;LDX #
+	cmp #$a1

	jmp a0		;LDY #
._9:
	cmp #$90
	bcc ._8
	cmp #$9d
	bne +
	jmp _9d		;STA ABS,X
+	cmp #$9c
	cmp #$98
	bne +
	jmp _98		;TYA
+	cmp #$96
._8:
	cmp #$80
	bcc ._7
	cmp #$8d
	bne +
	jmp _8d		;STA ABS
+	cmp #$8a
	bne +
	jmp _8a		;TXA
+	cmp #$88
	bne +
	jmp _88		;DEY
+	cmp #$86
	bne +
	jmp _86		;STX ZP
+	cmp #$85
	bne +
	jmp _85		;STA ZP
+	cmp #$84
._7:
._6:
	cmp #$60
	bcc ._5
	jmp _60		;RTS
._5:
._4:
	cmp #$40
	bcc ._3
	cmp #$4c
	bne +
	jmp _4c		;JMP ABS
+	cmp #$4d

._3:
	cmp #$30
	bcc ._2
._2:
	cmp #$20
	bcc ._1
	jmp _20		;JSR
._1:

_00:
	brk

;================
;DEC ABS,X
;================
de:	
	jsr getTarget
	jsr offsetAbsX
	;Add x register to absolute address
	;increment new address
	ldy #$00
	lda (tmpL),y
	sec
	sbc #$01
	sta (tmpL),y
	jsr updateZ		;do Zero flag updates
	jsr updateN
	ldx #$03
	jsr updatePC
	jmp main
;================
;RTS
;================
_60:
	ldy #$00
	lda (SP),y
	sta PCL
	nop
	inc SP
	nop
	lda (SP),y
	sta PCH
	ldx #$01
	jsr updatePC
	nop	
	jmp main


;================
;JSR
;================
_20:
	jsr getTarget
	;put the address of the last byte of
	;the instruction on the stack
	ldx #$02
	jsr updatePC
	ldy #$00
	lda PCH
	sta (SP),y
	dec SP
	lda PCL
	sta (SP),y
	;set PC to target address
	lda tmpL
	sta PCL
	lda tmpH
	sta PCH
	nop
	jmp main
	
	
	
;================
;LDX ABS
;================
ae:
	jsr getTarget
	ldy #$00
	lda (tmpL),y
	sta XR
	jsr updateZ
	jsr updateN
	ldx #$03
	jsr updatePC
	jmp main

;================
;LDA ABS
;================
ad:
	jsr getTarget
	ldy #$00
	lda (tmpL),y
	sta AC
	jsr updateZ
	jsr updateN
	ldx #$03
	jsr updatePC
	jmp main
;================
;LDA #
;================
a9:
	ldy #$01		
	lda (PCL),y		;get next byte after opcode
	sta AC			
	jsr updateZ
	jsr updateN
	
	ldx #$02
	jsr updatePC
	jmp main



;================
;TAX
;================
aa:
	lda AC
	sta XR
	jsr updateZ
	jsr updateN
	ldx #$01
	jsr updatePC
	jmp main
;================
;TAY
;================
a8:
	lda AC
	sta YR
	jsr updateZ
	jsr updateN
	ldx #$01
	jsr updatePC
	jmp main
;================
;TXA
;================
_8a:
	lda XR
	sta AC
	jsr updateZ
	jsr updateN
	ldx #$01
	jsr updatePC
	jmp main
;================
;TYA
;================
_98:
	lda YR
	sta AC
	jsr updateZ
	jsr updateN
	ldx #$01
	jsr updatePC
	jmp main

;================
;LDA ABS,x 
;================
bd:
	jsr getTarget
	jsr offsetAbsX
	ldy #$00
	lda (tmpL),y
	sta AC
	jsr updateZ
	jsr updateN
	ldx #$03
	jsr updatePC
	jmp main
;================
;LDA ZP
;================
a5:
	jsr getTarget
	lda #$00
	sta tmpH
	ldy #$00
	lda (tmpL),y
	sta AC
	jsr updateZ
	jsr updateN
	ldx #$02
	jsr updatePC
	jmp main

;================
;LDX ZP
;================
a6:
	jsr getTarget
	lda #$00
	sta tmpH
	ldy #$00
	lda (tmpL),y
	sta XR
	jsr updateZ
	jsr updateN
	ldx #$02
	jsr updatePC
	jmp main
;================
;LDX #
;================
a2:
	ldy #$01		
	lda (PCL),y		;get next byte after opcode
	sta XR			
	jsr updateZ
	jsr updateN
	ldx #$02
	jsr updatePC
	jmp main
	
;================
;LDY #
;================
a0:
	ldy #$01		
	lda (PCL),y		;get next byte after opcode
	sta YR			
	jsr updateZ
	jsr updateN
	ldx #$02
	jsr updatePC
	jmp main
;================
;STA ABS
;================
_8d:
	jsr getTarget
	lda AC
	ldy #$00
	sta (tmpL),y
	ldx #$03
	jsr updatePC
	jmp main
;================
;STA ZP
;================
_85:
	jsr getTarget
	lda #$00
	sta tmpH
	lda AC
	ldy #$00
	sta (tmpL),y
	ldx #$02
	jsr updatePC
	jmp main

;================
;STX ZP
;================
_86:
	jsr getTarget
	lda #$00
	sta tmpH
	lda XR
	ldy #$00
	sta (tmpL),y
	ldx #$02
	jsr updatePC
	jmp main
;================
;STA ABS,X
;================
_9d:
	jsr getTarget
	jsr offsetAbsX
	nop
	lda AC
	ldy #$00
	sta (tmpL),y
	nop
	ldx #$03
	jsr updatePC
	jmp main

;================
;CMP # 
;================
c9:
	ldy #$01
	lda AC
	cmp (PCL),y
	jsr updateC
	jsr updateZ
	jsr updateN
	ldx #$02
	jsr updatePC
	jmp main

;================
;CMP ABS
;================
cd:
	jsr getTarget
	ldy #$00
	lda AC
	cmp (tmpL),y
	jsr updateC
	jsr updateZ
	jsr updateN
	ldx #$03
	jsr updatePC
	jmp main

;================
;DEC ABS
;================
ce:
	jsr getTarget
	ldy #$00
	lda (tmpL),y	;get byte at target
	sec
	sbc #$01
	sta (tmpL),y
	jsr updateZ		;do Zero flag updates
	jsr updateN
	ldx #$03
	jsr updatePC
	jmp main
;================
;DEX
;================
ca:
	dec XR
	jsr updateZ
	jsr updateN
	ldx #$01
	jsr updatePC
	jmp main

;================
;CMP ZP
;================
c5:
	jsr getTarget
	ldy #$00
	sty tmpH
	lda AC
	cmp (tmpL),y
	jsr updateC
	jsr updateZ
	jsr updateN
	ldx #$03
	jsr updatePC
	jmp main

;================
;DEY
;================
_88:
	dec YR
	jsr updateZ
	jsr updateN
	ldx #$01
	jsr updatePC
	jmp main


;================
;INX
;================
e8:
	inc XR
	jsr updateZ
	jsr updateN
	ldx #$01
	jsr updatePC
	jmp main
;================
;INY
;================
c8:
	inc  YR
	jsr updateZ
	jsr updateN
	ldx #$01
	jsr updatePC
	jmp main
;================
;BNE
;================
d0:
	lda SR
	and #%00000010 
	bne +
	jsr branch
	jmp main
+	ldx #$02
	jsr updatePC
	nop	
	jmp main
;================
;NOP
;================
ea:
	ldx #$01
	jsr updatePC
	jmp main
;================
;CPX #
;================
e0:	
	lda XR
	ldy #$01
	cmp (PCL),y
	jsr updateZ
	jsr updateN
	jsr updateC
	ldx #$02
	jsr updatePC
	jmp main
;================
;DEC ZP 
;================
c6:
	jsr getTarget
	;the high bit is going to be part of the next
	;op code, since it's ZP just store #$00 there
	lda #$00
	sta tmpH
	ldy #$00
	lda (tmpL),y
	sec
	sbc #$01
	sta (tmpL),y
	jsr updateZ
	jsr updateN
	ldx #$02
	jsr updatePC
	jmp main
;================
;CPY #
;================
c0:	
	lda YR
	ldy #$01
	cmp (PCL),y
	jsr updateZ
	jsr updateN
	jsr updateC
	ldx #$02
	jsr updatePC
	jmp main
;================
;JMP ABS
;================
_4c:	
	jsr getTarget
	lda tmpL
	sta PCL
	lda tmpH
	sta PCH
	jmp main
;================
;INC ABS
;================
ee:		
	jsr getTarget
	ldy #$00
	lda (tmpL),y	;get byte at target
	clc
	adc #$01
	sta (tmpL),y
	jsr updateZ		;do Zero flag updates
	jsr updateN
	ldx #$03
	jsr updatePC
	jmp main

;================
;INC ZP 
;================
e6:
	jsr getTarget
	;the high bit is going to be part of the next
	;op code, since it's ZP just store #$00 there
	lda #$00
	sta tmpH
	ldy #$00
	lda (tmpL),y
	clc
	adc #$01
	sta (tmpL),y
	jsr updateZ
	jsr updateN
	ldx #$02
	jsr updatePC
	jmp main
;================
;INC ABS,X
;================
fe:	
	jsr getTarget
	jsr offsetAbsX
	;Add x register to absolute address
	;increment new address
	ldy #$00
	lda (tmpL),y
	clc
	adc #$01
	sta (tmpL),y
	jsr updateZ		;do Zero flag updates
	jsr updateN
	ldx #$03
	jsr updatePC
	jmp main
	
;================
;BEQ
;================
f0:
	
	lda SR
	and #%00000010 
	beq +
	jsr branch
	jmp main
+	ldx #$02
	jsr updatePC
	jmp main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Move the program counter forward by the number
;of bytes stored in X
updatePC:

-	
	lda PCL
	clc
	adc #$01
	sta PCL
	lda PCH
	adc #$00
	sta PCH
	dex
	cpx #$00
	bne -
	
	rts

;Subroutines to update flags. Simply looks at hardware
;flags and updates the memory locations accordingly
;Update the Zero flag
updateZ:
	php
	beq +	
	lda SR
	and #%11111101
	sta SR 
	plp
	rts
+	lda SR
	ora #%00000010
	sta SR
	plp
	rts

;Update the Negative flag
updateN:
	php
	bmi +	
	lda SR
	and #%01111111
	sta SR 
	plp
	rts
+	lda SR
	ora #%10000000
	sta SR
	plp
	rts

;Update the Carry flag
updateC:
	php
	bcs +	
	lda SR
	and #%11111110
	sta SR 
	plp
	rts
+	lda SR
	ora #%00000001
	sta SR
	plp
	rts
;Calculate branch (change PC to new value)
;Sign extend in X, offset in A
;Signed addition code from codebase64.org
branch:
	ldy #$01
	lda (PCL),y	;get offset
	nop
	pha
	ldx #$02	;We are branching from the next instruction
	jsr updatePC
	nop
	ldx #$00
	pla
	nop
	bpl +
	dex			;change sign extend to FF
+	clc
	adc PCL
	sta PCL
	txa
	adc PCH
	sta PCH
	nop
	rts
;puts the target address in tmpL and tmpH
;for ops that operate on an ABS address
getTarget:
	ldy #$01
	lda (PCL),y		;get low byte of target
	sta tmpL		
	iny
	lda (PCL),y		;get high byte of target
	sta tmpH
	rts


;add x to the address in tmpL and tmpH
offsetAbsX:
	clc
	lda tmpL
	adc XR
	sta tmpL
	lda tmpH
	adc #$00
	sta tmpH
	rts
offsetZPX:
	rts
offsetAbsY:
	clc
	lda tmpL
	adc YR
	sta tmpL
	lda tmpH
	adc #$00
	sta tmpH
	rts
offsetZPY:
	rts
convert:
	;convert low  bits
	tay
	and #%00001111
	pha
	;convert high bits
	tya
	and #%11110000
	lsr
	lsr
	lsr
	lsr
	cmp #$0a
	bcc +
	sec
	sbc #$09
	ora  #$80
	clc
	bcc ++
+	ora  #$b0
++	sta values,x
	inx
	pla
	cmp #$0a
	bcc +
	sec
	sbc  #$09
	ora  #$80
	clc
	bcc ++
+	ora  #$b0
++	sta  values,x
	inx
	rts
