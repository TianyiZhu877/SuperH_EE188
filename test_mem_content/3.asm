    .global _program
_program:
	mov.l stack_bottom_ptr, r15
;------------------------------------------------------------------
; 1) CMP/GE  (large equal)  - expect PASS
;------------------------------------------------------------------
	MOV.L   val789abcde, R0
    MOV.L   val789abcde, R1
	CMP/GE  R0, R1                ; T=1  ? BT
	MOV     #0, R2
	BT      success_1
	NOP
	BRA     end_1
    NOP
success_1:      
	MOV     #1, R2
end_1:      
	MOV.L   R2, @-R15             ; push result
;------------------------------------------------------------------
; 2) CMP/EQ  (large unequal) - expect PASS
;------------------------------------------------------------------
	MOV   	#H'CD, R0
	CMP/EQ  R0, R1                ; T=0  ? BF
	MOV     #0, R2
	BF      success_2
	NOP
	BRA     end_2
	NOP
success_2:      
	MOV     #2, R2
end_2:      
	MOV.L   R2, @-R15
;------------------------------------------------------------------
; 3) CMP/GT  (signed  -5  >  -10) - expect PASS
;------------------------------------------------------------------
	MOV     #-5,  R0
	MOV     #-10, R1
	CMP/GT  R0, R1                ; signed:  -5 > -10 ? T=1
	MOV     #0, R2
	BF/S    success_3
	NOP
	BRA     end_3
	NOP
success_3:      
	ADD     #3, R2
end_3:      
	MOV.L   R2, @-R15	
;------------------------------------------------------------------
; 4) CMP/HS  (unsigned 0xFFFFFFF0 >= 0x0010) - expect FAIL
;------------------------------------------------------------------
	MOV   #H'F0, R0
	MOV     #H'10, R1
	CMP/HS  R0, R1                ; higher-or-same (unsigned)
	MOV     #4, R2
	BT      fail_4                    ; T=0 because higher
	NOP
	BRA     end_4
	NOP
fail_4:      
	MOV     #0, R2
end_4:      
	MOV.L   R2, @-R15
;------------------------------------------------------------------
; 5) CMP/HI  (unsigned 0x10 > 0xFFFFFFF0) - expect PASS
;------------------------------------------------------------------
	CMP/HI  R1, R0                ; 0x10 higher? no ? T=0
	MOV     #5, R2                ; preset pass?0 later
    BT      success_5
	MOV     #0, R2                ; correct: should pass
success_5:      
	MOV.L   R2, @-R15
	
;------------------------------------------------------------------
; 6)
;------------------------------------------------------------------	
test_sum_function:
	mov		#8, R7
	mov.l   R7, @-R15
	bsr		compute_sum
	nop
	mov.l	@R15, R0
	cmp/eq	#36, R0
	BT      success_6
	NOP
	BRA     end_6
    NOP
success_6:      
	MOV     #6, R2
end_6:      
	MOV.L   R2, @-R15             ; push result
;------------------------------------------------------------------
; 7) JMP indirect test  (should execute)
;------------------------------------------------------------------
	MOV.L   #target, R0
	mov		#0, R2
	JMP     @R0
	NOP
	mov		#-7, R2
target:
	ADD     #7, R2
	MOV.L   R2, @-R15
	
	
test_logic:
	mov.l 	int64_l, r4
	mov.l 	int64_h, r0
	xor		r4, r0
	mov.l  	r0, @-r15; 
	or		#H'43, r0
	mov.l  	r0, @-r15; 
test_shift:
	shlr	r4
	rotl	r4
	rotcl	r4
	rotcr	r4
	swap.w	r4, r5
	mov.l  	r5, @-r15; 
	shll2	r5
	shlr8	r5
	mov.l  	r5, @-r15; 
	xtrct	r0, r5	
	mov.l  	r5, @-r15; 
test_sub_64:
	clrt
	mov.l  	val789abcde, r1
	mov.l  	int64_l, r2
	subc   	r2, r1
	mov.l  	r1, @-r15; 
	mov.l  	val123456, r1
	mov.l  	int64_h, r2
	subc   	r2, r1
	mov.l  	r1, @-r15; 
	negc	r2,	r2
	mov.l  	r2, @-r15;  	
test_addv:
	mov.l  	array1, r7
	addv   	r1, r7
	mov.l  	r7, @-r15;  
	stc    	sr, r0
	and    	#1, r0
	mov.l  	r0, @-r15; 	
test_mul:
	mov		#-81, r6
	muls.w  r6, r7
	mov.l  	val789abcde, r1
	sts		macl, r2
	mov.l  	r2, @-r15;  
	dmulu.l	r7, r1
	sts		macl, r2
	mov.l  	r2, @-r15; 
	sts		mach, r2
	mov.l  	r2, @-r15; 

test_mov_immd:
	mov 	#10, R1
	mov 	R1, R2
test_pc_addressing:
	mov.l  val123456, r3; 
	mov.w  valabcd, r5;    mov.w @(pc), R
	mov.w  val4321, r4;  
test_gbr:
	mov.l  data_store_ptr, r6
	mov.l  r5, @(0, r6);  mov.l Rn, @(0, Rm)
	ldc 	r6, GBR
	mov.l  @(0, GBR), r0
	add   #1, r0
	mov.l  r0, @(8, GBR)
		
test_list_operation: ;addressing by R0 as base, and self-decrementing to push on stack
	mov.l  array1_ptr, r1
	mov.l  array2_ptr, r2
	mov  #0, r0
	mov.l  @(r0, r1), r6
	mov.l  r6, @(r0, r2)
	mov.l  r6, @-r15
	add	   #4, r0
	mov.w  @(r0, r1), r6
	mov.w  r6, @(r0, r2)
	mov.w  r6, @-r15
	add	   #2, r0
	mov.w  @(r0, r1), r6
	mov.w  r6, @(r0, r2)
	mov.w  r6, @-r15
	add	   #2, r0
	mov.b  @(r0, r1), r6
	mov.b  r6, @(r0, r2)
	mov.b  r6, @-r15
	add	   #1, r0
	mov.b  @(r0, r1), r6
	mov.b  r6, @(r0, r2)
	mov.b  r6, @-r15
	add	   #1, r0
	mov.b  @(r0, r1), r6
	mov.b  r6, @(r0, r2)
	mov.b  r6, @-r15
	add	   #1, r0
	mov.b  @(r0, r1), r6
	mov.b  r6, @(r0, r2)
	mov.b  r6, @-r15
	add	   #1, r0
	
	
	sleep
	
	.align 4	
stack_bottom_ptr:
	.data.l stack_bottom
	.data.b 0
	
	.align 2
val4321:
	.data.w H'4321
	
	.align 2
valabcd:
	.data.w H'abcd
	
	.align 4
val123456:
	.data.l H'123456
val789abcde
	.data.l H'789abcde
	
	.align 2
dest1:
	.data.w 0
	
	.align 4
data_store_ptr:
	.data.l data_store

	.align 4
data_store:
	.data.l 0
	.data.w 0
	.data.b 0
	.data.b 0
	.align 4
	.data.l 0
	
	.align 4
array1_ptr:
	.data.l array1
array1:
	.data.l H'ee188765
int64_h:
	.data.l H'e11089ab
int64_l:
	.data.l H'cafe4567
	

	.align 4
array2_ptr:
	.data.l array2
array2:
	.res 12
	
compute_sum:
	mov.l @R15, R6
	add	  #4, R15
	mov		#0, r5
	add		#1, R6
compute_sum_loop:
	dt	R6
	bt	ret_compute_sum
	nop
	add	R6,R5
	bra  compute_sum_loop
	nop
ret_compute_sum:
	mov.l	R5, @-R15
	rts
	nop
	.align 4
	.res 128
stack_bottom:
	.data.l stack_bottom
	
	.res 1024
	
	