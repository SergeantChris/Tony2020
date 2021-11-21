	.text
	.file	"tony program"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %program_entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	jackthecutestdoggo
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	jackthecutestdoggo      # -- Begin function jackthecutestdoggo
	.p2align	4, 0x90
	.type	jackthecutestdoggo,@function
jackthecutestdoggo:                     # @jackthecutestdoggo
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movl	$1954047316, 23(%rsp)   # imm = 0x74786554
	movl	$1954047316, 18(%rsp)   # imm = 0x74786554
	movb	22(%rsp), %al
	movb	%al, 27(%rsp)
	movabsq	$7935449335195920743, %rax # imm = 0x6E20612065766967
	movq	%rax, 40(%rsp)
	movl	$1700949365, 48(%rsp)   # imm = 0x65626D75
	movw	$14962, 52(%rsp)        # imm = 0x3A72
	movb	$32, 54(%rsp)
	leaq	40(%rsp), %rdi
	callq	puts
	callq	geti
	movl	%eax, 8(%rsp)
	leaq	4(%rsp), %rsi
	leaq	8(%rsp), %rdx
	movl	%eax, %edi
	callq	getit
	movl	$1868654915, 13(%rsp)   # imm = 0x6F616943
	leaq	13(%rsp), %rdi
	callq	strlen
	movl	%eax, 4(%rsp)
	movabsq	$2335433509065269357, %rax # imm = 0x206920672061206D
	movq	%rax, 28(%rsp)
	movw	$14947, 36(%rsp)        # imm = 0x3A63
	movb	$32, 38(%rsp)
	leaq	28(%rsp), %rdi
	callq	puts
	movl	4(%rsp), %edi
	callq	puti
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	jackthecutestdoggo, .Lfunc_end1-jackthecutestdoggo
	.cfi_endproc
                                        # -- End function
	.globl	myprint                 # -- Begin function myprint
	.p2align	4, 0x90
	.type	myprint,@function
myprint:                                # @myprint
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	putb
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	myprint, .Lfunc_end2-myprint
	.cfi_endproc
                                        # -- End function
	.globl	"prime?"                # -- Begin function prime?
	.p2align	4, 0x90
	.type	"prime?",@function
"prime?":                               # @"prime?"
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	testl	%edi, %edi
	jns	.LBB3_2
# %bb.1:                                # %then
	negl	%edi
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	%rsi, -16(%rax)
	callq	"prime?"
	jmp	.LBB3_4
.LBB3_2:                                # %ifelse2
	cmpl	$1, %edi
	jg	.LBB3_5
.LBB3_3:                                # %then7
	xorl	%eax, %eax
	jmp	.LBB3_4
.LBB3_5:                                # %endif10
	cmpl	$2, %edi
	jne	.LBB3_7
.LBB3_6:                                # %then12
	movb	$1, %al
.LBB3_4:                                # %then7
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB3_7:                                # %endif15
	.cfi_def_cfa %rbp, 16
	movl	%edi, %ecx
	shrl	$31, %ecx
	addl	%edi, %ecx
	movl	%ecx, %eax
	andl	$-2, %eax
	cmpl	%eax, %edi
	je	.LBB3_3
# %bb.8:                                # %endif21
	movl	$3, -4(%rbp)
	sarl	%ecx
	cmpl	$2, %ecx
	setg	%al
	testb	$1, %al
	je	.LBB3_6
	.p2align	4, 0x90
.LBB3_10:                               # %body
                                        # =>This Inner Loop Header: Depth=1
	movl	%edi, %eax
	cltd
	idivl	-4(%rbp)
	testl	%edx, %edx
	je	.LBB3_3
# %bb.11:                               # %ifelse32
                                        #   in Loop: Header=BB3_10 Depth=1
	movl	-4(%rbp), %eax
	addl	$2, %eax
	movl	%eax, -4(%rbp)
	cmpl	%ecx, %eax
	setle	%al
	testb	$1, %al
	jne	.LBB3_10
	jmp	.LBB3_6
.Lfunc_end3:
	.size	"prime?", .Lfunc_end3-"prime?"
	.cfi_endproc
                                        # -- End function
	.globl	getit                   # -- Begin function getit
	.p2align	4, 0x90
	.type	getit,@function
getit:                                  # @getit
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rsi, 16(%rsp)
	callq	"prime?"
	movq	%rbx, 8(%rsp)
	movq	%r14, (%rsp)
	movzbl	%al, %edi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	myprint
	addq	$24, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end4:
	.size	getit, .Lfunc_end4-getit
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
