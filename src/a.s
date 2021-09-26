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
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	geti
	movl	%eax, 4(%rsp)
	leaq	4(%rsp), %rsi
	movl	%eax, %edi
	callq	"prime?"
	movzbl	%al, %edi
	callq	putb
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	jackthecutestdoggo, .Lfunc_end1-jackthecutestdoggo
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
	jns	.LBB2_2
# %bb.1:                                # %then
	negl	%edi
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	%rsi, -16(%rax)
	callq	"prime?"
	jmp	.LBB2_4
.LBB2_2:                                # %ifelse2
	cmpl	$1, %edi
	jg	.LBB2_5
.LBB2_3:                                # %then7
	xorl	%eax, %eax
	jmp	.LBB2_4
.LBB2_5:                                # %endif10
	cmpl	$2, %edi
	jne	.LBB2_7
.LBB2_6:                                # %then12
	movb	$1, %al
.LBB2_4:                                # %then7
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB2_7:                                # %endif15
	.cfi_def_cfa %rbp, 16
	movl	%edi, %ecx
	shrl	$31, %ecx
	addl	%edi, %ecx
	movl	%ecx, %eax
	andl	$-2, %eax
	cmpl	%eax, %edi
	je	.LBB2_3
# %bb.8:                                # %endif21
	movl	$3, -4(%rbp)
	sarl	%ecx
	cmpl	$2, %ecx
	setg	%al
	testb	$1, %al
	je	.LBB2_6
	.p2align	4, 0x90
.LBB2_10:                               # %body
                                        # =>This Inner Loop Header: Depth=1
	movl	%edi, %eax
	cltd
	idivl	-4(%rbp)
	testl	%edx, %edx
	je	.LBB2_3
# %bb.11:                               # %ifelse32
                                        #   in Loop: Header=BB2_10 Depth=1
	movl	-4(%rbp), %eax
	addl	$2, %eax
	movl	%eax, -4(%rbp)
	cmpl	%ecx, %eax
	setle	%al
	testb	$1, %al
	jne	.LBB2_10
	jmp	.LBB2_6
.Lfunc_end2:
	.size	"prime?", .Lfunc_end2-"prime?"
	.cfi_endproc
                                        # -- End function

	.section	".note.GNU-stack","",@progbits
