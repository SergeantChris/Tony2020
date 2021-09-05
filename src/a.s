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
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rsp, %rax
	addq	$-16, %rax
	movq	%rax, %rsp
	movq	%rsp, %rax
	addq	$-16, %rax
	movq	%rax, %rsp
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movl	$0, -4(%rbp)
	movl	$111, %edi
	callq	puti
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
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
	pushq	%rax
	.cfi_def_cfa_offset 16
	testl	%edi, %edi
	jns	.LBB2_1
# %bb.8:                                # %then
	negl	%edi
	callq	"prime?"
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.LBB2_1:                                # %ifelse2
	.cfi_def_cfa_offset 16
	cmpl	$1, %edi
	jg	.LBB2_2
.LBB2_9:                                # %then5
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.LBB2_2:                                # %endif8
	.cfi_def_cfa_offset 16
	cmpl	$2, %edi
	jne	.LBB2_3
.LBB2_10:                               # %then10
	movb	$1, %al
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.LBB2_3:                                # %endif13
	.cfi_def_cfa_offset 16
	movl	%edi, %ecx
	shrl	$31, %ecx
	addl	%edi, %ecx
	movl	%ecx, %eax
	andl	$-2, %eax
	cmpl	%eax, %edi
	je	.LBB2_9
# %bb.4:                                # %endif19
	movl	$3, 4(%rsp)
	sarl	%ecx
	cmpl	$2, %ecx
	setg	%al
	testb	$1, %al
	je	.LBB2_10
	.p2align	4, 0x90
.LBB2_6:                                # %body
                                        # =>This Inner Loop Header: Depth=1
	movl	%edi, %eax
	cltd
	idivl	4(%rsp)
	testl	%edx, %edx
	je	.LBB2_9
# %bb.7:                                # %ifelse30
                                        #   in Loop: Header=BB2_6 Depth=1
	movl	4(%rsp), %eax
	addl	$2, %eax
	movl	%eax, 4(%rsp)
	cmpl	%ecx, %eax
	setle	%al
	testb	$1, %al
	jne	.LBB2_6
	jmp	.LBB2_10
.Lfunc_end2:
	.size	"prime?", .Lfunc_end2-"prime?"
	.cfi_endproc
                                        # -- End function

	.section	".note.GNU-stack","",@progbits
