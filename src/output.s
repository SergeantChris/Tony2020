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
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movl	$25, 4(%rsp)
	movl	$25, %edi
	callq	puti
	movl	4(%rsp), %edi
	movl	%edi, 12(%rsp)
	callq	puti
	movl	12(%rsp), %edi
	leaq	8(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	32(%rsp), %rax
	movq	%rax, 24(%rsp)
	movl	%edi, 4(%rsp)
	callq	puti
	movl	4(%rsp), %eax
	movq	24(%rsp), %rcx
	movl	%eax, 4(%rcx)
	movq	24(%rsp), %rax
	movl	4(%rax), %edi
	callq	puti
	movq	24(%rsp), %rax
	movl	4(%rax), %edi
	movl	%edi, 4(%rsp)
	callq	puti
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	jackthecutestdoggo, .Lfunc_end1-jackthecutestdoggo
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
