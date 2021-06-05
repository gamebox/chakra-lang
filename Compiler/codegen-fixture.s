	.text
	.file	"codegen-fixture.ll"
	.globl	test__main__init        # -- Begin function test__main__init
	.p2align	4, 0x90
	.type	test__main__init,@function
test__main__init:                       # @test__main__init
	.cfi_startproc
# %bb.0:
	movq	(%rdi), %rdi
	movl	$.L.str.1, %esi
	jmp	Chakra_stdlib__print    # TAILCALL
.Lfunc_end0:
	.size	test__main__init, .Lfunc_end0-test__main__init
	.cfi_endproc
                                        # -- End function
	.globl	Main_Actor__receive     # -- Begin function Main_Actor__receive
	.p2align	4, 0x90
	.type	Main_Actor__receive,@function
Main_Actor__receive:                    # @Main_Actor__receive
	.cfi_startproc
# %bb.0:
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	cmpq	$101, %rdi
	je	.LBB1_3
# %bb.1:
	cmpq	$100, %rdi
	jne	.LBB1_5
# %bb.2:
	movl	$101, %r14d
	jmp	.LBB1_4
.LBB1_3:
	movl	$100, %r14d
.LBB1_4:
	callq	Chakra_stdlib__commands__none
	movq	%rax, %rbx
	movl	$16, %edi
	callq	malloc
	movq	%r14, (%rax)
	movq	%rbx, 8(%rax)
	jmp	.LBB1_6
.LBB1_5:
	xorl	%eax, %eax
.LBB1_6:
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	Main_Actor__receive, .Lfunc_end1-Main_Actor__receive
	.cfi_endproc
                                        # -- End function
	.type	.L.str.1,@object        # @.str.1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.1:
	.asciz	"Hello World!"
	.size	.L.str.1, 13

	.section	".note.GNU-stack","",@progbits
