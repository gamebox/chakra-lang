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
	.globl	init                    # -- Begin function init
	.p2align	4, 0x90
	.type	init,@function
init:                                   # @init
	.cfi_startproc
# %bb.0:                                # %entry
	movq	(%rdi), %rdi
	movq	Chakra_stdlib__io(%rip), %rax
	movl	$.L.const.0, %esi
	jmpq	*%rax                   # TAILCALL
.Lfunc_end1:
	.size	init, .Lfunc_end1-init
	.cfi_endproc
                                        # -- End function
	.globl	test_other__receive     # -- Begin function test_other__receive
	.p2align	4, 0x90
	.type	test_other__receive,@function
test_other__receive:                    # @test_other__receive
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
	je	.LBB2_3
# %bb.1:
	cmpq	$100, %rdi
	jne	.LBB2_5
# %bb.2:
	movl	$101, %r14d
	jmp	.LBB2_4
.LBB2_3:
	movl	$100, %r14d
.LBB2_4:
	callq	Chakra_stdlib__commands__none
	movq	%rax, %rbx
	movl	$16, %edi
	callq	malloc
	movq	%r14, (%rax)
	movq	%rbx, 8(%rax)
	jmp	.LBB2_6
.LBB2_5:
	xorl	%eax, %eax
.LBB2_6:
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	test_other__receive, .Lfunc_end2-test_other__receive
	.cfi_endproc
                                        # -- End function
	.globl	test_other__receive_ACTOR # -- Begin function test_other__receive_ACTOR
	.p2align	4, 0x90
	.type	test_other__receive_ACTOR,@function
test_other__receive_ACTOR:              # @test_other__receive_ACTOR
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rdi
	movq	%rdi, %rsi
	callq	test_other__receive
	movl	$16, %edi
	popq	%rax
	.cfi_def_cfa_offset 8
	jmp	malloc                  # TAILCALL
.Lfunc_end3:
	.size	test_other__receive_ACTOR, .Lfunc_end3-test_other__receive_ACTOR
	.cfi_endproc
                                        # -- End function
	.type	.L.str.1,@object        # @.str.1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.1:
	.asciz	"Hello World!"
	.size	.L.str.1, 13

	.type	Chakra_stdlib__io,@object # @Chakra_stdlib__io
	.data
	.globl	Chakra_stdlib__io
	.p2align	3
Chakra_stdlib__io:
	.quad	Chakra_stdlib__print
	.size	Chakra_stdlib__io, 8

	.type	.L.const.0,@object      # @.const.0
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.const.0:
	.asciz	"Hello World!"
	.size	.L.const.0, 13

	.section	".note.GNU-stack","",@progbits
