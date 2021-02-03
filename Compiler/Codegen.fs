module Codegen

type LLVMVec = {
    NumElements: uint
    Type: LLVMType
}
and LLVMFuncType = {
    Ret: LLVMType
    Args: LLVMType list
}
and LLVMType =
    | LLVMVoid
    | LLVMFuncType of LLVMFuncType
    | LLVMInt of uint
    | LLVMHalf
    | LLVMFloat
    | LLVMDouble
    | LLVMPointer of LLVMType
    | LLVMCustom of string
    | LLVMFixedVec of LLVMVec
    | LLVMScaledVec of LLVMVec
    | LLVMArray of LLVMVec

(*
    Boolean constants
    The two strings ‘true’ and ‘false’ are both valid constants of the i1 type.
    Integer constants
    Standard integers (such as ‘4’) are constants of the integer type. Negative numbers may be used with integer types.
    Floating-point constants
    Floating-point constants use standard decimal notation (e.g. 123.421), exponential notation (e.g. 1.23421e+2), or a more precise hexadecimal notation (see below). The assembler requires the exact decimal value of a floating-point constant. For example, the assembler accepts 1.25 but rejects 1.3 because 1.3 is a repeating decimal in binary. Floating-point constants must have a floating-point type.
    Null pointer constants
    The identifier ‘null’ is recognized as a null pointer constant and must be of pointer type.
    Token constants NOT SUPPORTED
    The identifier ‘none’ is recognized as an empty token constant and must be of token type.
    Structure constants
    Structure constants are represented with notation similar to structure type definitions (a comma separated list of elements, surrounded by braces ({})). For example: “{ i32 4, float 17.0, i32* @G }”, where “@G” is declared as “@G = external global i32”. Structure constants must have structure type, and the number and types of elements must match those specified by the type.
    Array constants
    Array constants are represented with notation similar to array type definitions (a comma separated list of elements, surrounded by square brackets ([])). For example: “[ i32 42, i32 11, i32 74 ]”. Array constants must have array type, and the number and types of elements must match those specified by the type. As a special case, character array constants may also be represented as a double-quoted string using the c prefix. For example: “c"Hello World\0A\00"”.
    Vector constants
    Vector constants are represented with notation similar to vector type definitions (a comma separated list of elements, surrounded by less-than/greater-than’s (<>)). For example: “< i32 42, i32 11, i32 74, i32 100 >”. Vector constants must have vector type, and the number and types of elements must match those specified by the type.

*)
type LLVMConstant =
    | LLVMBoolConstant of bool
    | LLVMIntConstant of int
    | LLVMFloatConstant of float
    | LLVMNullP
    | LLVMStructureConstant of LLVMConstant list
    | LLVMArrayConstant of LLVMConstant list
    | LLVMVectorConstant of LLVMConstant list

(*
    Parameter Attributes
    --------------------

    preallocated(<ty>)
    sret(<ty>)
    align(<n>)
    noalias
    nocapture
    nofree
    nest
    returned
    nonnull
    dereferenceable(<n>)
    dereferenceable_or_null(<n>)
    swiftself
    swifterror
    immarg
    noundef
*)
type LLVMParamAttr =
    | LLVMPreallocated of LLVMType
    | LLVMSretTy
    | LLVMAlignN
    | LLVMNoalias
    | LLVMNocapture
    | LLVMNofree
    | LLVMNest
    | LLVMReturned
    | LLVMNonnull
    | LLVMDereferenceable of uint
    | LLVMDereferenceableOrNull of uint
    | LLVMSwiftself
    | LLVMSwifterror
    | LLVMImmarg
    | LLVMNoundef

(*
    Function Attributes
    --------------------

    alignstack(<n>)
    allocsize(<EltSizeParam>[, <NumEltsParam>])
    alwaysinline
    builtin
    cold
    convergent
    hot
    inaccessiblememonly
    inaccessiblemem_or_argmemonly
    inlinehint
    jumptable
    minsize
    naked
    "no-inline-line-tables"
    no-jump-tables
    nobuiltin
    noduplicate
    nofree
    noimplicitfloat
    noinline
    nomerge
    nonlazybind
    noredzone
    indirect-tls-seg-refs
    noreturn
    norecurse
    willreturn
    nosync
    nounwind
    null_pointer_is_valid
    optforfuzzing
    optnone
    optsize
    "patchable-function"
    "probe-stack"
    readnone
    readonly
    "stack-probe-size"
    "no-stack-arg-probe"
    writeonly
    argmemonly
    returns_twice
    safestack
    sanitize_address
    sanitize_memory
    sanitize_thread
    sanitize_hwaddress
    sanitize_memtag
    speculative_load_hardening
    speculatable
    ssp
    sspstrong
    sspreq
    strictfp
    "denormal-fp-math"
    "denormal-fp-math-f32"
    "thunk"
    uwtable
    nocf_check
    shadowcallstack
    mustprogress
*)
type LLVMFnAttr =
    | LLVMalignstack of uint
    | LLVMallocsize of (uint * uint option)
    | LLVMAlwaysinline
    | LLVMBuiltin
    | LLVMCold
    | LLVMConvergent
    | LLVMHot
    | LLVMInaccessiblememonly
    | LLVMInaccessiblememOrArgmemonly
    | LLVMInlinehint
    | LLVMJumptable
    | LLVMMinsize
    | LLVMNaked
    | LLVMNoInlineLineTables
    | LLVMNoJumpTables
    | LLVMNobuiltin
    | LLVMNoduplicate
    | LLVMNofree
    | LLVMNoimplicitfloat
    | LLVMNoinline
    | LLVMNomerge
    | LLVMNonlazybind
    | LLVMNoredzone
    | LLVMIndirectTlsSegRefs
    | LLVMNoreturn
    | LLVMNorecurse
    | LLVMWillreturn
    | LLVMNosync
    | LLVMNounwind
    | LLVMNullPointerIsValid
    | LLVMOptforfuzzing
    | LLVMOptnone
    | LLVMOptsize
    | LLVMPatchableFunction
    | LLVMProbeStack
    | LLVMReadnone
    | LLVMReadonly
    | LLVMStackProbeSize
    | LLVMNoStackArgProbe
    | LLVMWriteonly
    | LLVMArgmemonly
    | LLVMReturnsTwice
    | LLVMSafestack
    | LLVMSanitizeAddress
    | LLVMSanitizeMemory
    | LLVMSanitizeThread
    | LLVMSanitizeHwaddress
    | LLVMSanitizeMemtag
    | LLVMSpeculativeLoadHardening
    | LLVMSpeculatable
    | LLVMSsp
    | LLVMSspstrong
    | LLVMSspreq
    | LLVMStrictfp
    | LLVMDenormalFpMath
    | LLVMDenormalFpMathF32
    | LLVMThunk
    | LLVMUwtable
    | LLVMNocfCheck
    | LLVMShadowcallstack
    | LLVMMustprogress

type LLVMTypeInfo =
    | LLVMOpaqueType
    | LLVMStructType of LLVMType list

type LLVMTypedef = {
    Name: string
    Type: LLVMTypeInfo
}

type LLVMGlobalVar = LLVMGlobalVar

(*
    Linkage Types
    --------------------

    private
    internal
    available_externally
    linkonce
    weak
    common
    appending
    extern_weak
    linkonce_odr
    weak_odr
    external
*)
type LLVMLinkage = 
    | LLVMLinkageNone
    | LLVMLinkagePrivate
    | LLVMInternal
    | LLVMAvailableExternally
    | LLVMLinkonce
    | LLVMWeak
    | LLVMCommon
    | LLVMAppending
    | LLVMExternWeak
    | LLVMLinkonceODR
    | LLVMWeakODR
    | LLVMExternal

(*
    Runtime Preemption Specifiers
    --------------------

    dso_preemptable
    dso_local
*)
type LLVMPreemptionSpecifier =
    | LLVMPreemptionNone
    | LLVMDsoPreemtable
    | LLVMDsoLocal

(*
    Visibility Styles
    --------------------

    default
    hidden
    protected
*)
type LLVMVisibility =
    | LLVMVisibilityDefault
    | LLVMVisibilityHidden
    | LLVMVisibilityProtected

(*
    DLL Storage Classes
    --------------------

    dllimport
    dllexport
*)
type LLVMDllStorageClass =
    | LLVMDllNone
    | LLVMDllImport
    | LLVMDllExport


type LLVMAddr =
    | LLVMAddrNone
    | LLVMAddrUnnamed
    | LLVMAddrLocalUnnamed

type LLVMFnArg = {
    Name: string
    Attrs: LLVMParamAttr // attrs
    Type: LLVMType
}

(*
    Calling Conventions
    --------------------

    ccc
    fastcc
    coldcc
    cc 10
    cc 11
    webkit_jscc
    anyregcc
    preserve_mostcc
    preserve_allcc
    cxx_fast_tlscc
    swiftcc
    tailcc
    cfguard_checkcc
    cc <n>
*)
type LLVMCallingConvention =
    | LLVMCConvNone
    | LLVMCConvCCC
    | LLVMCConvFastCC
    | LLVMColdcc
    | LLVMCC10
    | LLVMCC11
    | LLVMWebkitJscc
    | LLVMAnyregcc
    | LLVMPreserveMostcc
    | LLVMPreserveAllcc
    | LLVMCXXFastTlscc
    | LLVMSwiftcc
    | LLVMTailcc
    | LLVMCfguardCheckcc

(*
    'ret' Instruction
    ret <type> <value>       ; Return a value from a non-void function
    ret void                 ; Return from void function


    ‘br’ Instruction
    br i1 <cond>, label <iftrue>, label <iffalse>
    br label <dest>          ; Unconditional branch


    ‘switch’ Instruction
    switch <intty> <value>, label <defaultdest> [ <intty> <val>, label <dest> ... ]


    ‘indirectbr’ Instruction
    indirectbr <somety>* <address>, [ label <dest1>, label <dest2>, ... ]


    ‘invoke’ Instruction
    <result> = invoke [cconv] [ret attrs] [addrspace(<num>)] <ty>|<fnty> <fnptrval>(<function args>) [fn attrs]
                  [operand bundles] to label <normal label> unwind label <exception label>


    ‘callbr’ Instruction
    <result> = callbr [cconv] [ret attrs] [addrspace(<num>)] <ty>|<fnty> <fnptrval>(<function args>) [fn attrs]
                  [operand bundles] to label <fallthrough label> [indirect labels]


    ‘resume’ Instruction
    resume <type> <value>


    ‘catchswitch’ Instruction
    <resultval> = catchswitch within <parent> [ label <handler1>, label <handler2>, ... ] unwind to caller
    <resultval> = catchswitch within <parent> [ label <handler1>, label <handler2>, ... ] unwind label <default>


    ‘catchret’ Instruction
    catchret from <token> to label <normal>


    ‘cleanupret’ Instruction
    cleanupret from <value> unwind label <continue>
    cleanupret from <value> unwind to caller


    ‘unreachable’ Instruction
    unreachable
*)
type LLVMRegister = LLVMRegister of string

type LLVMBrInstr = {
    Cond: LLVMRegister
    True: LLVMRegister
    False: LLVMRegister
}

type LLVMTerminalInstruction = 
    | LLVMRetInstruction of LLVMType * LLVMConstant
    | LLVMBrInstruction of LLVMBrInstr
    // | LLVMSwitchInstruction
    // | LLVMIndirectBrInstruction
    // | LLVMInvokeInstruction
    // | LLVMCallbrInstruction
    // | LLVMResumeInstruction
    // | LLVMCatchswitchInstruction
    // | LLVMCatchretInstruction
    // | LLVMCleanupretInstruction
    | LLVMUnreachableInstruction

(*
    ‘shl’ Instruction
    <result> = shl <ty> <op1>, <op2>           ; yields ty:result
    <result> = shl nuw <ty> <op1>, <op2>       ; yields ty:result
    <result> = shl nsw <ty> <op1>, <op2>       ; yields ty:result
    <result> = shl nuw nsw <ty> <op1>, <op2>   ; yields ty:result


    ‘lshr’ Instruction
    <result> = lshr <ty> <op1>, <op2>         ; yields ty:result
    <result> = lshr exact <ty> <op1>, <op2>   ; yields ty:result


    ‘ashr’ Instruction
    <result> = ashr <ty> <op1>, <op2>         ; yields ty:result
    <result> = ashr exact <ty> <op1>, <op2>   ; yields ty:result


    ‘and’ Instruction
    <result> = and <ty> <op1>, <op2>   ; yields ty:result


    ‘or’ Instruction
    <result> = or <ty> <op1>, <op2>   ; yields ty:result


    ‘xor’ Instruction
    <result> = xor <ty> <op1>, <op2>   ; yields ty:result

    ‘extractvalue’ Instruction
    <result> = extractvalue <aggregate type> <val>, <idx>{, <idx>}*


    ‘insertvalue’ Instruction
    <result> = insertvalue <aggregate type> <val>, <ty> <elt>, <idx>{, <idx>}*    ; yields <aggregate type>

    ‘alloca’ Instruction
    <result> = alloca [inalloca] <type> [, <ty> <NumElements>] [, align <alignment>] [, addrspace(<num>)]     ; yields type addrspace(num)*:result


    ‘load’ Instruction
    <result> = load [volatile] <ty>, <ty>* <pointer>[, align <alignment>][, !nontemporal !<nontemp_node>][, !invariant.load !<empty_node>][, !invariant.group !<empty_node>][, !nonnull !<empty_node>][, !dereferenceable !<deref_bytes_node>][, !dereferenceable_or_null !<deref_bytes_node>][, !align !<align_node>][, !noundef !<empty_node>]
    <result> = load atomic [volatile] <ty>, <ty>* <pointer> [syncscope("<target-scope>")] <ordering>, align <alignment> [, !invariant.group !<empty_node>]
    !<nontemp_node> = !{ i32 1 }
    !<empty_node> = !{}
    !<deref_bytes_node> = !{ i64 <dereferenceable_bytes> }
    !<align_node> = !{ i64 <value_alignment> }


    ‘store’ Instruction
    store [volatile] <ty> <value>, <ty>* <pointer>[, align <alignment>][, !nontemporal !<nontemp_node>][, !invariant.group !<empty_node>]        ; yields void
    store atomic [volatile] <ty> <value>, <ty>* <pointer> [syncscope("<target-scope>")] <ordering>, align <alignment> [, !invariant.group !<empty_node>] ; yields void
    !<nontemp_node> = !{ i32 1 }
    !<empty_node> = !{}


    ‘fence’ Instruction
    fence [syncscope("<target-scope>")] <ordering>  ; yields void


    ‘cmpxchg’ Instruction
    cmpxchg [weak] [volatile] <ty>* <pointer>, <ty> <cmp>, <ty> <new> [syncscope("<target-scope>")] <success ordering> <failure ordering> ; yields  { ty, i1 }


    ‘atomicrmw’ Instruction
    atomicrmw [volatile] <operation> <ty>* <pointer>, <ty> <value> [syncscope("<target-scope>")] <ordering>                   ; yields ty


    ‘getelementptr’ Instruction
    <result> = getelementptr <ty>, <ty>* <ptrval>{, [inrange] <ty> <idx>}*
    <result> = getelementptr inbounds <ty>, <ty>* <ptrval>{, [inrange] <ty> <idx>}*
    <result> = getelementptr <ty>, <ptr vector> <ptrval>, [inrange] <vector index type> <idx>
*)
type LLVMInstruction =
    | LLVMExtractValueInstruction
    | LLVMInsertValueInstruction
    | LLVMAllocaInstruction
    | LLVMLoadInstruction
    | LLVMStoreInstruction
    | LLVMGepInstruction


type LLVMEntryBlock = {
    Instructions: LLVMInstruction list
    Terminator: LLVMTerminalInstruction
}

type LLVMPhiNode = LLVMPhiNode

type LLVMBasicBlock = {
    Phi: LLVMPhiNode
    Instructions: LLVMInstruction list
    Terminator: LLVMTerminalInstruction
}

(*
    LLVM function declarations consist of the “declare” keyword,
    an optional linkage type, an optional visibility style, an
    optional DLL storage class, an optional calling convention,
    an optional unnamed_addr or local_unnamed_addr attribute, an
    optional address space, a return type, an optional parameter
    attribute for the return type, a function name, a possibly
    empty list of arguments, an optional alignment, an optional
    garbage collector name, an optional prefix, and an optional
    prologue.
*)
type LLVMDeclaration = {
    Linkage: LLVMLinkage // [linkage]
    Visibility: LLVMVisibility // [visibility]
    DllStorageClass: LLVMDllStorageClass // [DLLStorageClass]
    CallingConvention: LLVMCallingConvention // [cconv]
    Addr: LLVMAddr // [(unnamed_addr|local_unnamed_addr)]
    // [AddrSpace] NOT SUPPORTED
    RetType: LLVMType // <ResultType>
    RetAttr: LLVMParamAttr
    Name: string // @<FunctionName>
    Args: LLVMFnArg list // ([argument list])
    Alignment: uint // [align N]
    GC: string option // [gc]
}


(*
    LLVM function definitions consist of the “define” keyword, an
    optional linkage type, an optional runtime preemption
    specifier, an optional visibility style, an optional DLL
    storage class, an optional calling convention, an optional
    unnamed_addr attribute, a return type, an optional parameter
    attribute for the return type, a function name, a (possibly
    empty) argument list (each with optional parameter attributes),
    optional function attributes, an optional address space, an
    optional section, an optional alignment, an optional comdat,
    an optional garbage collector name, an optional prefix, an
    optional prologue, an optional personality, an optional list
    of attached metadata, an opening curly brace, a list of basic
    blocks, and a closing curly brace.
*)
type LLVMDefine = {
    Linkage: LLVMLinkage // [linkage]
    PreemptionSpecifier: LLVMPreemptionSpecifier // [PreemptionSpecifier]
    Visibility: LLVMVisibility // [visibility]
    DllStorageClass: LLVMDllStorageClass // [DLLStorageClass]
    CallingConvention: LLVMCallingConvention // [cconv]
    RetAttrs: LLVMParamAttr list
    RetType: LLVMType // <ResultType>
    Name: string // @<FunctionName>
    Args: LLVMFnArg list // ([argument list])
    Addr: LLVMAddr // [(unnamed_addr|local_unnamed_addr)]
    // [AddrSpace] NOT SUPPORTED
    Attrs: LLVMFnAttr list // [fn Attrs]
    // [section "name"] NOT SUPPORTED
    // [comdat [($name)]] NOT SUPPORTED
    Alignment: uint // [align N]
    GC: string option // [gc]
    // [prefix Constant] NOT SUPPORTED
    // [prologue Constant] NOT SUPPORTED
    // [personality Constant] Maybe?
    AttrGroups: string list
    Metadata: string list // (!name !N)*
    Body: LLVMEntryBlock * LLVMBasicBlock list // { ... }
}

type LLVMModule = {
    Typedefs: LLVMModule list
    Globals: LLVMGlobalVar list
    Declarations: LLVMDeclaration list
    Defines: LLVMDefine list
}