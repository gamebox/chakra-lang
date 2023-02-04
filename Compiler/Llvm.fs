module Llvm


type Identifier =
    | GlobalId of string
    | LocalReg of int
    | NumberLiteral of System.Decimal

type Index =
    | PtrIndex of int
    | StructIndex of int

type InstructionType =
    | Call of ret: TypeSystem.Type * func: Identifier * args: (Identifier * TypeSystem.Type) list
    | GetElementPtr of id: Identifier * ty: TypeSystem.Type * indexes: Index list
    | Load of ty: TypeSystem.Type * source: Identifier

type TerminalInstructionType =
    | Ret of ty: TypeSystem.Type * id: Identifier
    | Br
    | Switch

type Instruction =
    { Register: int
      Instruction: InstructionType }

type BasicBlock =
    { Label: string
      Instructions: Instruction list
      Terminator: TerminalInstructionType }

type Func =
    { FuncName: string
      ModName: string
      Ret: TypeSystem.Type
      Args: TypeSystem.Type list
      EntryBlock: BasicBlock
      OtherBlocks: Map<string, BasicBlock>
      IsRecursive: bool }

type Const =
    | StringConstant of string
    | NumberConstant of System.Decimal

type Module =
    { Constants: Const list
      Functions: Func list
      Target: string
      Datalayout: string }

let init target layout =
    { Constants = []
      Functions = []
      Target = target
      Datalayout = layout }

let func modName funcName ret argTypes =
    { FuncName = funcName
      ModName = modName
      Ret = ret
      Args = argTypes
      EntryBlock =
          { Label = "entry"
            Instructions = []
            Terminator = Ret(TypeSystem.num, LocalReg 0) }
      OtherBlocks = Map.empty
      IsRecursive = false }

let addStringConstant s (m: Module) =
    let n = m.Constants.Length
    let id = sprintf ".const.%i" n |> GlobalId

    (id,
     { m with
           Constants = StringConstant s :: m.Constants })

let addNumberConstant d (m: Module) =
    let id = NumberLiteral d

    (id, m)

let addBasicBlock block (f: Func) =
    { f with
          OtherBlocks = Map.add block.Label block f.OtherBlocks }

let findBasicBlock label (f: Func) =
    if label = "entry" then
        Some f.EntryBlock
    else
        Map.tryFind label f.OtherBlocks

let saveBasicBlock block (f: Func) =
    if block.Label = "entry" then
        { f with EntryBlock = block }
    else
        { f with
              OtherBlocks = Map.add block.Label block f.OtherBlocks }


(********************************************************
*
*  Instructions constructors
*
********************************************************)


let private ins reg it = { Register = reg; Instruction = it }

let globalId = GlobalId
let localReg = LocalReg
let ptrIdx = PtrIndex
let structIdx = StructIndex

let gep id ty indexes reg =
    ins reg (GetElementPtr(id, ty, indexes))

let call retTy func args reg = ins reg (Call(retTy, func, args))
let load ty source reg = ins reg (Load(ty, source))

let ret ty id = Ret(ty, id)


(********************************************************
*
*  Basic Block operations
*
********************************************************)


let addInstructionToBlock ins block =
    { block with
          Instructions = ins :: block.Instructions }



(********************************************************
*
*  Printing
*
********************************************************)

let indent n s =
    sprintf "%s%s" (String.replicate n "    ") s

let stdlib = """
%struct.ActorId = type { i64, i64 }
%struct.Msg = type { i8*, i8* }
%struct.Envelope = type { %struct.ActorId, %struct.Msg }
%struct.MainActor = type { %struct.Envelope* ({i64}*)* }
%struct.Capabilities = type { i64 }

@Capabilities = global %struct.Capabilities { i64 0 }
@Chakra_stdlib__io = external global { %struct.Envelope* (i64, i8*)* }
@Chakra_stdlib__string = external global { i8* (i8*)* }
@Chakra_stdlib__format = external global { i8* (double)*, i8* (double)* }
@Chakra_stdlib__math = external global { double (double, double)*, double (double, double)*, double (double, double)*, double (double, double)*, double (double, double)*, double (double)*, double (double)*, double (double)* }
@MainActor = global %struct.MainActor { %struct.Envelope* ({i64}*)* @init }

"""

let rec printChakraType (ty: TypeSystem.Type) =
    match ty with
    | TypeSystem.StructType (fields, _) ->
        List.map (printChakraType << snd) fields
        |> String.concat ", "
        |> sprintf "{ %s }*"
    | TypeSystem.FunctionType (args, ret) ->
        let irArgs =
            List.map (printChakraType << snd) args
            |> String.concat ", "

        sprintf "%s (%s)*" (printChakraType ret) irArgs
    | TypeSystem.StringType _ -> "i8*"
    | TypeSystem.NumberType -> "double"
    | _ ->
        printfn "Got a type that I can't print yet: %O" ty
        raise (System.Exception())

let printConstant (i: int) (c: Const) =
    match c with
    | StringConstant s ->
        sprintf "@.const.%i = private unnamed_addr constant [%i x i8] c\"%s\\00\", align 1" i (s.Length + 1) s
    | NumberConstant d -> sprintf "@.const.%i = private unnamed_addr constant double %f" i (System.Decimal.ToDouble d)

let printId id =
    match id with
    | GlobalId s -> sprintf "@%s" s
    | LocalReg n -> sprintf "%%%i" n
    | NumberLiteral d -> sprintf "%f" (System.Decimal.ToDouble d)

let printTerminal (t: TerminalInstructionType) =
    match t with
    | Ret (ty, id) -> sprintf "ret %s %s" (printChakraType ty) (printId id)

let printIndex i =
    match i with
    | PtrIndex n -> sprintf "i64 %i" n
    | StructIndex n -> sprintf "i32 %i" n

let printCallArg constants (id, ty) =
    match id with
    | GlobalId s ->
        let c = Map.find s constants

        match c with
        | StringConstant s ->
            let len = s.Length + 1

            let gep =
                sprintf "getelementptr inbounds ([%i x i8], [%i x i8]* %s, i64 0, i64 0)" len len (printId id)

            sprintf "%s %s" (printChakraType ty) gep
        | NumberConstant d -> sprintf "double %s" (printId id)
    | LocalReg n -> sprintf "%s %s" (printChakraType ty) (printId id)
    | NumberLiteral d -> sprintf "double %f" (System.Decimal.ToDouble d)

let printInstruction constants (i: Instruction) =
    match i.Instruction with
    | Load (ty, id) ->
        let ty' = (printChakraType ty)
        let id' = (printId id)
        sprintf "%%%i = load %s, %s* %s, align 8" i.Register ty' ty' id'
    | Call (retTy, func, args) ->
        let args' =
            List.map (printCallArg constants) args
            |> String.concat ", "

        sprintf "%%%i = tail call %s %s(%s)" i.Register (printChakraType retTy) (printId func) args'
    | GetElementPtr (id, ty, idxs) ->
        let rawTy = printChakraType ty
        let baseTy = rawTy.[0..(rawTy.Length - 2)]

        let is =
            List.map printIndex ((ptrIdx 0) :: idxs)
            |> String.concat ", "

        sprintf "%%%i = getelementptr inbounds %s, %s %s, %s" i.Register baseTy rawTy (printId id) is

let printBlock constants (b: BasicBlock) =
    let ins =
        List.map ((indent 1) << (printInstruction constants)) (List.rev b.Instructions)
        |> String.concat "\n"

    let term = indent 1 (printTerminal b.Terminator)
    sprintf "%s:\n%s\n%s\n" b.Label ins term

let printArg i ty =
    sprintf "%s %%%i" (printChakraType ty) i

let printFunction constants (f: Func) =
    let printedArgs =
        List.mapi (printArg) f.Args |> String.concat ", "

    let printedBlocks =
        List.map
            (printBlock constants)
            (f.EntryBlock
             :: (Map.toList f.OtherBlocks |> List.map snd))
        |> String.concat "\n"

    sprintf "define noalias %s @%s(%s) {\n%s}" (printChakraType f.Ret) f.FuncName printedArgs printedBlocks

let print (m: Module) =
    let constants =
        List.mapi (printConstant) (List.rev m.Constants)
        |> String.concat "\n"

    let constantMap =
        Map(List.mapi (fun i c -> (sprintf ".const.%i" i, c)) (List.rev m.Constants))

    let functions =
        List.map (printFunction constantMap) m.Functions
        |> String.concat "\n\n"

    // printfn "%s" functions

    sprintf
        "target datalayout=\"%s\"\ntarget triple=\"%s\"\n\n%s\n\n%s\n\n\n%s"
        m.Datalayout
        m.Target
        stdlib
        constants
        functions
