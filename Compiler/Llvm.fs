module Llvm

type InstructionType =
    | Call
    | GetElementPtr

type TerminalInstructionType =
    | Ret

type Instruction =
    { Register: int
      Instruction: InstructionType }

type BasicBlock =
    { Label: int
      Instructions: Instruction list
      Terminator: TerminalInstructionType }

type Func =
    { FuncName: string
      ModName: string
      Ret: TypeSystem.Type
      Args: TypeSystem.Type list
      EntryBlock: BasicBlock
      OtherBlocks: BasicBlock list
      IsRecursive: bool }

type Const =
    { Constant: string }

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
        { Label = 0
          Instructions = []
          Terminator = Ret }
      OtherBlocks = []
      IsRecursive = false }

(********************************************************
*
*  Printing
*
********************************************************)

let indent n s = sprintf "%s%s" (String.replicate n "    ") s

let rec printChakraType (ty: TypeSystem.Type) =
    match ty with
    | TypeSystem.StructType (fields, _, _) ->
        List.map (printChakraType << snd) fields
        |> String.concat ", "
        |> sprintf "{ %s }"
    | TypeSystem.FunctionType _ -> "type ()"
    | TypeSystem.CapabilityType _ -> "u64"
    | TypeSystem.CommandType _ -> "%Envelope_t"
    | _ -> raise (System.Exception ())

let printConstant (i: int) (c: Const) =
    sprintf "@%i = private unnamed_addr constant [%i x i8] c\"%s\"" i (c.Constant.Length + 1) c.Constant

let printTerminal (t: TerminalInstructionType) =
    match t with
    | Ret -> sprintf "%%%i = ret"

let printInstruction (i: Instruction) =
    match i.Instruction with
    | Call -> sprintf "%%%i = call" i.Register
    | GetElementPtr -> sprintf "%%%i = getelementptr" i.Register

let printBlock (b: BasicBlock) =
    let ins =
        List.map ((indent 1) << printInstruction) b.Instructions
        |> String.concat "\n"
    sprintf "%%b%i:\n%s" b.Label ins

let printArg i ty =
    sprintf "%s %%arg%i" (printChakraType ty) i

let printFunction (f: Func) =
    let printedArgs =
        List.mapi (printArg) f.Args
        |> String.concat ", "
    let printedBlocks =
        List.map (printBlock) (f.EntryBlock::f.OtherBlocks)
        |> String.concat "\n"

    sprintf "define %s @%s(%s) {\n%s}" (printChakraType f.Ret) f.FuncName printedArgs printedBlocks

let print (m: Module) =
    let constants =
        List.mapi (printConstant) m.Constants
        |> String.concat "\n"

    let functions =
        List.map (printFunction) m.Functions
        |> String.concat "\n\n"

    printfn "%s" functions

    sprintf
        "target datalayout=\"%s\"\ntarget triple=\"%s\"\n\n%s\n\n\n%s"
        m.Datalayout
        m.Target
        constants
        functions