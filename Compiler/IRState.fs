module IRState

type LLVMIdentifier =
    | GlobalId of string
    | LocalReg of int

type IREnv = Env.Env<LLVMIdentifier * TypeSystem.Type>
type GenericPrototype =
    {   GenericArgTys: TypeSystem.Type list
        GenericRetTy: TypeSystem.Type
        Instantiations: Set<Map<TypeSystem.Type, TypeSystem.Type>> }

type EditingBlockState =
    {   IRCurrentFunction: Llvm.Func
        IRCurrentBlock: Llvm.BasicBlock
        ChakraCurrentModule: string
        IRCurrentModule: Llvm.Module
        IRGenerics: Map<string, GenericPrototype>
        LastInstruction: LLVMIdentifier
        Env: IREnv }

type EditingFuncState =
    {   IRCurrentFunction: Llvm.Func
        ChakraCurrentModule: string
        IRCurrentModule: Llvm.Module
        IRGenerics: Map<string, GenericPrototype>
        LastInstruction: LLVMIdentifier
        Env: IREnv  } 

type EditingModState =
    {   ChakraCurrentModule: string
        IRCurrentModule: Llvm.Module
        IRGenerics: Map<string, GenericPrototype>
        Env: IREnv }

type InitState =
    {   IRCurrentModule: Llvm.Module
        IRGenerics: Map<string, GenericPrototype>
        Env: IREnv }

type IRState =
    | Init of InitState
    | EditingMod of EditingModState
    | EditingFunc of EditingFuncState
    | EditingBlock of EditingBlockState

let empty triple datalayout =
    Init
        {   IRCurrentModule = Llvm.init "" ""
            IRGenerics = Map.empty
            Env = Env.empty () }



(********************************************************
*
*  Init operations
*
********************************************************)



let openModule modName state =
    printfn "openingModule %s" modName
    match state with
    | Init s ->
        EditingMod
            {   IRCurrentModule = s.IRCurrentModule
                ChakraCurrentModule = modName
                IRGenerics = s.IRGenerics
                Env = s.Env }
        |> Some
    | EditingMod s ->
        EditingMod
            {   IRCurrentModule = s.IRCurrentModule
                ChakraCurrentModule = modName
                IRGenerics = s.IRGenerics
                Env = s.Env }
        |> Some
    | _ -> None

let registerPrototype name args ret (state: IRState) =
    let prototype =
        {   GenericArgTys = args
            GenericRetTy = ret
            Instantiations = Set.empty }
    match state with
    | Init s -> Init { s with IRGenerics = Map.add name prototype s.IRGenerics }
    | EditingMod s -> EditingMod { s with IRGenerics = Map.add name prototype s.IRGenerics }
    | EditingFunc s -> EditingFunc { s with IRGenerics = Map.add name prototype s.IRGenerics }
    | EditingBlock s -> EditingBlock { s with IRGenerics = Map.add name prototype s.IRGenerics }

let print (state: IRState) =
    match state with
    | Init { IRCurrentModule = m } ->
        Llvm.print m
        |> Ok
    | _ -> Error "Could not print module while not in Init state"



(********************************************************
*
*  Editing Module operations
*
********************************************************)


let unloadModule state =
    match state with
    | EditingMod s ->
        Init
            {   IRCurrentModule = s.IRCurrentModule
                IRGenerics = s.IRGenerics
                Env = s.Env }
        |> Some
    | _ -> None


let addFunction name ret argTypes state=
    match state with
    | EditingMod s ->
        printfn "Adding a function for %s" name
        let func = Llvm.func s.ChakraCurrentModule name ret argTypes
        EditingFunc
            {   IRCurrentFunction = func
                ChakraCurrentModule = s.ChakraCurrentModule
                IRCurrentModule = s.IRCurrentModule
                IRGenerics = s.IRGenerics
                LastInstruction = LocalReg 0
                Env = s.Env} 
        |> Some
    | _ -> None

let registerBinding ctx reg ty state =
    match state with
    | EditingMod s ->
        Some (EditingMod { s with Env = Env.add (ctx, (reg, ty)) s.Env })
    | EditingFunc s ->
        Some (EditingFunc { s with Env = Env.add (ctx, (reg, ty)) s.Env })
    | EditingBlock s ->
        Some (EditingBlock { s with Env = Env.add (ctx, (reg, ty)) s.Env })
    | _ -> None

(********************************************************
*
*  Editing Function operations
*
********************************************************)


let editBasicBlock state =
    match state with
    | EditingFunc s ->
        let block = s.IRCurrentFunction.EntryBlock
        EditingBlock
            {   IRCurrentFunction = s.IRCurrentFunction
                ChakraCurrentModule = s.ChakraCurrentModule
                IRCurrentModule = s.IRCurrentModule
                IRGenerics = s.IRGenerics
                Env = s.Env
                LastInstruction = s.LastInstruction
                IRCurrentBlock = block } 
        |> Some
    | _ -> None


let completeFunction state =
    match state with
    | EditingFunc s ->
        printfn "Completing current function %s" s.IRCurrentFunction.FuncName
        let m =
            { s.IRCurrentModule with
                Functions = s.IRCurrentFunction::s.IRCurrentModule.Functions }
        EditingMod
            {   IRCurrentModule = m
                ChakraCurrentModule = s.ChakraCurrentModule
                IRGenerics = s.IRGenerics
                Env = s.Env }
        |> Some
     | _ -> None


let lastInstruction state =
    match state with
    | EditingFunc s -> Some (s.LastInstruction)
    | EditingBlock s -> Some (s.LastInstruction)
    | _ -> None

let createScope state =
    match state with
    | EditingFunc s ->
        Some (EditingFunc { s with Env = Env.pushScope [] s.Env })
    | EditingBlock s ->
        Some (EditingBlock { s with Env = Env.pushScope [] s.Env })
    | _ -> None

let destroyScope state =
    match state with
    | EditingFunc s ->
        Some (EditingFunc { s with Env = Env.popScope s.Env })
    | EditingBlock s ->
        Some (EditingBlock { s with Env = Env.popScope s.Env })
    | _ -> None

let findIdentifierForVar name state =
    match state with
    | EditingFunc s ->
        Env.find name s.Env
    | EditingBlock s ->
        Env.find name s.Env
    | _ -> None

(********************************************************
*
*  Editing block operations
*
********************************************************)


let completeEntryBlockWithRet state =
    match state with
    | EditingBlock s ->
        let block = s.IRCurrentBlock
        let func = 
            { s.IRCurrentFunction with
                EntryBlock = block
            }
        EditingFunc
            {   IRCurrentFunction = func
                ChakraCurrentModule = s.ChakraCurrentModule
                IRCurrentModule = s.IRCurrentModule
                IRGenerics = s.IRGenerics
                LastInstruction = s.LastInstruction
                Env = s.Env } 
        |> Some
    | _ -> None

let addPrototypeInstance name mappings state =
    match state with
    | EditingBlock s -> 
        Map.tryFind name s.IRGenerics
        |> Option.map (fun prototype ->
            let p = { prototype with Instantiations = Set.add mappings prototype.Instantiations }
            EditingBlock { s with IRGenerics = Map.add name p s.IRGenerics })
    | _ -> None