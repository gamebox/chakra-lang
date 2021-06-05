module IRState

type EditingBlockState =
    {   IRCurrentFunction: Llvm.Func
        IRCurrentBlock: Llvm.BasicBlock
        ChakraCurrentModule: string
        IRCurrentModule: Llvm.Module
        Env: Env.Env<string> }

type EditingFuncState =
    {   IRCurrentFunction: Llvm.Func
        ChakraCurrentModule: string
        IRCurrentModule: Llvm.Module
        Env: Env.Env<string>  } 

type EditingModState =
    {   ChakraCurrentModule: string
        IRCurrentModule: Llvm.Module
        Env: Env.Env<string> }

type InitState =
    { IRCurrentModule: Llvm.Module
      Env: Env.Env<string> }

type IRState =
    | Init of InitState
    | EditingMod of EditingModState
    | EditingFunc of EditingFuncState
    | EditingBlock of EditingBlockState

let empty triple datalayout =
    Init
        {   IRCurrentModule = Llvm.init "" ""
            Env = Env.empty () }



(********************************************************
*
*  Init operations
*
********************************************************)



let openModule modName state =
    printfn "openingModule %s" modName
    match state with
    | Init { IRCurrentModule = m; Env = e } ->
        EditingMod
            {   IRCurrentModule = m
                ChakraCurrentModule = modName
                Env = e }
        |> Some
    | EditingMod { IRCurrentModule = m; Env = e  } ->
        EditingMod
            {   IRCurrentModule = m
                ChakraCurrentModule = modName
                Env = e }
        |> Some
    | _ -> None


let print (state: IRState) =
    match state with
    | Init { IRCurrentModule = m } ->
        printfn "This should print"
        Llvm.print m
        |> Some
    | _ -> None



(********************************************************
*
*  Editing Module operations
*
********************************************************)


let unloadModule state =
    match state with
    | EditingMod modState ->
        Init
            {   IRCurrentModule = modState.IRCurrentModule
                Env = modState.Env }
        |> Some
    | _ -> None


let addFunction name ret argTypes state=
    match state with
    | EditingMod modState ->
        printfn "Adding a function for %s" name
        let func = Llvm.func modState.ChakraCurrentModule name ret argTypes
        EditingFunc
            {   IRCurrentFunction = func
                ChakraCurrentModule = modState.ChakraCurrentModule
                IRCurrentModule = modState.IRCurrentModule
                Env = modState.Env} 
        |> Some
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
                Env = s.Env
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
                Env = s.Env }
        |> Some
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
                Env = s.Env } 
        |> Some
    | _ -> None
