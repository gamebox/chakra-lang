module IRState

type EditingBlockState =
    {   IRCurrentFunction: Llvm.Func
        IRCurrentBlock: Llvm.BasicBlock
        ChakraCurrentModule: string
        IRCurrentModule: Llvm.Module }

type EditingFuncState =
    {   IRCurrentFunction: Llvm.Func
        ChakraCurrentModule: string
        IRCurrentModule: Llvm.Module }

type EditingModState =
    {   ChakraCurrentModule: string
        IRCurrentModule: Llvm.Module }

type InitState = { IRCurrentModule: Llvm.Module }

type IRState =
    | Init of InitState
    | EditingMod of EditingModState
    | EditingFunc of EditingFuncState
    | EditingBlock of EditingBlockState

let empty =
    Init {   IRCurrentModule = Llvm.init "" "" }



(********************************************************
*
*  Init operations
*
********************************************************)



let openModule modName state =
    match state with
    | Init { IRCurrentModule = m } ->
        EditingMod
            {   IRCurrentModule = m
                ChakraCurrentModule = modName }
        |> Some
    | EditingMod { IRCurrentModule = m } ->
        EditingMod
            {   IRCurrentModule = m
                ChakraCurrentModule = modName }
        |> Some
    | _ -> None



(********************************************************
*
*  Editing Module operations
*
********************************************************)







(********************************************************
*
*  Editing Function operations
*
********************************************************)








(********************************************************
*
*  Editing block operations
*
********************************************************)



