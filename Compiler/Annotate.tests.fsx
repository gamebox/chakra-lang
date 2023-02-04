#r "nuget: FParsec"
#load "RunProcess.fs"
#load "Operators.fs"
#load "Env.fs"
#load "Graph.fs"
#load "Codegen.fs"
#load "CConsole.fs"
#load "ParserLibrary.fs"
#load "AST.fs"
#load "TypeSystem.fs"
#load "Stdlib.fs"
#load "TypeError.fs"
#load "TypedAST.fs"
#load "ChakraFparsec.fs"
#load "Pretty.fs"
#load "Unify.fs"
#load "TypeGraph.fs"
#load "Annotate.fs"

let trim (addlChars: char array) (s: string) = s.TrimStart([| '\n'; '\t'; ' '|]).TrimEnd(Array.append [| '\t'; ' '|] addlChars)

let bindingType ({ Typ = typ }: TypedAST.TCBinding) = typ

let test (l: string) m env types =
    let checkResult (r: Result<TypedAST.TCModule, TypeError.TypeError>) =
        match r with
        | Result.Ok tm ->
            if List.forall (fun (b, t) -> (bindingType b) = t) (List.zip tm.Bindings types) then
                printfn "%s" (CConsole.green $"Sucess: %s{l}")
            else
                let received = List.map (fun b -> bindingType b) tm.Bindings
                printfn "%s" (CConsole.red $"Failure: %s{l}\n\n%O{received}\n\n")
        | Result.Error (e) ->
            printfn "%s" (CConsole.red $"Type Failure: %s{l}\n%%O{e}")
    let (FParsec.CharParsers.ParserResult.Success (parsed, _, _)) = FParsec.CharParsers.run (ChakraFparsec.chakraModule "Test") m
    Annotate.annotate "Test" parsed env
    |> checkResult


let mod1 =
    """
= %( init )

%( io ) = /stdlib

init(caps) =
    io.print(caps.stdio, "Hello World!")
    """ |> trim [||]

let (stdlib: TypedAST.TCModule) =
    { DocComments = None
      Bindings = []
      ExportMap = Map(Stdlib.stdlibExports)
      Imports = [] }

let mod2 =
    """
= %( goofy )

%( add, div ) = /stdlib

goofy(x) =
    added = add(x, 3)
    div(added, add(x, x))
    """ |> trim [||]

let mod3 =
    """
= %( found-in-list )

%( list, eq? ) = /stdlib

found-in-list(item, l) =
    predicate(acc, i) =
        acc ?
        | #true -> #true
        | #false ->
            eq?(i, item)

    list.fold(predicate, l, #false)
    """ |> trim [||]

let stdlibEnv = Map.empty |> Map.add "stdlib" stdlib
let capsType = TypeSystem.strct ([("stdio", TypeSystem.cap TypeSystem.Capability.StdioCapability)], false, None)
test "Hello world" mod1 stdlibEnv [TypeSystem.fn [("init/caps", capsType)] TypeSystem.cmd]
test "Numbers" mod2 stdlibEnv [TypeSystem.fn [("goofy/x", TypeSystem.num)] TypeSystem.num]
test "Generics" mod3 stdlibEnv [TypeSystem.fn [("found-in-list/item", TypeSystem.genA); ("found-in-list/list", TypeSystem.list TypeSystem.genA)] TypeSystem.bool] 