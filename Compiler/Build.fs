module Build

open System.IO
open System.Text.RegularExpressions
open ParserLibrary
open ChakraParser
open AST
open Pretty
open TypedAST

let rec gatherFiles directory =
    let dirs =
        Directory.EnumerateDirectories(directory)
        |> Seq.map gatherFiles

    let files =
        Directory.EnumerateFiles(directory)
        |> Seq.filter (fun f -> f.EndsWith(".chakra"))
        |> Seq.toList

    List.concat [ files; List.concat dirs ]

type Project = Project of projectName: string * root: string * version: string

type ParsedModules = Map<string, ChakraModule>

type ParsedProject = ParsedProject of proj: Project * modules: ParsedModules

type VerifiedModules = Map<string, TCModule>

type VerifiedProject = VerifiedProject of proj: Project * modules: VerifiedModules

type LoweredProject = LoweredProject of proj: Project * llvm: string

type BuildError =
    | BuildIOError of string
    | BuildConfigNoNameError
    | BuildParseError of string * (ParserLabel * ParserError * ParserPosition)
    | BuildImportError
    | BuildTypeError of TypeError.TypeError list
    | BuildIRError
    | BuildLinkError
    | BuildCompileError

let (.>>.) res fn = Result.bind fn res

let printPhase phase = printfn "%s" (CConsole.blue phase)

let fileContents path =
    try
        Ok(File.ReadAllLines path)
        |> Result.map (String.concat "\n")
    with _ -> Error(BuildIOError path)

let parseFile fileParser file =
    run fileParser file
    |> toResult
    |> Result.map fst
    |> Result.mapError (fun e -> BuildParseError("", e))

let metadataPath projectPath =
    Path.Combine [| projectPath
                    "meta.chakra" |]

let projectFromMetadata projectPath =
    printPhase "Gathering Metadata"
    let metadataFilePath = metadataPath projectPath
    let parseMetadata = parseFile chakraMetdata

    let extractMetadataFromParsed parsed =
        match (Map.tryFind "name" parsed, Map.tryFind "version" parsed) with
        | (Some (ChakraString (_, name)), Some (ChakraString (s, v))) -> Ok(Project(name, projectPath, v))
        | (Some (ChakraString (_, name)), _) -> Ok(Project(name, projectPath, "0.0.1"))
        | (a, b) ->
            printfn "%O\n%O" a b
            Error BuildConfigNoNameError

    fileContents metadataFilePath
    .>>. parseMetadata
    .>>. extractMetadataFromParsed

let modName (file: string) (root: string) =
    file.Replace('\\', '/')
    |> fun x -> x.Replace(root, "")

let parseProjectFiles (Project (name, root, v)) =
    printPhase "Parsing"

    let rec collectFiles path : string list =
        if File.GetAttributes path = FileAttributes.Directory then
            let files = Directory.GetFiles path |> List.ofSeq

            let dirs =
                Directory.GetDirectories path |> List.ofSeq

            match dirs with
            | [] -> files
            | _ -> List.fold (fun acc p -> List.concat [ acc; collectFiles p ]) files dirs
        else
            []

    let parseAllFiles res filePath =
        let consRight tail h = h :: tail
        let pair l r = (l, r)

        res
        .>>. (fun files ->
            (fileContents filePath)
            .>>. (fun file ->
                (parseFile (chakraModule (modName filePath root)) file)
                |> Result.mapError
                    (fun e ->
                        match e with
                        | BuildParseError (_, e') -> BuildParseError((modName filePath root), e')
                        | _ -> e))
            |> Result.map ((consRight files) << (pair filePath)))



    collectFiles (Path.Combine(root, "libs"))
    |> List.fold parseAllFiles (Ok [])
    |> Result.map
        (fun pairs ->
            ParsedProject (Project (name, root, v), Map pairs))

let importedModules ({ Imports = imports }: ChakraModule) modulePath =
    List.map
        (fun im ->
            match im with
            | ChakraLocalImport i ->
                if i.Relative then
                    i.Library
                else
                    sprintf "/libs/%s" i.Library
            | ChakraPackageImport i -> sprintf "/pkgs%s" i.PackageName)
        imports
    |> List.filter ((<>) "/pkgs/stdlib")

let relativePath root path = Path.GetRelativePath(root, path)

let verifyProject
    (ParsedProject (Project (name, root, v), modules))
    =
    printPhase "Verifying"

    let sortResult =
        Map.toList modules
        |> List.map (fun (s, m) -> (s, m, importedModules m s))
        |> Graph.withNodes<string, ChakraModule>
        |> Graph.sort

    match sortResult with
    | Some sortedModules ->
        let (stdlib: TCModule) =
            { DocComments = None
              Bindings = []
              ExportMap = Map(Stdlib.stdlibExports)
              Imports = [] }

        let blah (acc: Result<Map<string, TCModule>, TypeError.TypeError>) (path, module') =
            match acc with
            | Ok envs ->
                Annotate.annotate path module' envs
                |> Result.map (fun e -> Map.add path e envs)
            | _ ->
                printf "Skipping annotation for %s" path
                acc

        // printfn "%O" (List.map ((modName root) << (relativePath root) << fst) sortedModules)

        List.fold blah (Ok(Map [ "/stdlib", stdlib ])) sortedModules
        |> Result.mapError (fun e -> BuildTypeError [ e ])
        |> Result.map (fun mods -> VerifiedProject (Project (name, root, v), mods))
    | None -> Error BuildImportError

let generateIR (VerifiedProject (p, mods)) =
    printPhase "Generating IR"
    Generate.generate mods (Set.empty)
    |> Result.mapError (fun _ -> BuildIRError)
    |> Result.map (fun llvm -> LoweredProject (p, llvm))

let writeToDisk (LoweredProject (Project (name, root, v), llvm)) =
    printPhase "Writing to disk"
    // Make sure the local has a bin directory, if not create it
    // Write the file to "PROJECT_NAME.ll"
    let buildDir = sprintf "%s/.build" root
    if not (System.IO.Directory.Exists buildDir) then
       System.IO.Directory.CreateDirectory (buildDir)
       |> ignore

    let llvmPath = sprintf "%s/%s.ll" buildDir name
    System.IO.File.WriteAllText (llvmPath, llvm)
    Ok llvmPath

let linkAndCompile proj =
    printPhase "Compiling executable"
    printfn "Makefile exists: %s" (System.IO.Path.GetFullPath ("./Makefile"))
    printfn "Makefile exists: %O" (System.IO.File.Exists ("./Makefile"))
    Ok ()

let build optPath =
    let buildResult =
        Option.defaultWith Directory.GetCurrentDirectory optPath
        |> Path.GetFullPath
        |> projectFromMetadata
        .>>. parseProjectFiles
        .>>. verifyProject
        .>>. generateIR
        .>>. writeToDisk
        .>>. linkAndCompile

    match buildResult with
    | Ok () -> printPhase "DONE"
    | Error err -> printfn "Build Error: %O" err
