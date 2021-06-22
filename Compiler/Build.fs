module Build

open System.IO
open System.Text.RegularExpressions
open ChakraFparsec
open AST
open Pretty
open TypedAST

let rec gatherFiles filter directory =
    let dirs =
        Directory.EnumerateDirectories(directory)
        |> Seq.map (gatherFiles filter)

    let files =
        Directory.EnumerateFiles(directory)
        |> Seq.filter filter
        |> Seq.toList

    List.concat [ files; List.concat dirs ]

type Project = Project of projectName: string * root: string * version: string

type ParsedModules = Map<string, ChakraModule>

type ParsedProject = ParsedProject of proj: Project * modules: ParsedModules

type VerifiedModules = Map<string, TCModule>

type VerifiedProject = VerifiedProject of proj: Project * modules: VerifiedModules

type LoweredProject = LoweredProject of proj: Project * llvm: string

type WrittenProject = WrittenProject of proj: Project * llvmPath: string

type BuildError =
    | BuildIOError of string
    | BuildConfigNoNameError
    | BuildParseError of string * (ParserLibrary.ParserLabel * ParserLibrary.ParserError * ParserLibrary.ParserPosition)
    | BuildImportError
    | BuildTypeError of TypeError.TypeError list
    | BuildIRError
    | BuildLinkError
    | BuildCompileError

let (.>>.) res fn = Result.bind fn res

let printPhase phase timed =
    let msg = CConsole.blue phase

    if timed then
        printf "%s" msg
    else
        printfn "%s" msg

let printTime ms =
    printfn "...%s ms" (CConsole.blue (sprintf "%d" ms))

let fileContents path =
    try
        Ok(File.ReadAllLines path)
        |> Result.map (String.concat "\n")
    with _ -> Error(BuildIOError path)

let toResult =
    function
    | FParsec.CharParsers.Success (res, _, _) -> Ok res
    | FParsec.CharParsers.Failure (e, err, state) ->
        let msgs = ""

        Error(
            BuildParseError(
                "",
                (e,
                 msgs,
                 { Line = err.Position.Line |> int
                   Column = err.Position.Column |> int
                   CurrentLine = "" })
            )
        )

let parseFile fileParser file =
    FParsec.CharParsers.run fileParser file
    |> toResult

let metadataPath projectPath =
    Path.Combine [| projectPath
                    "meta.chakra" |]

let projectFromMetadata projectPath =
    printPhase "Gathering Metadata" false
    let metadataFilePath = metadataPath projectPath
    let parseMetadata = parseFile chakraMetdata

    let extractMetadataFromParsed parsed =
        match (Map.tryFind "name" parsed, Map.tryFind "version" parsed) with
        | (Some (ChakraString (_, name)), Some (ChakraString (s, v))) -> Ok(Project(name, projectPath, v))
        | (Some (ChakraString (_, name)), _) -> Ok(Project(name, projectPath, "0.0.1"))
        | (a, b) ->
            // printfn "%O\n%O" a b
            Error BuildConfigNoNameError

    fileContents metadataFilePath
    .>>. parseMetadata
    .>>. extractMetadataFromParsed

let modName (file: string) (root: string) =
    file.Replace('\\', '/')
    |> fun x -> x.Replace(root, "")

let parseProjectFiles (Project (name, root, v)) =
    printPhase "Parsing" true

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
                // printfn "Parsing %s" filePath
                let timer = System.Diagnostics.Stopwatch.StartNew()

                (parseFile (chakraModule (modName filePath root)) file)
                |> Result.map
                    (fun e ->
                        timer.Stop()
                        // printfn "Parsing took %ims" timer.ElapsedMilliseconds
                        e)
                |> Result.mapError
                    (fun e ->
                        timer.Stop()

                        match e with
                        | BuildParseError (_, e') -> BuildParseError((modName filePath root), e')
                        | _ -> e))
            |> Result.map ((consRight files) << (pair filePath)))



    let f =
        collectFiles (Path.Combine(root, "libs"))

    let timer = System.Diagnostics.Stopwatch.StartNew()

    f
    |> List.fold parseAllFiles (Ok [])
    |> Result.map
        (fun pairs ->
            timer.Stop()
            printTime timer.ElapsedMilliseconds
            ParsedProject(Project(name, root, v), Map pairs))

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

let verifyProject (ParsedProject (Project (name, root, v), modules)) =
    printPhase "Verifying" false

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
                // printfn "Annotating module %s\n---------------\n%O" path module'

                Annotate.annotate path module' envs
                |> Result.map (fun e -> Map.add path e envs)
            | _ ->
                printf "Skipping annotation for %s" path
                acc

        // printfn "%O" (List.map ((modName root) << (relativePath root) << fst) sortedModules)

        List.fold blah (Ok(Map [ "/stdlib", stdlib ])) sortedModules
        |> Result.mapError (fun e -> BuildTypeError [ e ])
        |> Result.map (fun mods -> VerifiedProject(Project(name, root, v), mods))
    | None -> Error BuildImportError

let generateIR (VerifiedProject (p, mods)) =
    printPhase "Generating IR" false

    Generate.generate mods (Set.empty)
    |> Result.mapError (fun _ -> BuildIRError)
    |> Result.map (fun llvm -> LoweredProject(p, llvm))

let writeToDisk (LoweredProject (Project (name, root, v), llvm)) =
    printPhase "Writing to disk" false
    // Make sure the local has a bin directory, if not create it
    // Write the file to "PROJECT_NAME.ll"
    let buildDir = sprintf "%s/.build" root

    if not (System.IO.Directory.Exists buildDir) then
        System.IO.Directory.CreateDirectory(buildDir)
        |> ignore

    let llvmPath = sprintf "%s/%s.ll" buildDir name
    System.IO.File.WriteAllText(llvmPath, llvm)
    Ok(WrittenProject(Project(name, root, v), llvmPath))

let linkAndCompile (WrittenProject (Project (name, root, v), llvmPath)) =
    printPhase "Compiling executable" false

    let rtPath =
        System.Environment.GetEnvironmentVariable("CHAKRA_RUNTIME_PATH")
        |> System.IO.Path.GetFullPath

    let inputs =
        gatherFiles (fun f -> f.EndsWith(".c") || f.EndsWith(".h")) rtPath
        |> String.concat " "

    let output = sprintf "%s/.build/%s" root name

    let args =
        sprintf "%s %s -lpthread -lm" inputs llvmPath

    let (time, outputs, errs) = RunProcess.runProc "clang" args

    match Seq.toList errs with
    | [] ->
        System.IO.File.Delete(output)
        System.IO.File.Move("./a.out", output)
        printfn "%s" (String.concat "\n" outputs)

        printfn "Wrote executable '%s' in %ims " output (time)
        |> Ok
    | _ -> Error(BuildIOError(sprintf "Failed to write '%s' after %ims:\n\n%s" output time (String.concat "\n" errs)))

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
    | Ok () -> printPhase "DONE" false
    | Error err ->
        match err with
        | BuildParseError (_, (e, _, _)) -> printfn "Parse error:\n%s" (CConsole.red e)
        | _ -> printfn "Build Error: %O" err
