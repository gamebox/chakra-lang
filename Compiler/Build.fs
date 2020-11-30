module Build

open System.IO
open System.Text.RegularExpressions
open ParserLibrary
open ChakraParser


let cleaner lines =
    let docCommentRegex = Regex "^;;"
    let lineCommentRegex = Regex ";.*"
    let restoreDocCommentRegex = Regex "^--"

    let replace (regex: Regex) (replacement: string) str =
        regex.Replace(str, replacement)

    let cleanLine line =
        docCommentRegex.Replace (line, "--")
        |> replace lineCommentRegex ""
        |> replace restoreDocCommentRegex ";;"

    Seq.map cleanLine lines
    |> String.concat "\n"

let rec gatherFiles directory =
    let dirs =
        Directory.EnumerateDirectories (directory)
        |> Seq.map gatherFiles
    let files =
        Directory.EnumerateFiles (directory)
        |> Seq.toList

    List.concat
        [ files ; List.concat dirs ]


let build optPath =
    let path = Option.defaultWith Directory.GetCurrentDirectory optPath

    let chakraFilePaths = gatherFiles path

    let fileFolder fileMap file =
        let fileLines = File.ReadAllLines (file)
        Map.add file fileLines fileMap
    
    let fileMap = 
        Seq.fold fileFolder Map.empty chakraFilePaths

    let fileParseResultMapper k (v: string []) =
        run chakraModule (cleaner v)

    let cleanFileMap =
        Map.map fileParseResultMapper fileMap

    let isFailure _ result =
        match result with
        | Failure _ -> true
        | _ -> false

    let (failures, successes) = Map.partition isFailure cleanFileMap

    let extractModuleFolder acc (fileName: string) (result: Result<ChakraModule * InputState>) =
        match result with
        | Success (m, input) ->
            // TODO: Create a function that returns a binding name based on the
            // filename and the binding name
            let bindingName (ChakraBinding (_, pattern, _)) =
                match pattern with
                | ChakraFunctionBindingPattern info -> info.Name
                | ChakraSimpleBindingPattern name -> name
                | ChakraDestructuredBindingPattern (_) -> ""

            let addFlatBinding map binding =
                let moduleName =
                    fileName
                        .Replace(".chakra", "")
                        .Replace(path, "")

                let resolvedName = sprintf "%s:%O" moduleName (bindingName binding)

                Map.add
                    resolvedName
                    binding
                    map

            List.fold
                addFlatBinding
                acc
                m.Bindings

        | Failure (label, error, parserPos) -> acc

    if Map.isEmpty failures then

        Map.fold extractModuleFolder Map.empty successes
        |> Map.iter (fun k v -> printfn "%s: %O" k v)
    else
        let printFailure fileName result =
            printfn "%s\n================" fileName
            printResult result

        Map.iter printFailure failures