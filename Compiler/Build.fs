module Build

open System.IO
open System.Text.RegularExpressions
open ParserLibrary
open ChakraParser
open Format


let cleaner lines =
    // let docCommentRegex = Regex "^;;"
    // let lineCommentRegex = Regex ";.*"
    // let restoreDocCommentRegex = Regex "^--"

    // let replace (regex: Regex) (replacement: string) str =
    //     regex.Replace(str, replacement)

    // let cleanLine line =
    //     docCommentRegex.Replace (line, "--")
    //     |> replace lineCommentRegex ""
    //     |> replace restoreDocCommentRegex ";;"

    // Seq.map cleanLine lines
    // |> String.concat "\n"
    String.concat "\n" lines

let rec gatherFiles directory =
    let dirs =
        Directory.EnumerateDirectories (directory)
        |> Seq.map gatherFiles
    let files =
        Directory.EnumerateFiles (directory)
        |> Seq.filter (fun f -> f.EndsWith(".chakra"))
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
        printfn "Parsing %s" k
        run chakraModule (cleaner v)

    let cleanFileMap =
        Map.map fileParseResultMapper fileMap

    let isFailure _ result =
        match result with
        | Failure _ -> true
        | Success (_, is) -> not (atEndOfInput is)

    let (failures, successes) = Map.partition isFailure cleanFileMap

    let extractModuleFolder acc (fileName: string) (result: ParserResult<ChakraModule * InputState>) =
        match result with
        | Success (m, input) ->
            // TODO: Create a function that returns a binding name based on the
            // filename and the binding name
            let bindingName ({ Pattern = pattern}) =
                match pattern with
                | ChakraFunctionBindingPattern info -> info.Name
                | ChakraSimpleBindingPattern name -> name
                | ChakraComplexBindingPattern _ -> ""

            let addFlatBinding map binding =
                let moduleName =
                    fileName
                        .Replace(".chakra", "")
                        .Replace(path, "")

                let resolvedName = sprintf "%s:%s" moduleName (bindingName binding)

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
        // Map.fold extractModuleFolder Map.empty successes
        // |> Map.iter (fun k v -> printfn "%s\n------------------------------\n%O" k v)
        successes
        |> Map.iter (fun k v ->
            match v with
            | Success (m, _) ->
                printfn "%s\n----------------------\n%s" k (pretty 80 (showModule m))
            | f ->
                printfn "WTF?")
    else
        let printFailure fileName result =
            printfn "%s\n------------------------\n" fileName
            match result with
            | Success(_, is) when not (atEndOfInput is) ->
                printfn "Was not able to parse the entire module, ended on (%d, %d):\n%s" is.Position.Line is.Position.Column (currentLine is)
            | _ -> printResult result

        Map.iter printFailure failures