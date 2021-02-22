module Build

open System.IO
open System.Text.RegularExpressions
open ParserLibrary
open ChakraParser
open Pretty


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

type Project = Project of projectName: string * root: string * version: string
type BuildError =
    | BuildIOError of string
    | BuildConfigNoNameError
    | BuildParseError of (ParserLabel * ParserError * ParserPosition) list
    | BuildTypeError of Env.TypeError list
    | BuildIRError
    | BuildLinkError
    | BuildCompileError

let (.>>.) res fn = Result.bind fn res

let fileContents path =
    try
        Ok (File.ReadAllLines path)
        |> Result.map (String.concat "\n")
    with
    | _ -> Error (BuildIOError path) 

let parseFile fileParser file =
    run fileParser file
    |> toResult
    |> Result.map fst
    |> Result.mapError (fun e -> BuildParseError [e])


let projectFromMetadata metadataFilePath =
    let parseMetadata = parseFile chakraMetdata
    let extractMetadataFromParsed parsed =
        match (Map.tryFind "name" parsed, Map.tryFind "version" parsed) with
        | (Some (ChakraString name), Some (ChakraString v)) ->
            Ok (Project (name, metadataFilePath, v))
        | _ ->
            Error BuildConfigNoNameError

    fileContents metadataFilePath
    .>>. parseMetadata
    .>>. extractMetadataFromParsed

let parseProjectFiles (Project (name, root, v)) =
    // Directory.EnumerateFileSystemEntries (Path.Combine root "libs")
    // |> List.ofSeq
    Ok (Project (name, root, v))

let verifyProject proj =
    Ok proj

let generateIR proj =
    Ok proj

let writeToDisk proj =
    Ok proj

let linkAndCompile proj =
    Ok ()

let metadataPath projectPath =
    Path.Combine [| projectPath; "metadata.chakra" |]


let build optPath =
    Option.defaultWith Directory.GetCurrentDirectory optPath
    |> Path.GetFullPath
    |> metadataPath
    |> projectFromMetadata
    .>>. parseProjectFiles
    .>>. verifyProject
    .>>. generateIR
    .>>. writeToDisk
    .>>. linkAndCompile




