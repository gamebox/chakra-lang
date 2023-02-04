module TypeSystem

[<StructuralEqualityAttribute>]
[<StructuralComparisonAttribute>]
type Capability =
    | StdioCapability
    | FileReadCapability
    | FileWriteCapability

type StdioFD =
    | Stdin
    | Stdout
    | Stderr

type ActorRef<'msg> = Actor of 'msg
// type Cmd =
//     | StdioPrintCommand
//     | SendCommand
//     | TimeoutCommand
//     | IntervalCommand
//     | FileOpenCommand
//     | FileReadCommand
//     | FileWriteCommand
//     | StdioReadCommand
//     | GetRandomCommand
//     | GetDateCommand

[<StructuralEqualityAttribute>]
[<StructuralComparisonAttribute>]
type Type =
    // Concrete Types
    | StringType
    | NumberType
    | TupleType of Type list
    | ListType of Type
    | MapType of key: Type * value: Type
    | StructType of fields: (string * Type) list * isOpen: bool
    | FunctionType of args: (string * Type) list * ret: Type
    // ADT
    | GenericType of string // A type variable
    | CustomType of name: string * variables: Type list

    member x.IsGeneric =
        let annotationIsGeneric (t: Type) = t.IsGeneric
        match x with
        | GenericType _ -> true
        | ListType a ->
            a.IsGeneric
        | MapType (k, v) ->
            k.IsGeneric || v.IsGeneric
        | FunctionType (argTys, retTy) ->
            List.exists (annotationIsGeneric) (List.map snd argTys) ||
                retTy.IsGeneric
        | StructType (fs, isOpen) ->
            isOpen ||
                List.exists (annotationIsGeneric) (List.map snd fs)
        | TupleType itemTys ->
            List.exists (annotationIsGeneric) itemTys
        | StringType -> false
        | NumberType -> false
        | CustomType (_, args) -> List.exists (fun (x: Type) -> x.IsGeneric) args


let num = NumberType

let str = StringType

let tup = TupleType
let list = ListType
let map k v = MapType(k, v)
let strct (fields, isOpen) =
    let sortedFields =
        List.sortBy (fst) fields
    StructType (sortedFields, isOpen)

let custom n args = CustomType (n, args)
let custom0 n = CustomType (n, [])
let fn args retrn = FunctionType(args, retrn)
let gen = GenericType
// let union = UnionType
// let sum = SumType
// let cap = CapabilityType
// let cmd = CommandType
// let actor state msg = ActorType (state, msg)

let genA = gen "a"
let genB = gen "b"

let nextGen (prev: string) =
    (sprintf "%c" (char (int (prev.Chars 0) + 1)))

let bool = custom0 "Bool"

let opt t = custom "Maybe" [genA]


/// Creates a user-friendly string representing the type
let rec print typ =
    match typ with
    | StringType -> "\"\""
    | NumberType -> "#"
    | ListType genericType -> sprintf "[ %s ]" (print genericType)
    | MapType (keyType, valueType) -> sprintf "%%[ %s = %s ]" (print keyType) (print valueType)
    | GenericType typ -> typ
    | TupleType types ->
        List.map print types
        |> String.concat ", "
        |> sprintf "( %s )"
    | StructType (fields, isOpen) ->
        sprintf
            "%%( %s %s)"
            (fields
             |> List.map (fun (name, typ) -> sprintf "%s = %s" name (print typ))
             |> String.concat ", ")
            (if isOpen then "..." else "")
    | FunctionType (args, retrn) ->
        let argList =
            sprintf
                "( %s )"
                (args
                 |> List.map (print << snd)
                 |> String.concat ", ")

        sprintf "{ %s -> %s }" argList (print retrn)
    | CustomType (name, args) when args.IsEmpty -> name
    | CustomType (name, args) -> sprintf $"""{name}({(String.concat "," (List.map print args) )})"""