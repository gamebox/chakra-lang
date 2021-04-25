module TypeSystem

open AST

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

type SymbolInfo =
    | GlobalSymbol of string
    | ModuleSymbol of mod': string * name: string

type Type =
    // Concrete Types
    | StringType
    | NumberType
    | SymbolType of SymbolInfo
    | LiteralType of ChakraLiteral
    | TupleType of Type list
    | ListType of Type
    | MapType of key: Type * value: Type
    | StructType of fields: (string * Type) list * isOpen: bool * tag: SymbolInfo option
    | FunctionType of args: (string * Type) list * ret: Type
    // Opaque or Compiler types
    | CommandType // A command to the system
    | CapabilityType of Capability // A capability to perform an action
    | RefType of Type // A reference to an Actor that takes a msg of Type
    // ADT
    | GenericType of string // A type variable
    | UnionType of Type list // One or more different type
    | SumType of Type list // Multiple different types
    | PolymorphicType of string // Typeclass membership


let num = NumberType

let str = StringType
let gSym = SymbolType << GlobalSymbol
let mSym = SymbolType << ModuleSymbol
let sym (s: string) =
    let firstChar = (s.ToCharArray()).[0]

    if System.Char.IsUpper firstChar then
        mSym s
    else gSym s
let lit = LiteralType
let tup = TupleType
let list = ListType
let map k v = MapType(k, v)
let strct = StructType
let fn args retrn = FunctionType(args, retrn)
let gen = GenericType
let union = UnionType
let sum = SumType
let cap = CapabilityType
let cmd = CommandType

let genA = gen "a"
let genB = gen "b"
let nextGen (prev: string) = (sprintf "%c" (char (int (prev.Chars 0) + 1)))
let bool = union [ gSym "true"; gSym "false" ]

let opt t =
    union [ (gSym "none")
            tup [ lit (ChakraSymbol "some"); t ] ]