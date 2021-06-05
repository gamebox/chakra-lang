module TypeError

type TypeError =
    | FatalTypeError of string
    | UntypedError of varName: string
    | FeatureNotSupported
    | NonFunctionApplication of bindingName: string * typ: Type
    | UndefinedBinding of string
    | ArgumentMismatch of argT: Type * paramT: Type
    | IllegalFieldAccess of binding: string * typ: Type
    | UnifyError of types: Type list
    | ModuleNotFound of moduleName: string
    | ExportsMissing of missingExports: string list
    | PatternMismatch of pattern: ChakraPattern * typ: Type

    member x.IsUntyped =
        match x with
        | UntypedError _ -> true
        | _ -> false