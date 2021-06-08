module Operators

let (.<?>.) opt fn =
    opt
    |> Option.map fn
let (.?>>.) opt fn =
    opt
    |> Option.bind fn
let (.>>.) opt fn = Result.bind fn opt
let (.<.>.) a b = a, b