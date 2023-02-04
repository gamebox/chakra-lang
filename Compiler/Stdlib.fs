module Stdlib

open TypeSystem

let stdlibModule fields = StructType (fields, false)

let stdio = custom0 "Stdio"
let cmd = custom0 "Cmd"
let actor = custom "Actor" [gen "init"; gen "msg" ]
let actorRef = custom "Ref" [gen "msg"]

let stdlibMath =
    stdlibModule [ ("add", fn [ "a", num; "b", num ] num)
                   ("sub", fn ["left", num; "right", num] num);
                   ("mul", fn ["left", num; "right", num] num);
                   ("div", fn ["left", num; "right", num] num);
                   ("pow", fn ["left", num; "exponent", num] num);
                   ("floor", fn ["num", num] num);
                   ("ceil", fn ["num", num] num);
                   ("round", fn ["num", num] num)]

let stdlibString =
    stdlibModule [ ("to-upper", fn [ "string", str ] str)
                   //  ("starts-with?", fn [ "query", str; "string", str ] bool)
                   //  ("ends-with?", fn [ "query", str; "string", str ] bool)
                   //  ("contains?", fn [ "query", str; "string", str ] bool)
                   //  ("substring",
                   //   fn
                   //       [ "start", num
                   //         "end", num
                   //         "string", str ]
                   //       str)
                   //  ("join",
                   //   fn
                   //       [ "separator", str
                   //         "strings", list str ]
                   //       str)
                    ]

let stdlibMap =
    stdlibModule [ ("has?", fn [ "key", genA; "map", map genA genB ] bool)
                   ("get", fn [ "key", genA; "map", map genA genB ] (opt genB))
                   ("set",
                    fn
                        [ "key", genA
                          "value", genB
                          "map", map genA genB ]
                        (map genA genB))
                   ("keys", fn [ "map", map genA genB ] (list genA))
                   ("values", fn [ "map", map genA genB ] (list genB))
                   ("pairs", fn [ "map", map genA genB ] (list (tup [ genA; genB ]))) ]

let stdlibList =
    stdlibModule [ ("map",
                    fn
                        [ "fn", fn [ "item", genA ] genB
                          "list", list genA ]
                        genB)
                   ("append", fn [ "item", genA; "list", list genA ] (list genA))
                   ("head", fn [ "list", list genA ] (opt genA))
                   ("tail", fn [ "list", list genA ] (list genA))
                   ("concat", fn [ "lists", list (list genA) ] (list genA))
                   ("fold",
                    fn
                        [ "fn", fn [ "state", genB; "item", genA ] genB
                          "list", list (genA)
                          "state", genB]
                        genB) ]

let stdlibIo =
    stdlibModule [ ("print",
                    fn
                        [ "cap", stdio
                          "str", str ]
                        (cmd)) ]

let stdlibFormat =
    stdlibModule [ ("number", fn [ "num", num ] str)
                   ("integer", fn [ "num", num ] str)]

let initFuncTy =
    fn [ "init", gen "init" ] (tup [ gen "state"; cmd ])

let receiveFuncTy =
    fn
        [ "state", gen "state"
          "msg", gen "msg" ]
        (tup [ gen "state"; cmd ])

let actorDef =
    strct (
        [ "init", initFuncTy
          "receive", receiveFuncTy ],
        false
    )

let stdlibActors =
    stdlibModule [ ("spawn",
                    fn
                        [ "actor", (actor)
                          "init", gen "init" ]
                        cmd)
                    ]

let stdlibExports =
    [ ("add", fn [ "a", num; "b", num ] num)
      ("sub", fn [ "a", num; "b", num ] num)
      ("div", fn [ "a", num; "b", num ] num)
      ("mul", fn [ "a", num; "b", num ] num)
      ("eq?", fn [ "a", genA; "b", genA ] bool)
      ("neq?", fn [ "a", genA; "b", genA ] bool)
      ("gt?", fn [ "a", num; "b", num ] bool)
      ("lt?", fn [ "a", num; "b", num ] bool)
      ("math", stdlibMath)
      ("string", stdlibString)
      ("map", stdlibMap)
      ("list", stdlibList)
      ("io", stdlibIo)
      ("actors", stdlibActors)
      ("format", stdlibFormat) ]
