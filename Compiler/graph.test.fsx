#load "Graph.fs"

open Graph

let sortTest nodes =
    let g = withNodes nodes

    match sort g with
    | Some sorted ->
        printfn "Sorted:\n"
        for node in sorted do printfn "%O" node
        printfn ""

    | None ->
        printfn "Failed to sort."
        display g

sortTest [
    "d", "", []
    "a", "sdfsdfdsf", [ "b" ]
    "b", "", [ "d" ]
    "c", "", [ "b" ]

]

sortTest [
    "main", "sfsfsfsdffsd", [ "some-lib"]
    "some-lib", "sgsdfsdfsdf", [ "some-sub-lib"]
    "some-sub-lib", "dsdfdfs", [ "/stdlib" ]
    "/stdlib", "fsdfsdfsdfsdf", []
]

