module Doculisp.Tests.Document.``map when mapping inline code blocks should``

open Archer
open Archer.Arrows
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Inline Code Block"
        Category "Code Block"
    ]
)

let ``map an inline code block`` =
    feature.Test (fun _ ->
        "`var x = 13`"
        |> Document.map
        |> Should.BeOk [
            TextMap ("`var x = 13`", { Line = 0; Char = 0 })
        ]
    )

let ``map an inline code block containing a html comment`` =
    feature.Test (fun _ ->
        "`<!-- My Comment -->`"
        |> Document.map
        |> Should.BeOk [
            TextMap ("`<!-- My Comment -->`", { Line = 0; Char = 0 })
        ]
    )

let ``map an inline code block containing an unclosed html comment`` =
    feature.Test (fun _ ->
        "`<!-- My Comment`"
        |> Document.map
        |> Should.BeOk [
            TextMap ("`<!-- My Comment`", { Line = 0; Char = 0 })
        ]
    )

let ``map an inline code block containing an inline code block`` =
    feature.Test (fun _ ->
        "`\`var c = 'c'\``"
        |> Document.map
        |> Should.BeOk [
            TextMap (@"`\`var c = 'c'\``", { Line = 0; Char = 0 })
        ]
    )

let ``error if there is a new line before it closes`` =
    feature.Test (fun _ ->
        "example: `var name = \"Alpha\"
`"
        |> Document.map
        |> Should.BeError "Inline code block at (0, 9) contains a new line."
    )

let ``error if there is no close to the block`` =
    feature.Test (fun _ ->
        "example:
`var name = \"Alpha\""
        |> Document.map
        |> Should.BeError "Inline code block at (1, 0) is not closed."
    )

let ``not map an escaped back-tick`` =
    feature.Test (fun _ ->
        @"\`<!-- My Comment -->\`"
        |> Document.map
        |> Should.BeOk [
            TextMap (@"\`", { Line = 0; Char = 0 })
            TextMap (@"\`", { Line = 0; Char = 21 })
        ]
    )

let ``not map a commented out code block`` =
    feature.Test (fun _ ->
        "<!-- `let x = y |> doSomething` -->"
        |> Document.map
        |> Should.BeOk []
    )

let ``Test Cases`` = feature.GetTests ()
