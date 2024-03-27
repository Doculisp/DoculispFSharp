module Doculisp.Tests.Document.``map when mapping comments should``

open Archer
open Archer.Arrows
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Comments"
    ]
)

let ``ignore a comment`` =
    feature.Test (fun _ ->
        "<!-- Some piffy comment -->"
        |> Document.map
        |> Should.BeOk []
    )

let ``ignore a comment with extra whitespace`` =
    feature.Test (fun _ ->
        "    <!-- Some piffy comment -->    "
        |> Document.map
        |> Should.BeOk []
    )

let ``ignore a comment surrounded with text`` =
    feature.Test (fun _ ->
        "My cool <!-- Some piffy comment -->   world"
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = "My cool"; Coordinate = { Line = 0; Char = 0 } }
            TextMap { Value = "world"; Coordinate = { Line = 0; Char = 38 } }
        ]
    )

let ``ignore a multiline comment surrounded with text`` =
    feature.Test (fun _ ->
        "My cool
<!-- Some piffy comment -->
world"
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = "My cool"; Coordinate = { Line = 0; Char = 0 } }
            TextMap { Value = "world"; Coordinate = { Line = 2; Char = 0 } }
        ]
    )
    
let ``error when the comment block is unclosed`` =
    feature.Test (fun _ ->
        "My awesome text
to be hold <!-- not really"
        |> Document.map
        |> Should.BeError "Comment at (1, 11) is not closed."
    )

let ``Test Cases`` = feature.GetTests ()
