module Doculisp.Tests.Document.``map when mapping comments should``

open Archer
open Archer.Arrows
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Text"
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
            TextMap ("My cool", { Line = 0; Char = 0 })
            TextMap ("world", { Line = 0; Char = 38 })
        ]
    )

let ``ignore a multiline comment surrounded with text`` =
    feature.Test (fun _ ->
        "My cool
<!-- Some piffy comment -->
world"
        |> Document.map
        |> Should.BeOk [
            TextMap ("My cool", { Line = 0; Char = 0 })
            TextMap ("world", { Line = 2; Char = 0 })
        ]
    )

let ``Test Cases`` = feature.GetTests ()