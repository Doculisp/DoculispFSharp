module Doculisp.Tests.Document.``map when mapping text should``

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

let ``map an empty document`` =
    feature.Test (fun _ ->
        ""
        |> Document.map
        |> Should.BeOk []
    )

let ``map "Hello" as text`` =
    feature.Test (fun _ ->
        "Hello"
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = "Hello"; Coordinate = { Line = 0; Char = 0 } }
        ]
    )

let ``map "Good bye" as text`` =
    feature.Test (fun _ ->
        "Good Bye"
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = "Good Bye"; Coordinate = { Line = 0; Char = 0 } }
        ]
    )

let ``map text followed by spaces`` =
    feature.Test (fun _ ->
        "Good Bye   "
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = "Good Bye"; Coordinate = { Line = 0; Char = 0 } }
        ]
    )

let ``map text surrounded by spaces`` =
    feature.Test (fun _ ->
        "   Doculisp   "
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = "Doculisp"; Coordinate = { Line = 0; Char = 3 } }
        ]
    )

let ``map text surrounded by spaces and preceded by new lines`` =
    feature.Test (fun _ ->
        "\r\n\r\n\r\n\r\n   After Lines   "
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = "After Lines"; Coordinate = { Line = 4; Char = 3 } }
        ]
    )
    
let ``map text that includes new lines`` =
    feature.Test (fun _ ->
        "# A document\r\n\r\nAbout something"
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = "# A document\r\n\r\nAbout something"; Coordinate = { Line = 0; Char = 0 } }
        ]
    )

let ``Test Cases`` = feature.GetTests ()
