module Doculisp.Tests.Document.``map when mapping text should``

open Archer
open Archer.Arrows
open Archer.ApprovalsSupport
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes
open Doculisp.Tests

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Text"
    ],
    setupApprovals
)

let ``map an empty document`` =
    feature.Test (fun _ ->
        ""
        |> Document.map
        |> Should.BeOk []
    )

let ``map "Hello" as text`` =
    feature.Test (fun reporters env ->
        "Hello"
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map "Good bye" as text`` =
    feature.Test (fun reporters env ->
        "Good Bye"
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map text followed by spaces`` =
    feature.Test (fun reporters env ->
        "Good Bye   "
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map text surrounded by spaces`` =
    feature.Test (fun reporters env ->
        "   Doculisp   "
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map text surrounded by spaces and preceded by new lines`` =
    feature.Test (fun reporters env ->
        "\r\n\r\n\r\n\r\n   After Lines   "
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )
    
let ``map text that includes new lines`` =
    feature.Test (fun reporters env ->
        "# A document\r\n\r\nAbout something"
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``Test Cases`` = feature.GetTests ()
