module Doculisp.Tests.Document.``map when mapping comments should``

open Archer
open Archer.Arrows
open Archer.ApprovalsSupport
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes
open Doculisp.Tests

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Comments"
    ],
    setupApprovals
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
    feature.Test (fun reporters env ->
        "My cool <!-- Some piffy comment -->   world"
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``ignore a multiline comment surrounded with text`` =
    feature.Test (fun reporters env ->
        "My cool
<!-- Some piffy comment -->
world"
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )
    
let ``error when the comment block is unclosed`` =
    feature.Test (fun reporters env ->
        "My awesome text
to be hold <!-- not really"
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``Test Cases`` = feature.GetTests ()
