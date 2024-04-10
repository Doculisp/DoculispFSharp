module Doculisp.Tests.Document.``map when mapping inline code blocks should``

open Archer
open Archer.Arrows
open Archer.ApprovalsSupport
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes
open Doculisp.Tests

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Inline Code Block"
        Category "Code Block"
    ],
    setupApprovals
)

let ``map an inline code block`` =
    feature.Test (fun reporters env ->
        "`var x = 13`"
        |> stringToMaybeCharSeq
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map an inline code block containing a html comment`` =
    feature.Test (fun reporters env ->
        "`<!-- My Comment -->`"
        |> stringToMaybeCharSeq
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map an inline code block containing an unclosed html comment`` =
    feature.Test (fun reporters env ->
        "`<!-- My Comment`"
        |> stringToMaybeCharSeq
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map an inline code block containing an inline code block`` =
    feature.Test (fun reporters env ->
        "`\`var c = 'c'\``"
        |> stringToMaybeCharSeq
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``error if there is a new line before it closes`` =
    feature.Test (fun reporters env ->
        "example: `var name = \"Alpha\"
`"
        |> stringToMaybeCharSeq
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``error if there is no close to the block`` =
    feature.Test (fun reporters env ->
        "example:
`var name = \"Alpha\""
        |> stringToMaybeCharSeq
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``not map an escaped back-tick`` =
    feature.Test (fun reporters env ->
        @"\`<!-- My Comment -->\`"
        |> stringToMaybeCharSeq
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``not map a commented out code block`` =
    feature.Test (fun _ ->
        "<!-- `let x = y |> doSomething` -->"
        |> stringToMaybeCharSeq
        |> Document.map
        |> Should.BeOk []
    )

let ``Test Cases`` = feature.GetTests ()
