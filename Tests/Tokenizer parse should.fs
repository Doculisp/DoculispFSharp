module Doculisp.Tests.``Tokenizer parse should``

open Archer
open Archer.Arrows
open Archer.ApprovalsSupport
open ApprovalTests
open Doculisp.Lib
open Doculisp.Lib.TokenTypes

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Text"
    ],
    Setup (fun _ ->
        [
            Searching
                |> findFirstReporter<Reporters.DiffReporter>
                |> findFirstReporter<Reporters.WinMergeReporter>
                |> findFirstReporter<Reporters.InlineTextReporter>
                |> findFirstReporter<Reporters.AllFailingTestsClipboardReporter>
                |> unWrapReporter

            Reporters.ClipboardReporter() :> Core.IApprovalFailureReporter;

            Reporters.QuietReporter() :> Core.IApprovalFailureReporter;
        ]
        |> buildReporter
        |> Ok
    )
)

let ``parse an empty map`` =
    feature.Test (fun _ ->
        Ok []
        |> Tokenizer.parse
        |> Should.BeOk []
    )

let ``error if given an error`` =
    feature.Test (fun _ ->
        "My bogus error"
        |> Error
        |> Tokenizer.parse
        |> Should.BeError "My bogus error"
    )

let ``parse text`` =
    feature.Test (fun _ ->
        "Hello Test"
        |> Document.map
        |> Tokenizer.parse
        |> Should.BeOk [
            Text { Value = "Hello Test"; Coordinate = { Line = 0; Char = 0 } }
        ]
    )

let ``parse multiline text`` =
    feature.Test (fun _ ->
        "<!-- --> Hello Test
this is a case"
        |> Document.map
        |> Tokenizer.parse
        |> Should.BeOk [
            Text { Value = "Hello Test
this is a case"; Coordinate = { Line = 0; Char = 9 } }
        ]
    )

let ``parse single line Doculisp`` =
    feature.Test (fun reporter environment ->
        "<!-- (dl (content)) -->"
        |> Document.map
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``parse single line Doculisp with parameter`` =
    feature.Test (fun reporter environment ->
        "<!-- (dl (# My Heading)) -->"
        |> Document.map
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``parse single line Doculisp with parameter that contain parentheses`` =
    feature.Test (fun reporter environment ->
        "<!-- (dl (# My \(Heading\))) -->"
        |> Document.map
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``parse multiline Doculisp with parameter that contain parentheses`` =
    feature.Test (fun reporter environment ->
        "<!--
(dl
    (section-meta
        (title My Document)
    )
)
-->

# Heading
"
        |> Document.map
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``error if missing atom`` =
    feature.Test (fun _ ->
        "<!--
(dl
    ( my parameter)
)
-->"
        |> Document.map
        |> Tokenizer.parse
        |> Should.BeError "Open parentheses without atom at (2, 4)."
    )

let ``Test Cases`` = feature.GetTests ()
