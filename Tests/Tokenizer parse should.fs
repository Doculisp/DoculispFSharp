module Doculisp.Tests.``Tokenizer parse should``

open Archer
open Archer.Arrows
open Archer.ApprovalsSupport
open Doculisp.Lib

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Text"
    ],
    setupApprovals
)

let ``parse an empty map`` =
    feature.Test (fun _ ->
        Ok []
        |> Tokenizer.parse
        |> Should.BeOk []
    )

let ``error if given an error`` =
    feature.Test (fun reporters env ->
        "My bogus error"
        |> Error
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``parse text`` =
    feature.Test (fun reporters env ->
        "Hello Test"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/_main.md"
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``parse multiline text`` =
    feature.Test (fun reporters env ->
        "<!-- --> Hello Test
this is a case"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/_readme.md"
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``parse single line Doculisp`` =
    feature.Test (fun reporter environment ->
        "<!-- (dl (content)) -->"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/_start.md"
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``parse single line Doculisp with parameter`` =
    feature.Test (fun reporter environment ->
        "<!-- (dl (# My Heading)) -->"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/piffy.md"
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``parse single line Doculisp with parameter that contain parentheses`` =
    feature.Test (fun reporter environment ->
        "<!-- (dl (# My \(Heading\))) -->"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/test.md"
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
        |> stringToMaybeCharSeq
        |> Document.map "./docs/_main.md"
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``error if missing atom`` =
    feature.Test (fun reporter env ->
        "<!--
(dl
    ( my parameter)
)
-->"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/_doc.md"
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporter env.TestInfo
    )

let ``parse a real file`` =
    feature.Test (
        Setup (fun reporter ->
            try
                let markdown = openMarkdownFile ()

                Ok (markdown, reporter)
            with
            | e -> e |> SetupTeardownExceptionFailure |> Error
        ),
        TestBody (fun (markdown, reporter) env ->
            markdown
            |> stringToMaybeCharSeq
            |> Document.map "./docs/_readme.md"
            |> Tokenizer.parse
            |> formatTokens
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``Test Cases`` = feature.GetTests ()
