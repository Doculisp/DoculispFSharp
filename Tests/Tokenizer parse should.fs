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
        |> Tokenizer.parse "./main.md"
        |> Should.BeOk []
    )

let ``error if given an error`` =
    feature.Test (fun reporters env ->
        "My bogus error"
        |> Error
        |> Tokenizer.parse "./docs/myDoc.md"
        |> formatTokens
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``parse text`` =
    feature.Test (fun reporters env ->
        let path = "./docs/_main.md"
        "Hello Test"
        |> stringToMaybeCharSeq
        |> Document.map path
        |> Tokenizer.parse path
        |> formatTokens
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``parse multiline text`` =
    feature.Test (fun reporters env ->
        let path = "./docs/_readme.md"
        "<!-- --> Hello Test
this is a case"
        |> stringToMaybeCharSeq
        |> Document.map path
        |> Tokenizer.parse path
        |> formatTokens
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``parse single line Doculisp`` =
    feature.Test (fun reporter environment ->
        let path = "./docs/_start.md"
        "<!-- (dl (content)) -->"
        |> stringToMaybeCharSeq
        |> Document.map path
        |> Tokenizer.parse path
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``parse single line Doculisp with parameter`` =
    feature.Test (fun reporter environment ->
        let path = "./docs/piffy.md"
        "<!-- (dl (# My Heading)) -->"
        |> stringToMaybeCharSeq
        |> Document.map path
        |> Tokenizer.parse path
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``parse single line Doculisp with parameter that contain parentheses`` =
    feature.Test (fun reporter environment ->
        let path = "./docs/test.md"
        "<!-- (dl (# My \(Heading\))) -->"
        |> stringToMaybeCharSeq
        |> Document.map path
        |> Tokenizer.parse path
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``parse multiline Doculisp with parameter that contain parentheses`` =
    feature.Test (fun reporter environment ->
        let path = "./docs/_main.md"
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
        |> Document.map path
        |> Tokenizer.parse path
        |> formatTokens
        |> Should.MeetStandard reporter environment.TestInfo
    )

let ``error if missing atom`` =
    feature.Test (fun reporter env ->
        let path = "./docs/_doc.md"
        "<!--
(dl
    ( my parameter)
)
-->"
        |> stringToMaybeCharSeq
        |> Document.map path
        |> Tokenizer.parse path
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
            let path = "./docs/_readme.md"
            markdown
            |> stringToMaybeCharSeq
            |> Document.map path
            |> Tokenizer.parse path
            |> formatTokens
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``Test Cases`` = feature.GetTests ()
