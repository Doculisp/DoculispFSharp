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
        |> Document.map
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``parse multiline text`` =
    feature.Test (fun reporters env ->
        "<!-- --> Hello Test
this is a case"
        |> Document.map
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporters env.TestInfo
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
    feature.Test (fun reporter env ->
        "<!--
(dl
    ( my parameter)
)
-->"
        |> Document.map
        |> Tokenizer.parse
        |> formatTokens
        |> Should.MeetStandard reporter env.TestInfo
    )

let ``parse a real file`` =
    feature.Test (
        Setup (fun reporter ->
            try
                let markdown = openMarkdown ()

                Ok (markdown, reporter)
            with
            | e -> e |> SetupTeardownExceptionFailure |> Error
        ),
        TestBody (fun (markdown, reporter) env ->
            markdown
            |> Document.map
            |> Tokenizer.parse
            |> formatTokens
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``Test Cases`` = feature.GetTests ()
