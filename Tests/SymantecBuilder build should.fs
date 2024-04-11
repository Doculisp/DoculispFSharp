module Doculisp.Tests.``SymantecBuilder build should``

open Archer
open Archer.Arrows
open Archer.ApprovalsSupport
open Doculisp.Lib
open Doculisp.Lib.SymantecTypes

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Text"
    ],
    setupApprovals
)

let ``build from empty tokens`` =
    feature.Test (
        TestBody(fun _ ->
            []
            |> Ok
            |> SymantecBuilder.build
            |> Should.BeEqualTo (Ok Empty)
        )
    )

let ``build symantec tree for text`` =
    feature.Test (
        TestBody(fun reporter env ->
            "Hello world"
            |> stringToMaybeCharSeq
            |> Document.map "./docs/_main.md"
            |> Tokenizer.parse
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``build symantec tree document containing simple doculisp`` =
    feature.Test (
        TestBody (fun reporter env ->
            "<!--
(dl
    (section-meta
        (title My document)
    )
)
-->

## A hardcoded heading

with some text"
            |> stringToMaybeCharSeq
            |> Document.map "./docs/_doc.md"
            |> Tokenizer.parse
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``build symantec tree for real document`` =
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
            |> Document.map "./docs/_start.md"
            |> Tokenizer.parse
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that does not contain section-meta but contains doculisp`` =
    feature.Test (
        TestBody (fun reporter env ->
            "Some text\n<!-- (dl (# My Heading)) -->"
            |> stringToMaybeCharSeq
            |> Document.map "./docs/readme.md"
            |> Tokenizer.parse
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that has a section-meta block without a title`` =
    feature.Test (
        TestBody (fun reporter env ->
            "<!-- (dl (section-meta (external (section ./section.md)))) -->"
            |> stringToMaybeCharSeq
            |> Document.map "./docs/build.md"
            |> Tokenizer.parse
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that has 2 section-metas block`` =
    feature.Test (
        TestBody (fun reporter env ->
        "<!-- (dl\n\t(section-meta (title My Title))\n\t(section-meta (title My more awesome title))\n) -->"
            |> stringToMaybeCharSeq
            |> Document.map "_.build.md"
            |> Tokenizer.parse
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that has 2 section-metas block in two different dl blocks`` =
    feature.Test (
        TestBody (fun reporter env ->
        "<!-- (dl\n\t(section-meta (title My Title))) -->\n# Hello\n<!-- (dl (section-meta (title My more awesome title))) -->"
            |> stringToMaybeCharSeq
            |> Document.map "./docs/bob.md"
            |> Tokenizer.parse
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that has a content block and no externals`` =
    feature.Test (fun reporter env ->
        "<!-- (dl\n\t(section-meta (title My Title))\n\t(content)\n) -->"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/_main.md"
        |> Tokenizer.parse
        |> SymantecBuilder.build
        |> formatSymantecTree
        |> Should.MeetStandard reporter env.TestInfo
    )

let ``Test Cases`` =
    feature.GetTests ()
