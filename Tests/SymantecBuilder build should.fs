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
            let path = "./docs/_main.md"
            "Hello world"
            |> stringToMaybeCharSeq
            |> Document.map path
            |> Tokenizer.parse path
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``build symantec tree document containing simple doculisp`` =
    feature.Test (
        TestBody (fun reporter env ->
            let path = "./docs/_doc.md"
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
            |> Document.map path
            |> Tokenizer.parse path
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
            let path = "./docs/_start.md"
            markdown
            |> stringToMaybeCharSeq
            |> Document.map path
            |> Tokenizer.parse path
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that does not contain section-meta but contains doculisp`` =
    feature.Test (
        TestBody (fun reporter env ->
            let path = "./docs/readme.md"
            "Some text\n<!-- (dl (# My Heading)) -->"
            |> stringToMaybeCharSeq
            |> Document.map path
            |> Tokenizer.parse path
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that has a section-meta block without a title`` =
    feature.Test (
        TestBody (fun reporter env ->
            let path = "./docs/build.md"
            "<!-- (dl (section-meta (external (section ./section.md)))) -->"
            |> stringToMaybeCharSeq
            |> Document.map path
            |> Tokenizer.parse path
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that has 2 section-metas block`` =
    feature.Test (
        TestBody (fun reporter env ->
        let path = "_.build.md"
        "<!-- (dl\n\t(section-meta (title My Title))\n\t(section-meta (title My more awesome title))\n) -->"
            |> stringToMaybeCharSeq
            |> Document.map path
            |> Tokenizer.parse path
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that has 2 section-metas block in two different dl blocks`` =
    feature.Test (
        TestBody (fun reporter env ->
        let path = "./docs/bob.md"
        "<!-- (dl\n\t(section-meta (title My Title))) -->\n# Hello\n<!-- (dl (section-meta (title My more awesome title))) -->"
            |> stringToMaybeCharSeq
            |> Document.map path
            |> Tokenizer.parse path
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that has a content block and no externals`` =
    feature.Test (fun reporter env ->
        let path = "./docs/_main.md"
        "<!-- (dl\n\t(section-meta (title My Title))\n\t(content)\n) -->"
        |> stringToMaybeCharSeq
        |> Document.map path
        |> Tokenizer.parse path
        |> SymantecBuilder.build
        |> formatSymantecTree
        |> Should.MeetStandard reporter env.TestInfo
    )

let ``Test Cases`` =
    feature.GetTests ()
