module Doculisp.Tests.``SymantecBuilder build should``

open Archer
open Archer.Arrows
open Archer.ApprovalsSupport
open Doculisp.Lib
open Doculisp.Lib.SymantecTypes

let private feature = Arrow.Ignore (
    TestTags [
        Category "Document"
        Category "Text"
    ],
    setupApprovals
)

let ``build from empty tokens`` =
    feature.Test (fun _ ->
        []
        |> Ok
        |> SymantecBuilder.build
        |> Should.BeEqualTo (Ok Empty)
    )

let ``build symantec tree for text`` =
    feature.Test (fun reporter env ->
        "Hello world"
        |> Document.map
        |> Tokenizer.parse
        |> SymantecBuilder.build
        |> formatSymantecTree
        |> Should.MeetStandard reporter env.TestInfo
    )

let ``build symantec tree document containing simple doculisp`` =
    feature.Test (fun reporter env ->
        "<!--
(dl
    (section-meta
        (title My document
    )
)
-->

## A hardcoded heading

with some text"
        |> Document.map
        |> Tokenizer.parse
        |> SymantecBuilder.build
        |> formatSymantecTree
        |> Should.MeetStandard reporter env.TestInfo
    )

let ``build symantec tree for real document`` =
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
            |> SymantecBuilder.build
            |> formatSymantecTree
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``error for document that does not contain section-meta but contains doculisp`` =
    feature.Test (fun reporter env ->
        "Some text\n<!-- (dl (# My Heading)) -->"
        |> Document.map
        |> Tokenizer.parse
        |> SymantecBuilder.build
        |> Should.BeError "Doculisp block at (2, 10) appears before section-meta block."
    )

let ``error for document that has a section-meta block without a title`` =
    feature.Test (fun reporter env ->
        "<!-- (dl (section-meta (external (section ./section.md)))) -->"
        |> Document.map
        |> Tokenizer.parse
        |> SymantecBuilder.build
        |> Should.BeError "Doculisp section-meta block at (1, 10) does not have title block."
    )

let ``error for document that has 2 section-metas block`` =
    feature.Test (fun reporter env ->
        "<!-- (dl\n\t(section-meta (title My Title))\n\t(section-meta (title My more awesome title))\n) -->"
        |> Document.map
        |> Tokenizer.parse
        |> SymantecBuilder.build
        |> Should.BeError "Doculisp section-meta block at (3, 2) is a duplicate section-meta block."
    )

let ``error for document that has a content block and no externals`` =
    feature.Test (fun reporter env ->
        "<!-- (dl\n\t(section-meta (title My Title))\n\t(content)\n) -->"
        |> Document.map
        |> Tokenizer.parse
        |> SymantecBuilder.build
        |> Should.BeError "Doculisp content block at (3, 2) does not have any external content in section-meta."
    )

let ``Test Cases`` =
    feature.GetTests ()
