﻿module Doculisp.Tests.Document.``map when mapping doculisp block should``

open System
open Archer
open Archer.Arrows
open Archer.ApprovalsSupport
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes
open Doculisp.Tests

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Doculisp"
    ],
    setupApprovals
)

let ``map a doculisp block`` =
    feature.Test (fun reporters env ->
        "<!-- (dl (# My Heading)) -->"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/stun.md"
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map a doculisp block contained with other comments`` =
    feature.Test (fun reporters env ->
        "<!--
My heading is the best
      (dl (# My New Heading))
Just look at its dynamic-ness
-->"
        |> stringToMaybeCharSeq
        |> Document.map"./docs/_head.md"
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map a multiline doculisp block contained with other comments`` =
    feature.Test (fun reporters env ->
        "<!--
My heading is the best
     (dl
        (# My New Heading)
     )
Just look at its dynamic-ness
-->"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/_main.md"
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map a multiline doculisp block with escaped parentheses`` =
    feature.Test (fun reporters env ->
        @"<!--
My heading is the best
     (dl
       (# My New \(Heading)
     )
Just look at its dynamic-ness
-->"
        |> stringToMaybeCharSeq
        |> Document.map "./docs/_doc.md"
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``error if the lisp does not properly close`` =
    feature.Test (
        TestBody(
            fun reporters env ->
            "<!-- (dl (# My New Heading) -->"
            |> stringToMaybeCharSeq
            |> Document.map "./docs/here.md"
            |> formatMap
            |> Should.MeetStandard reporters env.TestInfo
        )
    )

let ``map a real markdown document`` =
    feature.Test(
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
            |> Document.map "./docs/free.md"
            |> formatMap
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``Test Cases`` = feature.GetTests ()
