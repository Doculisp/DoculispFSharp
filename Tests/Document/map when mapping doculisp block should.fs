module Doculisp.Tests.Document.``map when mapping doculisp block should``

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
        |> Document.map
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
        |> Document.map
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
        |> Document.map
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
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``error if the lisp does not properly close`` =
    feature.Test (fun reporters env ->
        "<!-- (dl (# My New Heading) -->"
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
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
            |> Document.map
            |> formatMap
            |> Should.MeetStandard reporter env.TestInfo
        )
    )

let ``Test Cases`` = feature.GetTests ()
