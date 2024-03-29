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
    ]
)

let ``map a doculisp block`` =
    feature.Test (fun _ ->
        "<!-- (dl (# My Heading)) -->"
        |> Document.map
        |> Should.BeOk [
            LispMap { Value = "(dl (# My Heading))"; Coordinate = { Line = 0; Char = 5 } }
        ]
    )

let ``map a doculisp block contained with other comments`` =
    feature.Test (fun _ ->
        "<!--
My heading is the best
      (dl (# My New Heading))
Just look at its dynamic-ness
-->"
        |> Document.map
        |> Should.BeOk [
            LispMap { Value = "(dl (# My New Heading))"; Coordinate = { Line = 2; Char = 6 } }
        ]
    )

let ``map a multiline doculisp block contained with other comments`` =
    feature.Test (fun _ ->
        "<!--
My heading is the best
     (dl
        (# My New Heading)
     )
Just look at its dynamic-ness
-->"
        |> Document.map
        |> Should.BeOk [
            LispMap { Value = "(dl
        (# My New Heading)
     )"; Coordinate = { Line = 2; Char = 5 } }
        ]
    )

let ``map a multiline doculisp block with escaped parentheses`` =
    feature.Test (fun _ ->
        @"<!--
My heading is the best
     (dl
       (# My New \(Heading)
     )
Just look at its dynamic-ness
-->"
        |> Document.map
        |> Should.BeOk [
            LispMap { Value = @"(dl
       (# My New \(Heading)
     )"; Coordinate = { Line = 2; Char = 5 } }
        ]
    )

let ``error if the lisp does not properly close`` =
    feature.Test (fun _ ->
        "<!-- (dl (# My New Heading) -->"
        |> Document.map
        |> Should.BeError "Doculisp block at (0, 5) is not closed."
    )

let ``map a real markdown document`` =
    feature.Test(
        Setup (fun _ ->
            try
                let assembly = System.Reflection.Assembly.GetExecutingAssembly ()
                let resourceName =
                    assembly.GetManifestResourceNames()
                    |> Array.filter (fun name -> name.EndsWith "section-meta.md")
                    |> Array.head

                let markdown =
                    use stream = assembly.GetManifestResourceStream resourceName
                    use reader = new System.IO.StreamReader (stream)
                    reader.ReadToEnd ()

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
