module Doculisp.Tests.Document.``map when mapping doculisp block should``

open Archer
open Archer.Arrows
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes

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
            LispMap ("(dl (# My Heading))", { Line = 0; Char = 5 })
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
            LispMap ("(dl (# My New Heading))", { Line = 2; Char = 6 })
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
            LispMap ("(dl
        (# My New Heading)
     )", { Line = 2; Char = 5 })
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
            LispMap (@"(dl
       (# My New \(Heading)
     )", { Line = 2; Char = 5 })
        ]
    )

let ``error if the lisp does not properly close`` =
    feature.Test (fun _ ->
        "<!-- (dl (# My New Heading) -->"
        |> Document.map
        |> Should.BeError "Doculisp block at (0, 5) is not closed."
    )

let ``Test Cases`` = feature.GetTests ()
