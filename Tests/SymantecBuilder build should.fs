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

let ``Test Cases`` =
    feature.GetTests ()
