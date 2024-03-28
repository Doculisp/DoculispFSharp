[<AutoOpen>]
module Doculisp.Tests.TestHelpers

open Archer.Logger
open Archer.Arrows
open Archer.ApprovalsSupport
open ApprovalTests
open Doculisp.Lib
open Doculisp.Lib.TokenTypes

let setupApprovals =
    Setup (fun _ ->
        [
            Searching
                |> findFirstReporter<Reporters.DiffReporter>
                |> findFirstReporter<Reporters.WinMergeReporter>
                |> findFirstReporter<Reporters.InlineTextReporter>
                |> findFirstReporter<Reporters.AllFailingTestsClipboardReporter>
                |> unWrapReporter

            Reporters.ClipboardReporter() :> Core.IApprovalFailureReporter;

            Reporters.QuietReporter() :> Core.IApprovalFailureReporter;
        ]
        |> buildReporter
        |> Ok
    )

let formatTokens (maybeTokens: Result<Token list, string>) =
    let rec formatLisps (current: string) (indenter: Indent.IIndentTransformer) (tokens: LispToken list) =
        match tokens with
        | [] -> current
        | (Open value)::tail ->
            let opn = $"(%s{value.Value} @ %s{value.Coordinate.ToString ()}" |> indenter.Transform
            tail
            |> formatLisps $"%s{current}\n%s{opn}" (indenter.Indent 1)
        | (Parameter value)::tail ->
            let param = $"Parameter: %s{value.Value} @ %s{value.Coordinate.ToString ()}" |> indenter.Transform
            tail
            |> formatLisps $"%s{current}\n%s{param}" indenter
        | (Close coordinate)::tail ->
            let ind = indenter.Indent -1
            let cls = $") @ %s{coordinate.ToString ()}" |> ind.Transform
            tail
            |> formatLisps $"%s{current}\n%s{cls}" ind

    let rec formatTokens (current: string) (indenter: Indent.IIndentTransformer) (tokens: Token list) =
        match tokens with
        | [] -> current
        | (Text value)::tail ->
            let v =
                let textOpen = indenter.Transform "Text ("
                let xformed = (indenter.Indent 1).Transform value.Value
                let coord = (indenter.Indent 1).Transform (value.Coordinate.ToString ())
                let textClose = indenter.Transform ")"
                $"%s{textOpen}\n%s{xformed}\n%s{coord}\n%s{textClose}"

            tail
            |> formatTokens $"%s{current}\n%s{v}" indenter
        | (Lisp lisps)::tail ->
            let lispOpen = "Lisp [" |> indenter.Transform
            let lispClose = "]" |> indenter.Transform
            let lispString =
                lisps
                |> formatLisps "" (indenter.Indent 1)

            if 0 < current.Length then
                tail
                |> formatTokens $"%s{current}\n%s{lispOpen}\n%s{lispString}\n%s{lispClose}" indenter
            else
                tail
                |> formatTokens $"%s{lispOpen}\n%s{lispString}\n%s{lispClose}" indenter

    let indenter = Indent.IndentTransformer ()

    match maybeTokens with
    | Ok tokens ->
        let opn = "Ok (" |> indenter.Transform
        let cls = ")" |> indenter.Transform
        let value =
            tokens
            |> formatTokens "" (indenter.Indent 1)

        $"%s{opn}\n%s{value}\n%s{cls}"
    | Error errorValue -> $"Error %A{errorValue}"
