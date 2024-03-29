[<AutoOpen>]
module Doculisp.Tests.TestHelpers

open Archer.Logger
open Archer.Arrows
open Archer.ApprovalsSupport
open ApprovalTests
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes
open Doculisp.Lib.TokenTypes

let reporter =
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

let setupApprovals =
    Setup (fun _ ->
        reporter
        |> Ok
    )

let formatMaybe (fn: IIndentTransformer -> 'a -> string) (value: Result<'a, string>) =
    match value with
    | Error err -> $"Error %A{err}"
    | Ok good ->
        let indenter = Indent.IndentTransformer ()
        let opn = "Ok (" |> indenter.Transform
        let cls = ")" |> indenter.Transform
        let value =
            good
            |> fn (indenter.Indent 1)

        $"%s{opn}\n%s{value}\n%s{cls}"

let formatMap (maybeMap: Result<DocumentMap list, string>) =
    let rec format (current: string) (indenter: IIndentTransformer) (maps: DocumentMap list) =
        match maps with
        | [] -> current
        | (TextMap mapped)::tail ->
            let opn = $"Text %s{mapped.Coordinate.ToString ()} (" |> indenter.Transform
            let cls = ")" |> indenter.Transform
            let mdl = mapped.Value |> (indenter.Indent 1).Transform
            let value = $"%s{opn}\n%s{mdl}\n%s{cls}"
            tail
            |> format $"%s{current}\n\n%s{value}" indenter
        | (LispMap mapped)::tail ->
            let opn = $"Lisp %s{mapped.Coordinate.ToString ()} (" |> indenter.Transform
            let cls = ")" |> indenter.Transform
            let mdl = mapped.Value |> (indenter.Indent 1).Transform
            let value = $"%s{opn}\n%s{mdl}\n%s{cls}"
            tail
            |> format $"%s{current}\n\n%s{value}" indenter

    formatMaybe (format "") maybeMap

let formatTokens (maybeTokens: Result<Token list, string>) =
    let rec formatLisps (current: string) (indenter: IIndentTransformer) (tokens: LispToken list) =
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

    formatMaybe (formatTokens "") maybeTokens
