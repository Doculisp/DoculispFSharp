[<AutoOpen>]
module Doculisp.Tests.TestHelpers

open Archer.Logger
open Archer.Arrows
open Archer.ApprovalsSupport
open ApprovalTests
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes
open Doculisp.Lib.TokenTypes
open Doculisp.Lib.SymantecTypes

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

let formatSymantecTree (maybeContent: Result<Tree, string>) =
    let rec formatContent (indenter: IIndentTransformer) (content: Content) =
        let getMaybe thing =
            match thing with
            | None -> "None"
            | Some value -> value

        let title = $"Title: %A{content.Title}" |> indenter.Transform
        let Subtitle = $"Subtitle: %A{content.Subtitle |> getMaybe}" |> indenter.Transform
        let link = $"Link: %A{content.Link}" |> indenter.Transform
        let toc = $"Table of Contents: %A{content.Table}"
        let partsA =
            let p : string = content.Parts |> formatPart "\n" (indenter.Indent 1)
            if 0 < p.Length then p
            else " None"

        let parts =
            let p = "Parts:" |> indenter.Transform
            $"%s{p}%s{partsA}"

        $"%s{title}\n%s{Subtitle}\n%s{link}\n%s{toc}\n%s{parts}"

    and formatPart (current: string) (indenter: IIndentTransformer) (parts: Part list) =
        let modifyCurrent heading text =
            let opn = $"%s{heading} (" |> indenter.Transform
            let cls = ")" |> indenter.Transform
            let value = text |> indenter.Transform

            $"%s{current}\n%s{opn}\n%s{value}\n%s{cls}"

        match parts with
        | [] -> current
        | (Markdown text)::tail ->
            let value = text.Value |> modifyCurrent "Markdown"

            tail
            |> formatPart value indenter
        | (Heading heading)::tail ->
            let pre = "".PadLeft (heading.Depth, '#')
            let value = $"%s{pre} %s{heading.Value}" |> modifyCurrent "Heading"

            tail
            |> formatPart value indenter
        | (External external)::tail ->
            let ext =
                $"%d{external.Index}. %s{external.Label}: %s{external.Path}"
            let content = formatContent indenter external.Content

            let value =
                $"%s{ext}\n%s{content}"
                |> modifyCurrent "External"

            tail
            |> formatPart value indenter

    let formatTree indenter (tree: Tree) =
        match tree with
        | Empty -> "Empty"
        | Content content -> formatContent indenter content

    maybeContent
    |> formatMaybe formatTree
