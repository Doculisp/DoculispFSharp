module Doculisp.Lib.SymantecTree

open System.IO
open Doculisp.Lib
open Doculisp.Lib.SymantecTypes

let private formatHeading (depth: int) (text: string) =
    let hd = "".PadLeft (depth + 1, '#')
    $"%s{hd} %s{text} %s{hd}"

type private Builder (builder: TextWriter) =
    member this.AppendWithSpace (text: string) =
        text
        |> this.Append

        ""
        |> builder.WriteLine

    member _.Append (text: string) =
        text
        |> builder.WriteLine

    member _.AppendLine () =
        builder.WriteLine ()

let private processToc (builder: Builder) (content: Content) =
    match content.Table with
    | NoTable -> ()
    | Unlabeled ->
        content.Externals
        |> List.iter (fun external ->
            $"[%s{external.Title}](%s{external.Link})"
            |> builder.Append
        )
    | Labeled ->
        content.Externals
        |> List.iter (fun external ->
            $"[%s{external.Label}: %s{external.Title}](%s{external.Link})"
            |> builder.Append
        )
    | Numbered ->
        let mutable cnt = 1
        content.Externals
        |> List.iter (fun external ->
            $"%d{cnt}. [%s{external.Title}](%s{external.Link})"
            |> builder.Append
            cnt <- cnt + 1
        )
    | NumberedAndLabeled ->
        let mutable cnt = 1
        content.Externals
        |> List.iter (fun external ->
            $"%d{cnt}. [%s{external.Label}: %s{external.Title}](%s{external.Link})"
            |> builder.Append
            cnt <- cnt + 1
        )
    | Bulleted ->
        content.Externals
        |> List.iter (fun external ->
            $"- [%s{external.Title}](%s{external.Link})"
            |> builder.Append
        )
    | BulletedAndLabeled ->
        content.Externals
        |> List.iter (fun external ->
            $"- [%s{external.Label}: %s{external.Title}](%s{external.Link})"
            |> builder.Append
        )

    builder.AppendLine ()

let rec private processParts (content: Content) (builder: Builder) (depth: int) (parts: Part list) =
    let rec processEachPart (parts: Part list) =
        match parts with
        | [] -> ()
        | (Markdown md)::tail ->
            md.Value
            |> builder.AppendWithSpace

            tail
            |> processEachPart
        | (Heading heading)::tail ->
            heading.Value
            |> formatHeading (heading.Depth + depth)
            |> builder.AppendWithSpace

            tail
            |> processEachPart
        | (ContentPlaceHolder _)::tail ->
            content
            |> processToc builder

            content.Externals
            |> List.iter (fun external ->
                match external.Content with
                | Waiting -> ()
                | Loaded tree ->
                    tree
                    |> processTree builder (depth + 1)
            )

            tail
            |> processEachPart

    parts
    |> processEachPart

and private processContent (builder: Builder) (depth: int) (content: Content) =
    if 0 < content.Title.Length then
        content.Title
        |> formatHeading depth
        |> builder.AppendWithSpace

    match content.Subtitle with
    | None -> ()
    | Some value ->
        value
        |> formatHeading (depth + 2)
        |> builder.AppendWithSpace

and private processTree (builder: Builder) (depth: int) (tree: Tree) =
    match tree with
    | Empty -> ()
    | Content content ->
        content
        |> processContent builder depth

        content.Parts
        |> processParts content builder 0

let processBuildResult (writer: TextWriter) (maybeTree: Result<Tree, string>) =
    let builder = writer |> Builder

    try
        maybeTree
        |> combine (processTree builder 0 >> Ok)
    with
    | e ->
        $"%A{e}"
        |> Error
