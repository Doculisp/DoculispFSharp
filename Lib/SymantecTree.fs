module Doculisp.Lib.SymantecTree

open System.Text
open Doculisp.Lib
open Doculisp.Lib.SymantecTypes

let private formatHeading (depth: int) (text: string) =
    let hd = "".PadLeft (depth + 1, '#')
    $"%s{hd} %s{text} %s{hd}"

type private Builder (builder: StringBuilder) =
    member this.AppendWithSpace (text: string) =
        text
        |> this.Append

        ""
        |> builder.AppendLine
        |> ignore

    member _.Append (text: string) =
        text
        |> builder.AppendLine
        |> ignore

    member _.AppendLine () =
        builder.AppendLine ""

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
        content.Externals
        |> List.iter (fun external ->
            $"# [%s{external.Title}](%s{external.Link})"
            |> builder.Append
        )
    | NumberedAndLabeled ->
        content.Externals
        |> List.iter (fun external ->
            $"# [%s{external.Label}: %s{external.Title}](%s{external.Link})"
            |> builder.Append
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


let processBuildResult (maybeTree: Result<Tree, string>) =
    let sb = StringBuilder ()
    let builder = sb |> Builder

    match maybeTree with
    | Error errorValue -> Error errorValue
    | Ok tree ->
        tree
        |> processTree builder 0

        sb.ToString ()
        |> Ok
