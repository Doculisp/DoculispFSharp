module Doculisp.Lib.SymantecBuilder

open Doculisp.Lib
open Doculisp.Lib.TokenTypes
open Doculisp.Lib.SymantecTypes
open Doculisp.Lib.TextHelpers

let toLinkString (value: string) =
    $"#%s{value.ToLower().Replace(' ', '-')}"

let private ToSafeLinkString (value: string option) (defaultValue: string) =
    match value with
    | Some v -> v
    | _ -> defaultValue

let private getHeadingDepth { Value = value; Coordinate = c } =
    let rec getHeading (depth: int) (value: char list) =
        match value with
        | [] -> Ok depth
        | '#'::tail ->
            tail
            |> getHeading (depth + 1)
        | _ -> Error $"Heading at %s{c.ToString ()} is malformed."

    value
    |> List.ofSeq
    |> getHeading 0

let rec private advanceToClose (tokens: LispToken list) =
    match tokens with
    | [] as tail
    | (Close _)::tail -> tail
    | _::tail ->
        tail
        |> advanceToClose

let private comment (tokens: LispToken list) =
    let rec comment (depth: int) (tokens: LispToken list) =
        match tokens with
        | [] -> []
        | (Close _)::tail when depth = 1 -> tail
        | (Close _)::tail -> tail |> comment (depth - 1)
        | (Open _)::tail -> tail |> comment (depth + 1)
        | _::tail -> tail |> comment depth

    tokens
    |> comment 0

let rec getParameter (tokens: LispToken list) =
    match tokens with
    | (Open { Value = name; Coordinate = _ })::_ when name.StartsWith "*" ->
        tokens
        |> comment
        |> getParameter
    | (Parameter value)::tail ->
        (Some value.Value), tail
    | _ -> None, tokens

let private getSectionMetaBlock (tokens: Token list) =
    let getSubBlocks (tokens: LispToken list) =
        let getExternals (tokens: LispToken list) =
            let rec getExternals (depth:int) (i: int) (acc: External list) (tokens: LispToken list) =
                match tokens with
                | [] ->
                    Ok (acc |> List.sortBy _.Index, tokens)
                | (Close _)::_ when depth < 1 ->
                    Ok (acc |> List.sortBy _.Index, tokens)
                | (Close _)::tail ->
                    tail
                    |> getExternals (depth - 1) i acc
                | (Open { Value = name; Coordinate = _ })::_ when name.StartsWith "*" ->
                    let tail = tokens |> comment
                    tail
                    |> getExternals depth i acc
                | (Open { Value = name; Coordinate = c })::tail ->
                    let maybePath, rest = tail |> getParameter

                    match maybePath with
                    | None -> Error $"External Section at %s{c.ToString ()} does not have a path"
                    | Some path ->
                        let ex =
                            {
                                Label = name
                                Path = path
                                Content = Waiting
                                Index = i
                            }

                        rest
                        |> getExternals (depth + 1) (i + 1) (ex::acc)

            tokens
            |> getExternals 0 0 []

        let rec getSectionParts (depth: int) (title: string option) (subtitle: string option) (link: string option) (externals: External list) (tokens: LispToken list) =
            match tokens with
            | [] -> Ok (title, subtitle, link, externals, [])
            | (Close _)::tail when 0 = depth ->
                Ok (title, subtitle, link, externals, tail)
            | (Close _)::tail ->
                tail
                |> getSectionParts (depth - 1) title subtitle link externals
            // -------- TITLE --------
            | (Open { Value = "title"; Coordinate = c })::_ when title.IsSome ->
                Error $"Title at %s{c.ToString ()} is a duplicate."
            | (Open { Value = "title"; Coordinate = c })::tail ->
                let maybeTitle, rest = tail |> getParameter

                match maybeTitle with
                | None -> Error $"Title at %s{c.ToString ()} is missing parameter."
                | Some t ->
                    rest
                    |> getSectionParts (depth + 1) (Some t) subtitle link externals
            // -------- SUB-TITLE --------
            | (Open { Value = "subtitle"; Coordinate = c })::_ when subtitle.IsSome ->
                Error $"Duplicate subtitle at %s{c.ToString ()}"
            | (Open { Value = "subtitle"; Coordinate = c })::tail ->
                let maybeSubtitle, rest = tail |> getParameter

                match maybeSubtitle with
                | None -> Error $"Subtitle at %s{c.ToString ()} is missing parameter."
                | Some t ->
                    rest
                    |> getSectionParts (depth + 1) title (Some t) link externals
            // -------- LINK --------
            | (Open { Value = "link"; Coordinate = c })::_ when link.IsSome ->
                Error $"Link at %s{c.ToString ()} is a duplicate."
            | (Open { Value = "link"; Coordinate = c })::tail ->
                let maybeLink, rest = tail |> getParameter

                match maybeLink with
                | None -> Error $"Link at %s{c.ToString ()} is missing parameter."
                | Some l ->
                    rest
                    |> getSectionParts (depth + 1) title subtitle (Some l) externals
            // -------- External --------
            | (Open { Value = "external"; Coordinate = c })::_ when 0 < externals.Length ->
                Error $"External at %s{c.ToString ()} is duplicate."
            | (Open { Value = "external"; Coordinate = c })::tail ->
                let maybeResult = tail |> getExternals

                match maybeResult with
                | Error errorValue -> Error errorValue
                | Ok (ext, rest) ->
                    rest
                    |> getSectionParts (depth + 1) title subtitle link ext
            // -------- COMMENT --------
            | (Open { Value = name; Coordinate = _ })::_ when name.StartsWith "*" ->
                let tail = tokens |> comment
                tail
                |> getSectionParts depth title subtitle link externals
            // -------- UNKNOWN (ERROR) --------
            | (Open { Value = name; Coordinate = c })::_ ->
                Error $"Unknown block %A{name} at %s{c.ToString ()}"

        tokens
        |> getSectionParts 0 None None None []

    let getSectionFromLisp (tokens: LispToken list) =
        match tokens with
        | (Open {Value = "section-meta"; Coordinate = c})::tail ->
            let result =
                tail
                |> getSubBlocks

            match result with
            | Ok (title, subtitle, link, externals, rest) ->
                match title with
                | None -> Error $"Doculisp section-meta block at %s{c.ToString ()} does not have a title block."
                | Some value ->
                    Ok (
                        true,
                        {
                            Title = value
                            Subtitle = subtitle
                            Link = value |> toLinkString |> ToSafeLinkString link
                            Coordinate = c
                            Externals = externals
                            Parts = []
                            Table = NoTable
                        },
                        rest
                    )
            | Error errorValue -> Error errorValue
        | _ ->
            Ok (
                false,
                {
                    Title = ""
                    Subtitle = None
                    Link = ""
                    Coordinate = { Line = 0; Char = 0 }
                    Externals = []
                    Parts = []
                    Table = NoTable
                },
                tokens
            )

    let getSectionMeta (tokens: Token list) =
        match tokens with
        | (Lisp ((Open { Value = "section-meta"; Coordinate = _coordinate })::_ as lisp))::tail ->
            let result =
                lisp |> getSectionFromLisp

            match result with
            | Error errorValue -> Error errorValue
            | Ok (found, content, rest) ->
                (found, content
                |> Content, (Lisp rest)::tail)
                |> Ok
        | _ -> (false, Empty, tokens) |> Ok

    tokens
    |> getSectionMeta

let private getMarkdown (tokens: Token list) =
    let getMarkdownFromValues (values: Value list) =
        let rec getMarkdown (st: Coordinate) (current: string) (values: Value list) =
            match values with
            | [] -> { Value = current; Coordinate = st } |> Markdown
            | { Value = text; Coordinate = _ }::tail ->
                let v = text |> addTo current " "

                tail
                |> getMarkdown st v

        let st = values.Head |> _.Coordinate
        values
        |> getMarkdown st ""

    let rec getAllOnSameLine (line: int) (acc: Value list) (tokens: Token list) =
        match tokens with
        | [] -> (acc |> getMarkdownFromValues), tokens
        | (Text text)::tail when line = text.Coordinate.Line ->
            tail
            |> getAllOnSameLine line (text::acc)
        | _ -> (acc |> getMarkdownFromValues), tokens

    let rec getMarkdown (acc: Part list) (tokens: Token list) =
        match tokens with
        | []
        | (Lisp _)::_ -> (acc |> List.sortBy _.Coordinate), tokens
        | Text { Value = _; Coordinate = c }::_ ->
            let md, tail =
                tokens
                |> getAllOnSameLine c.Line []

            tail
            |> getMarkdown (md::acc)

    tokens
    |> getMarkdown []

let private getContentLocation (hasExternals: bool) (tokens: LispToken list) =
    let getToc (tokens: LispToken list) =
        match tokens with
        | (Open { Value = "toc"; Coordinate = c })::tail ->
            let parameter, rest = tail |> getParameter
            let toc =
                match parameter with
                | None -> NoTable |> Ok
                | Some value ->
                    match value.ToLowerInvariant () with
                    | "no-table" -> NoTable |> Ok
                    | "unlabeled" -> Unlabeled |> Ok
                    | "labeled" -> Labeled |> Ok
                    | "numbered" -> Numbered |> Ok
                    | "numbered-labeled" -> NumberedAndLabeled |> Ok
                    | "bulleted" -> Bulleted |> Ok
                    | "bulleted-labeled" -> BulletedAndLabeled |> Ok
                    | _ -> Error $"TOC at %s{c.ToString ()} has invalid parameter %A{value}."

            toc, rest |> advanceToClose
        | _ -> NoTable |> Ok, tokens

    match tokens with
        | (Open { Value = "content"; Coordinate = c })::tail ->
            let toc, rest =
                tail
                |> getToc

            match toc with
            | Error errorValue -> Error errorValue
            | Ok resultValue ->
                if hasExternals then
                    (ContentPlaceHolder c, resultValue, rest) |> Ok
                else
                    Error $"Doculisp content block at %s{c.ToString ()} does not have any external content in section-meta."

let getHeading (tokens: LispToken list) =
    match tokens with
    | (Open ({ Value = value; Coordinate = c } as heading))::tail when value.StartsWith "#" ->
        let depthMaybe =
            heading |> getHeadingDepth

        match depthMaybe with
        | Error errorValue -> Error errorValue
        | Ok depth ->
            let textMaybe, rest = tail |> getParameter

            match textMaybe with
            | None -> Error $"Heading at %s{c.ToString ()} does not have text."
            | Some text ->
                ({
                    Depth = depth
                    Value = text
                    Coordinate = c
                }
                |> Heading, rest |> advanceToClose)
                |> Ok

let getLispParts (hasExternals: bool) (toc: TableOfContentsDefinition option) (lisps: LispToken list) =
    let rec getParts (acc: Part list) (toc: TableOfContentsDefinition option) (lisps: LispToken list) =
        match lisps, toc with
        | [], _ -> (acc |> List.sortBy _.Coordinate, toc) |> Ok
        | (Open { Value = "content"; Coordinate = _ })::_, None ->
            let result =
                lisps |> getContentLocation hasExternals

            match result with
            | Error errorValue -> Error errorValue
            | Ok (part, toc, tail) ->
                tail
                |> getParts (part::acc) (Some toc)
        | (Open { Value = "content"; Coordinate = c })::_, _ ->
            Error $"Duplicate content at %s{c.ToString ()}."
        | (Open { Value = value; Coordinate = _ })::_, _ when value.StartsWith "#" ->
            let result = lisps |> getHeading

            match result with
            | Error errorValue -> Error errorValue
            | Ok (part, tail) ->
                tail
                |> getParts (part::acc) toc
        | (Open { Value = value; Coordinate = _ })::_, _ when value.StartsWith "*" ->
            let tail =
                lisps |> comment

            tail
            |> getParts acc toc
        | (Open { Value = "section-meta"; Coordinate = c })::_, _ ->
            Error $"Duplicate section-meta at %s{c.ToString ()}."
        | (Open { Value = value; Coordinate = c })::_, _ ->
            Error $"Unknown atom %A{value} at %s{c.ToString ()}."
        | (Close _)::tail, _ ->
            tail
            |> getParts acc toc

    lisps
    |> getParts [] toc

let private getParts (hasSection: bool) (hasExternals: bool) (tokens: Token list) =
    let rec getParts (acc: Part list) (toc: TableOfContentsDefinition option) (tokens: Token list) =
        match tokens with
        | [] -> (acc |> List.sortBy _.Coordinate, toc) |> Ok
        | (Text _)::_ ->
            let md, tail =
                tokens |> getMarkdown

            tail
            |> getParts ([md; acc] |> List.concat) toc
        | (Lisp lisps)::tail when hasSection ->
            let result =
                lisps |> getLispParts hasExternals toc

            match result with
            | Error errorValue -> Error errorValue
            | Ok (parts, nToc) ->
                tail
                |> getParts ([parts; acc] |> List.concat) nToc
        | (Lisp (lisp::tail))::_ ->
            Error $"Doculisp block at %s{lisp.Coordinate.ToString ()} appears before section-meta block."

    tokens
    |> getParts [] None

let private buildTree (fileName: string) (tokens: Token list) =
    let section =
        tokens |> getSectionMetaBlock

    match section with
    | Error errorValue -> Error $"%s{fileName}\n\n%s{errorValue}"
    | Ok (_, Empty, tail) ->
        let partsMaybe =
            tail
            |> getParts false false

        match partsMaybe with
        | Error errorValue -> Error $"%s{fileName}\n\n%s{errorValue}"
        | Ok (parts, _) ->
            if 0 < parts.Length then
                {
                    Title = ""
                    Subtitle = None
                    Link = ""
                    Table = NoTable
                    Coordinate = { Line = -1; Char = -1 }
                    Parts = parts
                    Externals = []
                }
                |> Content
                |> Ok
            else
                Empty |> Ok

    | Ok (found, Content content, tail) ->
        let result = tail |> getParts found (0 < content.Externals.Length)

        match result with
        | Error errorValue -> Error errorValue
        | Ok (parts, tocMaybe) ->
            let content =
                match tocMaybe with
                | None -> content
                | Some value ->
                    { content with Table = value }

            { content with
                Parts = parts
            }
            |> Content
            |> Ok

let build (fileName: string) (document: Result<Token list, string>) =
    document
    |> combine (buildTree fileName)
