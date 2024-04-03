module Doculisp.Lib.SymantecBuilder

open Doculisp.Lib
open Doculisp.Lib.TokenTypes
open Doculisp.Lib.SymantecTypes

let toLinkString (value: string) =
    $"#%s{value.ToLower().Replace(' ', '_')}"

let private ToSafeLinkString (value: string option) (defaultValue: string) =
    match value with
    | Some v -> v
    | _ -> defaultValue

let private buildTree (tokens: Token list) =
    let getSectionParts (tokens: LispToken list) =
        let comment (tokens: LispToken list) =
            let rec comment (depth: int) (tokens: LispToken list) =
                match tokens with
                | [] -> []
                | (Close _)::tail when depth = 1 -> tail
                | (Close _)::tail -> tail |> comment (depth - 1)
                | (Open _)::tail -> tail |> comment (depth + 1)

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

        let getExternals (tokens: LispToken list) =
            let rec getExternals (depth:int) (i: int) (acc: External list) (tokens: LispToken list) =
                match tokens with
                | [] ->
                    Ok (acc |> List.sortBy _.Index, tokens)
                | (Close _)::_ when depth < 0 ->
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
            | (Close _)::tail when 1 = depth ->
                Ok (title, subtitle, link, externals, tail)
            | (Close _)::tail ->
                tail
                |> getSectionParts (depth - 1) title subtitle link externals
            | (Open { Value = "title"; Coordinate = c })::_ when title.IsSome ->
                Error $"Title at %s{c.ToString ()} is a duplicate."
            | (Open { Value = "title"; Coordinate = c })::tail ->
                let maybeTitle, rest = tail |> getParameter

                match maybeTitle with
                | None -> Error $"Title at %s{c.ToString ()} is missing parameter."
                | Some t ->
                    rest
                    |> getSectionParts (depth + 1) (Some t) subtitle link externals
            | (Open { Value = "subtitle"; Coordinate = c })::_ when subtitle.IsSome ->
                Error $"Duplicate subtitle at %s{c.ToString ()}"
            | (Open { Value = "subtitle"; Coordinate = c })::tail ->
                let maybeSubtitle, rest = tail |> getParameter

                match maybeSubtitle with
                | None -> Error $"Subtitle at %s{c.ToString ()} is missing parameter."
                | Some t ->
                    rest
                    |> getSectionParts (depth + 1) title (Some t) link externals

            | (Open { Value = "link"; Coordinate = c })::_ when link.IsSome ->
                Error $"Link at %s{c.ToString ()} is a duplicate."
            | (Open { Value = "link"; Coordinate = c })::tail ->
                let maybeLink, rest = tail |> getParameter

                match maybeLink with
                | None -> Error $"Link at %s{c.ToString ()} is missing parameter."
                | Some l ->
                    rest
                    |> getSectionParts (depth + 1) title subtitle (Some l) externals

            | (Open { Value = "external"; Coordinate = c })::_ when 0 < externals.Length ->
                Error $"External at %s{c.ToString ()} is duplicate."
            | (Open { Value = "external"; Coordinate = c })::tail ->
                let maybeResult = tail |> getExternals

                match maybeResult with
                | Error errorValue -> Error errorValue
                | Ok (ext, rest) ->
                    rest
                    |> getSectionParts (depth + 1) title subtitle link ext

            | (Open { Value = name; Coordinate = _ })::_ when name.StartsWith "*" ->
                let tail = tokens |> comment
                tail
                |> getSectionParts depth title subtitle link externals

            | (Open { Value = name; Coordinate = c })::_ ->
                Error $"Unknown block %A{name} at %s{c.ToString ()}"

        match tokens with
        | (Open {Value = "section-meta"; Coordinate = c})::tail ->
            let result =
                tail
                |> getSectionParts 0 None None None []

            match result with
            | Ok (title, subtitle, link, externals, rest) ->
                match title with
                | None -> Error $"Section meta at %s{c.ToString ()} does not have a title"
                | Some value ->
                    Ok (
                        true,
                        {
                            Title = value
                            Subtitle = subtitle
                            Link = value |> toLinkString |> ToSafeLinkString link
                            Coordinate = c
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
                    Parts = []
                    Table = NoTable
                },
                tokens
            )

    // let getSectionMeta (tokens: Token list) =
    //     match tokens with
    //     | (Lisp ((Open { Value = "section-meta"; Coordinate = coordinate })::lisps))::tail -> None

    Empty |> Ok

let build (document: Result<Token list, string>) =
    combine buildTree document
