module Doculisp.Lib.Externals

open Doculisp.Lib
open Doculisp.Lib.SymantecTypes
open Doculisp.Lib.IoHelpers
open Doculisp.Lib.TextHelpers

let rec private loadExternals (externals: External list) =
    let rec loadExternals (acc: External list) (externals: External list) =
        match externals with
        | [] -> acc |> List.sortBy _.Index |> Ok
        | ({ Content = Loaded _ } as ext)::tail ->
            tail
            |> loadExternals (ext::acc)
        | external::tail ->
            let file =
                external.Path
                |> loadFile

            let sectionMaybe =
                file
                |> stringMaybeToSeqMaybe
                |> Document.map external.Path
                |> Tokenizer.parse
                |> SymantecBuilder.build
                |> load

            match sectionMaybe with
            | Error errorValue -> Error $"%A{external.Path} has error:\n%A{errorValue}"
            | Ok tree ->
                let ext =

                    { external with
                        Content = Loaded tree
                    }

                tail
                |> loadExternals (ext::acc)

    externals
    |> loadExternals []

and load (maybeTree: Result<Tree, string>) =
    match maybeTree with
    | Error errorValue -> Error errorValue
    | Ok Empty -> Empty |> Ok
    | Ok (Content content) ->
        let externalsMaybe =
            content.Externals |> loadExternals

        match externalsMaybe with
        | Error errorValue -> Error errorValue
        | Ok externals ->
            { content with
                Externals = externals
            }
            |> Content
            |> Ok
