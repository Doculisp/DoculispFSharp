module Doculisp.Lib.Externals

open System.IO
open Doculisp.Lib
open Doculisp.Lib.SymantecTypes
open Doculisp.Lib.IoHelpers
open Doculisp.Lib.TextHelpers

let rec processFile (path: string) =
    let currentDir =
        Directory.GetCurrentDirectory ()

    let sourceDir =
        path
        |> getDirectoryFromFilePath

    let path =
        path
        |> getFullPath

    try
        Directory.SetCurrentDirectory sourceDir

        path
        |> loadFile
        |> stringMaybeToSeqMaybe
        |> Document.map path
        |> Tokenizer.parse path
        |> SymantecBuilder.build path
        |> load
    finally
        Directory.SetCurrentDirectory currentDir

and private loadExternals (externals: External list) =
    let rec loadExternals (acc: External list) (externals: External list) =
        match externals with
        | [] -> acc |> List.sortBy _.Index |> Ok
        | ({ Content = Loaded _ } as ext)::tail ->
            tail
            |> loadExternals (ext::acc)
        | external::tail ->
            let sectionMaybe =
                external.Path
                |> processFile

            match sectionMaybe with
            | Error errorValue -> Error errorValue
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
