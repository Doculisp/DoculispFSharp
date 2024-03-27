module Doculisp.Lib.Document

open Doculisp.Lib.TextHelpers
open Doculisp.Lib.DocumentTypes

let private mapText (linePtr: int) (charPtr: int) (document: char list) =
    let rec map (linePtr: int) (charPtr: int) (start: Coordinate option) (current: string) (document: char list) =
        match document, start with
        | [], Some st ->
            TextMap (current.Trim(), st), linePtr, charPtr, document
        | IsNewLine (ln, tail), _ ->
            tail
            |> map (linePtr + 1) 0 start $"%s{current}%s{ln}"
        | IsWhiteSpace (sp, tail), None ->
            tail
            |> map linePtr (charPtr + sp.Length) start current
        | c::tail, None ->
            tail
            |> map linePtr (charPtr + 1) (Some { Line = linePtr; Char = charPtr }) $"%s{current}%c{c}"
        | c::tail, _ ->
            tail
            |> map linePtr (charPtr + 1) start $"%s{current}%c{c}"
            
    document
    |> map linePtr charPtr None ""
    

let map (document: char seq) =
    let rec map (linePtr: int) (charPtr: int) (acc: DocumentMap list) (document: char list) =
        match document with
        | [] ->
            acc
            |> List.sortBy (fun t -> t.Coordinate)
        | IsNewLine (_, tail) ->
            tail
            |> map (linePtr + 1) 0 acc
        | _ ->
            let mapped, line, c, tail =
                document
                |> mapText linePtr charPtr
            tail
            |> map line c (mapped::acc)
        
    document
    |> List.ofSeq
    |> map 0 0 []