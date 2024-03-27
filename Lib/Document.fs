module Doculisp.Lib.Document

open Doculisp.Lib.TextHelpers
open Doculisp.Lib.DocumentTypes

let private mapText (linePtr: int) (charPt: int) (document: char list) =
    let rec map (linePtr: int) (charPtr: int) (start: Coordinate option) (current: string) (document: char list) =
        match document, start with
        | [], Some st ->
            TextMap (current, st)
        | IsNewLine (ln, tail), _ ->
            tail
            |> map (linePtr + 1) 0 start $"%s{current}%s{ln}"
        | c::tail, None ->
            tail
            |> map linePtr (charPt + 1) (Some { Line = linePtr; Char = charPt }) $"%s{current}%c{c}"
        | c::tail, _ ->
            tail
            |> map linePtr (charPt + 1) start $"%s{current}%c{c}"
            
    document
    |> map linePtr charPt None ""
    

let map (document: char seq) =
    let rec map (linePtr: int) (charPtr: int) (acc: DocumentMap list) (document: char list) =
        match document with
        | [] ->
            acc
            |> List.sortBy (fun t -> t.Coordinate)
        
    []