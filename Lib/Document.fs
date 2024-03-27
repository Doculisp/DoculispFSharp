module Doculisp.Lib.Document

open Doculisp.Lib.TextHelpers
open Doculisp.Lib.DocumentTypes

let private (|IsOpenComment|_|) = function
    | '<'::'!'::'-'::'-'::tail ->
        Some ("<!--", tail)
    | _ -> None
    
let private (|IsCloseComment|_|) = function
    | '-'::'-'::'>'::tail ->
        Some ("-->", tail)
    | _ -> None
    
let private mapComment (linePtr: int) (charPtr: int) (document: char list) =
    let rec map (linePtr: int) (charPtr: int) (start: Coordinate option) (document: char list) =
        match document, start with
        | [], Some st -> Error $"Comment at %s{st.ToString ()} is not closed"
        | IsOpenComment (op, tail), None ->
            tail
            |> map linePtr (charPtr + op.Length) (Some { Line = linePtr; Char = charPtr })
        | IsCloseComment (cl, tail), Some _ ->
            Ok (linePtr, charPtr + cl.Length, tail)
        | IsNewLine (_, tail), _ ->
            tail
            |> map (linePtr + 1) 0 start
        | _::tail, _ ->
            tail
            |> map linePtr (charPtr + 1) start
        
    document
    |> map linePtr charPtr None

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
        | IsOpenComment _, Some st ->
            TextMap (current.Trim (), st), linePtr, charPtr, document 
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
            |> Ok
        | IsNewLine (_, tail) ->
            tail
            |> map (linePtr + 1) 0 acc
        | IsWhiteSpace(sp, tail) ->
            tail
            |> map linePtr (charPtr + sp.Length) acc
        | IsOpenComment _ ->
            let result =
                document
                |> mapComment linePtr charPtr
                
            match result with
            | Ok (line, c, tail) ->
                tail
                |> map line c acc
            | Error errorMessage -> Error errorMessage
        | _ ->
            let mapped, line, c, tail =
                document
                |> mapText linePtr charPtr
            tail
            |> map line c (mapped::acc)
        
    document
    |> List.ofSeq
    |> map 0 0 []