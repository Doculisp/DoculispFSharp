﻿module Doculisp.Lib.Document

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
    
let private (|IsInline|_|) = function
    | '`'::tail ->
        Some ("`", tail)
    | _ -> None

let private (|IsMultiline|_|) = function
    | '`'::'`'::'`'::tail ->
        Some ("```", tail)
    | _ -> None

let private (|IsStartLisp|_|) = function
    | '('::'d'::'l'::c::tail when c |> System.Char.IsWhiteSpace ->
        Some ($"(dl", c::tail)
    | _ -> None

let private mapLisp (linePtr: int) (charPtr: int) (document: char list) =
    let rec map (linePtr: int) (charPtr: int) (depth: int) (trueStart: Coordinate) (start: Coordinate option) (current: string) (document: char list) =
        match document, start with
        | [], Some st ->
            Error $"Doculisp block at %s{trueStart.ToString ()} is not closed."
        | IsStartLisp (lisp, tail), None ->
            tail
            |> map linePtr (charPtr + lisp.Length) (depth + 1) (charPtr |> getCoordinate linePtr) None current
        | IsEscaped '(' (esc, tail), _ ->
            tail
            |> map linePtr (charPtr + esc.Length) depth trueStart start $"%s{current}%s{esc}"
        | IsEscaped ')' (esc, tail), _ ->
            tail
            |> map linePtr (charPtr + esc.Length) depth trueStart start $"%s{current}%s{esc}"
        | '('::tail, None ->
            tail
            |> map linePtr (charPtr + 1) (depth + 1) trueStart (charPtr |> getCoordinate linePtr |> Some) $"%s{current}("
        | '('::tail, _ ->
            tail
            |> map linePtr (charPtr + 1) (depth + 1) trueStart start $"%s{current}("
        | ')'::tail, _ when 1 < depth ->
            tail
            |> map linePtr (charPtr + 1) (depth - 1) trueStart start $"%s{current})"
        | ')'::tail, Some st ->
            Ok (LispMap { Value = $"%s{current}".Trim(); Coordinate = st }, linePtr, charPtr, tail)
        | IsNewLine (ln, tail), _ ->
            tail
            |> map (linePtr + 1) 0 depth trueStart start $"%s{current}%s{ln}"
        | c::tail, _ ->
            tail
            |> map linePtr (charPtr + 1) depth trueStart start $"%s{current}%c{c}"

    document
    |> map linePtr charPtr 0 { Line = -1; Char = -1 } None ""

let private mapMultilineCodeBlock (linePtr: int) (charPtr: int) (document: char list) =
    let rec map (linePtr: int) (charPtr: int) (start: Coordinate option) (current: string) (document: char list) =
        match document, start with
        | [], Some st ->
            Error $"Multiline code block at %s{st.ToString ()} is not closed."
        | IsMultiline (ln, tail), None ->
            tail
            |> map linePtr (charPtr + ln.Length) (Some { Line = linePtr; Char = charPtr }) $"%s{current}%s{ln}"
        | IsMultiline (ln, tail), Some st ->
            Ok (TextMap { Value = $"%s{current}%s{ln}".Trim (); Coordinate = st }, linePtr, charPtr + ln.Length, tail)
        | IsEscaped '`' (esc, tail), _ ->
            tail
            |> map linePtr (charPtr + esc.Length) start $"%s{current}%s{esc}"
        | IsNewLine (ln, tail), _ ->
            tail
            |> map (linePtr + 1) 0 start $"%s{current}%s{ln}"
        | c::tail, _ ->
            tail
            |> map linePtr (charPtr + 1) start $"%s{current}%c{c}"

    document
    |> map linePtr charPtr None ""

let private mapInlineCodeBlock (linePtr: int) (charPtr: int) (document: char list) =
    let rec map (linePtr: int) (charPtr: int) (start: Coordinate option) (current: string) (document: char list) =
        match document, start with
        | [], Some st ->
            Error $"Inline code block at %s{st.ToString ()} is not closed."
        | IsEscaped '`' (esc, tail), _ ->
            tail
            |> map linePtr (charPtr + esc.Length) start $"%s{current}%s{esc}"
        | IsInline (inl, tail), None ->
            tail
            |> map linePtr (charPtr + inl.Length) (charPtr |> getCoordinate linePtr |> Some) $"%s{current}%s{inl}"
        | IsInline (inl, tail), Some st ->
            Ok (TextMap { Value = $"%s{current}%s{inl}"; Coordinate = st }, linePtr, charPtr, tail)
        | IsNewLine _, Some st ->
            Error $"Inline code block at %s{st.ToString ()} contains a new line."
        | c::tail, _ ->
            tail
            |> map linePtr (charPtr + 1) start $"%s{current}%c{c}"

    document
    |> map linePtr charPtr None ""

let private mapComment (linePtr: int) (charPtr: int) (document: char list) =
    let rec map (linePtr: int) (charPtr: int) (acc: DocumentMap list) (start: Coordinate option) (document: char list) =
        match document, start with
        | [], Some st -> Error $"Comment at %s{st.ToString ()} is not closed."
        | IsOpenComment (op, tail), None ->
            tail
            |> map linePtr (charPtr + op.Length) acc (charPtr |> getCoordinate linePtr |> Some)
        | IsCloseComment (cl, tail), Some _ ->
            Ok (acc, linePtr, charPtr + cl.Length, tail)
        | IsNewLine (_, tail), _ ->
            tail
            |> map (linePtr + 1) 0 acc start
        | IsStartLisp _, _ ->
            let result =
                document
                |> mapLisp linePtr charPtr

            match result with
            | Ok (mapped, ln, c, tail) ->
                tail
                |> map ln c (mapped::acc) start
            | Error errorValue -> Error errorValue
        | _::tail, _ ->
            tail
            |> map linePtr (charPtr + 1) acc start
        
    document
    |> map linePtr charPtr [] None

let private mapText (linePointer: int) (charPointer: int) (documentChars: char list) =
    let mutable linePtr = linePointer
    let mutable charPtr = charPointer
    let mutable start: Coordinate option = None
    let mutable current = ""
    let mutable document: char list = documentChars
    let mutable finished = false
    let mutable endResult: Result<DocumentMap * int * int * char list, string> = Error "Not Run"

    while not finished do
        match document, start with
        | [], Some st ->
            endResult <- Ok (TextMap { Value = current.Trim (); Coordinate = st }, linePtr, charPtr, document)
            finished <- true
        | IsNewLine (ln, tail), _ ->
            document <- tail
            linePtr <- linePtr + 1
            charPtr <- 0
            current <- $"%s{current}%s{ln}"
        | IsOpenComment _, Some st ->
            endResult <- Ok (TextMap { Value = current.Trim (); Coordinate = st }, linePtr, charPtr, document)
            finished <- true
        | IsEscaped '`' (esc, tail), None ->
            document <- tail
            start <- (charPtr |> getCoordinate linePtr |> Some)
            charPtr <- charPtr + esc.Length
            current <- $"%s{current}%s{esc}"
        | IsEscaped '`' (esc, tail), _ ->
            document <- tail
            charPtr <- charPtr + esc.Length
            current <- $"%s{current}%s{esc}"
        | IsMultiline _, None ->
            let result =
                document
                |> mapMultilineCodeBlock linePtr charPtr

            match result with
            | Ok (mapped, ln, c, tail) ->
                document <- tail
                start <- (charPtr |> getCoordinate linePtr |> Some)
                linePtr <- ln
                charPtr <- c
                current <- $"%s{current}%s{mapped.Value}"
            | Error errorMessage ->
                endResult <- Error errorMessage
                finished <- true
        | IsMultiline _, _ ->
            let result =
                document
                |> mapMultilineCodeBlock linePtr charPtr

            match result with
            | Ok (mapped, ln, c, tail) ->
                document <- tail
                linePtr <- ln
                charPtr <- c
                current <- $"%s{current}%s{mapped.Value}"
            | Error errorMessage ->
                endResult <- Error errorMessage
                finished <- true
        | IsInline _, None _ ->
            let result =
                document
                |> mapInlineCodeBlock linePtr charPtr

            match result with
            | Ok (mapped, line, c, tail) ->
                document <- tail
                start <- charPtr |> getCoordinate linePtr |> Some
                linePtr <- line
                charPtr <- c
                current <- $"%s{current}%s{mapped.Value}"
            | Error errorMessage ->
                endResult <- Error errorMessage
                finished <- true
        | IsInline _, Some _ ->
            let result =
                document
                |> mapInlineCodeBlock linePtr charPtr

            match result with
            | Ok (mapped, line, c, tail) ->
                document <- tail
                linePtr <- line
                charPtr <- c
                current <- $"%s{current}%s{mapped.Value}"
            | Error errorMessage ->
                endResult <- Error errorMessage
                finished <- true
        | c::tail, None ->
            document <- tail
            start <- charPtr |> getCoordinate linePtr |> Some
            charPtr <- charPtr + 1
            current <- $"%s{current}%c{c}"
        | c::tail, _ ->
            document <- tail
            charPtr <- charPtr + 1
            current <- $"%s{current}%c{c}"

    endResult
    

let map (document: char seq) =
    let rec map (linePtr: int) (charPtr: int) (acc: DocumentMap list) (document: char list) =
        match document with
        | [] ->
            acc
            |> List.sortBy (_.Coordinate)
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
            | Ok (lisps, line, c, tail) ->
                tail
                |> map line c ([lisps; acc] |> List.concat)
            | Error errorMessage -> Error errorMessage
        | _ ->
            let result =
                document
                |> mapText linePtr charPtr
                
            match result with
            | Ok (mapped, line, c, tail) ->
                tail
                |> map line c (mapped::acc)
            | Error errorMessage -> Error errorMessage
        
    document
    |> List.ofSeq
    |> map 0 0 []
