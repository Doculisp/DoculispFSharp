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
    let rec map (linePtr: int) (charPtr: int) (depth: int) (start: Coordinate option) (current: string) (document: char list) =
        match document, start with
        | [], Some st ->
            Error $"Doculisp block at %s{st.ToString ()} is not closed."
        | IsStartLisp (lisp, tail), None ->
            tail
            |> map linePtr (charPtr + lisp.Length) (depth + 1) (Some { Line = linePtr; Char = charPtr }) $"%s{current}%s{lisp}"
        | IsEscaped '(' (esc, tail), _ ->
            tail
            |> map linePtr (charPtr + esc.Length) depth start $"%s{current}%s{esc}"
        | IsEscaped ')' (esc, tail), _ ->
            tail
            |> map linePtr (charPtr + esc.Length) depth start $"%s{current}%s{esc}"
        | '('::tail, _ ->
            tail
            |> map linePtr (charPtr + 1) (depth + 1) start $"%s{current}("
        | ')'::tail, _ when 1 < depth ->
            tail
            |> map linePtr (charPtr + 1) (depth - 1) start $"%s{current})"
        | ')'::tail, Some st ->
            Ok (LispMap ($"%s{current})", st), linePtr, charPtr, tail)
        | IsNewLine (ln, tail), _ ->
            tail
            |> map (linePtr + 1) 0 depth start $"%s{current}%s{ln}"
        | c::tail, _ ->
            tail
            |> map linePtr (charPtr + 1) depth start $"%s{current}%c{c}"

    document
    |> map linePtr charPtr 0 None ""

let private mapMultilineCodeBlock (linePtr: int) (charPtr: int) (document: char list) =
    let rec map (linePtr: int) (charPtr: int) (start: Coordinate option) (current: string) (document: char list) =
        match document, start with
        | [], Some st ->
            Error $"Multiline code block at %s{st.ToString ()} is not closed."
        | IsMultiline (ln, tail), None ->
            tail
            |> map linePtr (charPtr + ln.Length) (Some { Line = linePtr; Char = charPtr }) $"%s{current}%s{ln}"
        | IsMultiline (ln, tail), Some st ->
            Ok (TextMap ($"%s{current}%s{ln}", st), linePtr, charPtr + ln.Length, tail)
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
            |> map linePtr (charPtr + inl.Length) (Some { Line = linePtr; Char = charPtr }) $"%s{current}%s{inl}"
        | IsInline (inl, tail), Some st ->
            Ok (TextMap ($"%s{current}%s{inl}", st), linePtr, charPtr, tail)
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
            |> map linePtr (charPtr + op.Length) acc (Some { Line = linePtr; Char = charPtr })
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

let private mapText (linePtr: int) (charPtr: int) (document: char list) =
    let rec map (linePtr: int) (charPtr: int) (start: Coordinate option) (current: string) (document: char list) =
        match document, start with
        | [], Some st ->
            Ok (TextMap (current.Trim(), st), linePtr, charPtr, document)
        | IsNewLine (ln, tail), _ ->
            tail
            |> map (linePtr + 1) 0 start $"%s{current}%s{ln}"
        | IsOpenComment _, Some st ->
            Ok (TextMap (current.Trim (), st), linePtr, charPtr, document)
        | IsEscaped '`' (esc, tail), None ->
            tail
            |> map linePtr (charPtr + esc.Length) (Some { Line = linePtr; Char = charPtr }) $"%s{current}%s{esc}"
        | IsEscaped '`' (esc, tail), _ ->
            tail
            |> map linePtr (charPtr + esc.Length) start $"%s{current}%s{esc}"
        | IsMultiline _, None ->
            let result =
                document
                |> mapMultilineCodeBlock linePtr charPtr

            match result with
            | Ok (mapped, ln, c, tail) ->
                tail
                |> map ln c (Some { Line = linePtr; Char = charPtr }) $"%s{current}%s{mapped.Value}"
            | Error errorMessage -> Error errorMessage
        | IsMultiline _, _ ->
            let result =
                document
                |> mapMultilineCodeBlock linePtr charPtr

            match result with
            | Ok (mapped, ln, c, tail) ->
                tail
                |> map ln c start $"%s{current}%s{mapped.Value}"
            | Error errorMessage -> Error errorMessage
        | IsInline _, None _ ->
            let result =
                document
                |> mapInlineCodeBlock linePtr charPtr
                
            match result with
            | Ok (mapped, line, c, tail) ->
                tail
                |> map line c (Some { Line = linePtr; Char = charPtr }) $"%s{current}%s{mapped.Value}"
            | Error errorMessage -> Error errorMessage
        | IsInline _, Some _ ->
            let result =
                document
                |> mapInlineCodeBlock linePtr charPtr
                
            match result with
            | Ok (mapped, line, c, tail) ->
                tail
                |> map line c start $"%s{current}%s{mapped.Value}"
            | Error errorMessage -> Error errorMessage
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
