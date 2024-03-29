﻿module Doculisp.Lib.Tokenizer

open Doculisp.Lib
open Doculisp.Lib.DocumentTypes
open Doculisp.Lib.TextHelpers
open Doculisp.Lib.TokenTypes

let private parseLisp (value: Value) =
    let rec parseAtom (current: string) (charPtr: int) (document: char list) =
        match document with
        | IsEscaped '(' (esc, tail)
        | IsEscaped ')' (esc, tail) ->
            let escCleaned = esc.Replace (@"\", "")
            tail
            |> parseAtom $"%s{current}%s{escCleaned}" (charPtr + esc.Length)
        | IsWhiteSpace _
        | '('::_
        | ')'::_ ->
            if 0 < current.Length then
                (Some current), charPtr, document
            else
                None, charPtr, document
        | c::tail ->
            tail
            |> parseAtom $"%s{current}%c{c}" (charPtr + 1)

    let rec parseParameter (current: string) (charPtr: int) (document: char list) =
        match document with
        | IsEscaped '(' (esc, tail)
        | IsEscaped ')' (esc, tail) ->
            let escCleaned = esc.Replace(@"\", "")
            tail
            |> parseParameter $"%s{current}%s{escCleaned}" (charPtr + esc.Length)
        | IsNewLine _ ->
            if 0 < current.Length then
                (Some current), charPtr, document
            else
                None, charPtr, document
        | '('::_
        | ')'::_ ->
            if 0 < current.Length then
                (Some current), charPtr, document
            else
                None, charPtr, document
        | c::tail ->
            tail
            |> parseParameter $"%s{current}%c{c}" (charPtr + 1)

    let rec parse (linePtr: int) (charPtr: int) (acc: LispToken list) (document: char list) =
        match document with
        | [] -> Ok (acc |> List.rev)
        | IsNewLine (_, tail) ->
            tail
            |> parse (linePtr + 1) 0 acc
        | IsWhiteSpace (sp, tail) ->
            tail
            |> parse linePtr (charPtr + sp.Length) acc
        | '('::tail ->
            let result, c, rest =
                tail
                |> parseAtom "" (charPtr + 1)

            match result with
            | None ->
                Error $"Open parentheses without atom at (%d{linePtr}, %d{charPtr})."
            | Some value ->
                rest
                |> parse linePtr c ((Open { Value = value; Coordinate = { Line = linePtr; Char = charPtr } })::acc)
        | ')'::tail ->
            tail
            |> parse linePtr (charPtr + 1) ((Close { Line = linePtr; Char = charPtr })::acc)
        | _ ->
            let result, c, tail =
                document
                |> parseParameter "" charPtr

            match result with
            | None ->
                tail
                |> parse linePtr c acc
            | Some value ->
                tail
                |> parse linePtr c ((Parameter { Value = value; Coordinate = { Line = linePtr; Char = charPtr } })::acc)

    value.Value
    |> List.ofSeq
    |> parse value.Coordinate.Line value.Coordinate.Char []

let private parseMaps (document: DocumentMap list) =
    let rec parse (acc: Token list) (document: DocumentMap list) =
        match document with
        | [] -> acc |> List.rev |> Ok
        | (TextMap value)::tail ->
            tail
            |> parse ((Text value)::acc)
        | (LispMap value)::tail ->
            let result =
                value
                |> parseLisp

            match result with
            | Ok lisps ->
                tail
                |> parse ((Lisp lisps)::acc)
            | Error errorValue -> Error errorValue

    document
    |> parse []

let parse (document: Result<DocumentMap list, string>) =
    combine parseMaps document
