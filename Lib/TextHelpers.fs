module Doculisp.Lib.TextHelpers

let (|IsNewLine|_|) = function
    | '\r'::'\n'::tail ->
        Some ("\r\n", tail)
    | '\r'::tail ->
        Some ("\r", tail)
    | '\n'::tail ->
        Some ("\n", tail)
    | _ -> None
    
let (|IsWhiteSpace|_|) = function
    | IsNewLine (ln, tail) ->
        Some (ln, tail)
    | c::tail when c |> System.Char.IsWhiteSpace ->
        Some ($"%c{c}", tail)
    | _ -> None
    

let (|IsEscaped|_|) value = function
    | '\\'::c::tail when c = value ->
        Some ($"\\%c{c}", tail)
    | _ -> None