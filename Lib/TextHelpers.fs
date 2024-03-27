module Doculisp.Lib.TextHelpers

let (|IsNewLine|_|) = function
    | '\r'::'\n'::tail ->
        Some ("\r\n", tail)
    | '\r'::tail ->
        Some ("\r", tail)
    | '\n'::tail ->
        Some ("\n", tail)
    | _ -> None