module Doculisp.Lib.TokenTypes

type LispToken =
    | Open of Value
    | Parameter of Value
    | Close of Coordinate
    member this.Coordinate with get () =
        let tmp = this
        match tmp with
        | Open { Value = _; Coordinate = coordinate }
        | Parameter { Value = _; Coordinate = coordinate }
        | Close coordinate -> coordinate

type Token =
    | Text of Value
    | Lisp of LispToken list
