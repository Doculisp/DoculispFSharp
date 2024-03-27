module Doculisp.Lib.DocumentTypes

type DocumentMap =
    | TextMap of string * Coordinate
    | LispMap of string * Coordinate
    member this.Coordinate with get () =
        match this with
        | TextMap (_, coordinate)
        | LispMap (_, coordinate) -> coordinate
        
    member this.Value with get () =
        match this with
        | TextMap (value, _)
        | LispMap (value, _) -> value