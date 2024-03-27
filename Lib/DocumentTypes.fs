module Doculisp.Lib.DocumentTypes

type DocumentMap =
    | TextMap of Value
    | LispMap of Value
    member this.Coordinate with get () =
        match this with
        | TextMap { Value = _; Coordinate = coordinate }
        | LispMap { Value = _; Coordinate = coordinate } -> coordinate
        
    member this.Value with get () =
        match this with
        | TextMap { Value = value; Coordinate = _ }
        | LispMap { Value = value; Coordinate = _ } -> value
