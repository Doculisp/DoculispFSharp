[<AutoOpen>]
module Doculisp.Lib.Types

type Coordinate =
    {
        Line: int
        Char: int
    }
    override this.ToString () =
        $"(%d{this.Line}, %d{this.Char})"

let getCoordinate lineIncrease charIncrease =
    {
        Line = lineIncrease + 1
        Char = charIncrease + 1
    }

type Value =
    {
        Value: string
        Coordinate: Coordinate
    }

let combine<'a, 'b, 'c> (fn: 'a -> Result<'c, 'b>) (value: Result<'a, 'b>): Result<'c, 'b> =
    match value with
    | Error er -> Error er
    | Ok value -> fn value
