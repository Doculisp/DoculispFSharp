[<AutoOpen>]
module Doculisp.Lib.Types

type Coordinate =
    {
        Line: int
        Char: int
    }
    override this.ToString () =
        $"(%d{this.Line}, %d{this.Char})"

type Value =
    {
        Value: string
        Coordinate: Coordinate
    }
