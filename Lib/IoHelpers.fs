module Doculisp.Lib.IoHelpers

let loadFile (path: string) =
    try
        path
        |> System.IO.File.ReadAllText
        |> Ok
    with
    | e -> Error $"%A{e}"
