module Doculisp.Lib.Compiler

open Doculisp.Lib.IoHelpers

let compile (isTest: bool) (toFile: string) (fromFile: string) =
    try
        use writer = getWriter (isTest, toFile)
        writer.WriteLine "<!-- Generated Document do not edit! -->\n"

        let buildResult =
            fromFile
            |> Externals.processFile
            |> SymantecTree.processBuildResult writer

        match buildResult with
        | Error errorValue -> Error errorValue
        | Ok _ ->
            writer.WriteLine "<!-- Generated Document do not edit! -->"
            writer.Flush ()
            Ok ()
    with
    | e -> Error $"%A{e}"
