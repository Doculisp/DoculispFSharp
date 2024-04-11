﻿module Doculisp.Lib.IoHelpers

open System.IO
open System.Text

let loadFile (path: string) =
    try
        path
        |> File.ReadAllText
        |> Ok
    with
    | e -> Error $"%s{path}\n\n%A{e}"


let getWriter (testRun: bool, path: string) =
    if testRun then
        let sb = StringBuilder ()
        (new StringWriter (sb)) :> TextWriter
    else
        (new StreamWriter (path, false)) :> TextWriter
