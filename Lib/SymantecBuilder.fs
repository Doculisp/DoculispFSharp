module Doculisp.Lib.SymantecBuilder

open Doculisp.Lib
open Doculisp.Lib.DocumentTypes
open Doculisp.Lib.SymantecTypes

let private buildTree _ = [] |> Ok

let build (document: Result<DocumentMap list, string>) =
    combine buildTree document
