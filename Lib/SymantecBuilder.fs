module Doculisp.Lib.SymantecBuilder

open Doculisp.Lib
open Doculisp.Lib.TokenTypes
open Doculisp.Lib.SymantecTypes

let private buildTree _ = Empty |> Ok

let build (document: Result<Token list, string>) =
    combine buildTree document
