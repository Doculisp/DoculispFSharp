module Doculisp.Tests.Document.``map when mapping text should``

open Xunit
open Doculisp.Tests
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes

[<Fact>]
let ``map an empty document`` () =
    ""
    |> Document.map
    |> shouldBeEqualTo []
    
[<Fact>]
let ``map "Hello" as text`` () =
    "Hello"
    |> Document.map
    |> shouldBeEqualTo [
        TextMap ("Hello", { Line = 0; Char = 0 })
    ]
    
[<Fact>]
let ``map "Good bye" as text`` () =
    "Good Bye"
    |> Document.map
    |> shouldBeEqualTo [
        TextMap ("Good Bye", { Line = 0; Char = 0 })
    ]
    
[<Fact>]
let ``map text followed by spaces`` () =
    "Good Bye   "
    |> Document.map
    |> shouldBeEqualTo [
        TextMap ("Good Bye", { Line = 0; Char = 0 })
    ]
    
[<Fact>]
let ``map text surrounded by spaces`` () =
    "   Doculisp   "
    |> Document.map
    |> shouldBeEqualTo [
        TextMap ("Doculisp", { Line = 0; Char = 3 })
    ]
    
[<Fact>]
let ``map text surrounded by spaces and preceded by new lines`` () =
    "\r\n\r\n\r\n\r\n   After Lines   "
    |> Document.map
    |> shouldBeEqualTo [
        TextMap ("After Lines", { Line = 4; Char = 3 })
    ]
    
[<Fact>]
let ``map text that includes new lines`` () =
    "# A document\r\n\r\nAbout something"
    |> Document.map
    |> shouldBeEqualTo [
        TextMap ("# A document\r\n\r\nAbout something", { Line = 0; Char = 0 })
    ]