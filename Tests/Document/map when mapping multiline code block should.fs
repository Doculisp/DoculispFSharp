module Doculisp.Tests.Document.``map when mapping multiline code block should``

open Archer
open Archer.Arrows
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Multiline Code Block"
        Category "Code Block"
    ]
)

let ``map a multiline code block`` =
    feature.Test (fun _ ->
        let text = "```C#
var x = 2 + 2;
```"
        text
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = text; Coordinate = { Line = 0; Char = 0 } }
        ]
    )

let ``map a multiline code block containing Doculisp code`` =
    feature.Test (fun _ ->
        let text = "```markdown
<!--
    (dl (content))
-->
```"
        text
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = text; Coordinate = { Line = 0; Char = 0 } }
        ]
    )

let ``map a multiline code block with text before and after`` =
    feature.Test (fun _ ->
        let text = "An example of F#

```F#
let count = [ 1..100 ]
```

This creates a list with numbers."
        text
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = text; Coordinate = { Line = 0; Char = 0 } }
        ]
    )

let ``map a multiline code block containing a multiline code block`` =
    feature.Test (fun _ ->
        let text = @"```markdown
\`\`\`
// Some Code here
\`\`\`
```"
        text
        |> Document.map
        |> Should.BeOk [
            TextMap { Value = text; Coordinate = { Line = 0; Char = 0 } }
        ]
    )

let ``Error if code block is not closed`` =
    feature.Test (fun _ ->
        let text = "An example of F#

```F#
let count = [ 1..100 ]

This creates a list with numbers."
        text
        |> Document.map
        |> Should.BeError "Multiline code block at (2, 0) is not closed."
    )

let ``Test Cases`` = feature.GetTests ()
