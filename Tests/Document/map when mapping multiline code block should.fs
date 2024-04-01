module Doculisp.Tests.Document.``map when mapping multiline code block should``

open Archer
open Archer.Arrows
open Archer.ApprovalsSupport
open Doculisp.Lib
open Doculisp.Lib.DocumentTypes
open Doculisp.Tests

let private feature = Arrow.NewFeature (
    TestTags [
        Category "Document"
        Category "Multiline Code Block"
        Category "Code Block"
    ],
    setupApprovals
)

let ``map a multiline code block`` =
    feature.Test (fun reporters env ->
        let text = "```C#
var x = 2 + 2;
```"
        text
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map a multiline code block containing Doculisp code`` =
    feature.Test (fun reporters env ->
        let text = "```markdown
<!--
    (dl (content))
-->
```"
        text
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map a multiline code block with text before and after`` =
    feature.Test (fun reporters env ->
        let text = "An example of F#

```F#
let count = [ 1..100 ]
```

This creates a list with numbers."
        text
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``map a multiline code block containing a multiline code block`` =
    feature.Test (fun reporters env ->
        let text = @"```markdown
\`\`\`
// Some Code here
\`\`\`
```"
        text
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``Error if code block is not closed`` =
    feature.Test (fun reporters env ->
        let text = "An example of F#

```F#
let count = [ 1..100 ]

This creates a list with numbers."
        text
        |> Document.map
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``Test Cases`` = feature.GetTests ()
