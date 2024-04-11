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
        |> stringToMaybeCharSeq
        |> Document.map "./docs/math.md"
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
        |> stringToMaybeCharSeq
        |> Document.map "./docs/mydoc.md"
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
        |> stringToMaybeCharSeq
        |> Document.map "./docs/readme.md"
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
        |> stringToMaybeCharSeq
        |> Document.map "./docs/_main.md"
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
        |> stringToMaybeCharSeq
        |> Document.map "./docs/bob.md"
        |> formatMap
        |> Should.MeetStandard reporters env.TestInfo
    )

let ``Test Cases`` = feature.GetTests ()
