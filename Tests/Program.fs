﻿open Archer
open Archer.Bow
open Archer.CoreTypes.InternalTypes
open Archer.CoreTypes.InternalTypes.RunnerTypes
open Archer.Logger.Summaries
open MicroLang.Lang
open Doculisp.Tests

let runner = bow.Runner ()

runner.RunnerLifecycleEvent
|> Event.add (fun args ->
    match args with
    | RunnerStartExecution _ ->
        printfn ""
    | RunnerTestLifeCycle (test, testEventLifecycle, _) ->
        match testEventLifecycle with
        | TestEndExecution testExecutionResult ->
            match testExecutionResult with
            | TestExecutionResult TestSuccess -> ()
            | result ->
                let transformedResult = defaultTestExecutionResultSummaryTransformer result test
                printfn $"%s{transformedResult}"
            
        | _ -> ()
    | RunnerEndExecution ->
        printfn "\n"
)

runner
|> addMany [
    Document.``map when mapping text should``.``Test Cases``
    Document.``map when mapping comments should``.``Test Cases``
    Document.``map when mapping inline code blocks should``.``Test Cases``
    Document.``map when mapping multiline code block should``.``Test Cases``
    Document.``map when mapping doculisp block should``.``Test Cases``
    ``Tokenizer parse should``.``Test Cases``
    ``SymantecBuilder build should``.``Test Cases``
]
|> runAndReport
