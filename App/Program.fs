open System
open System.IO
open Argu
open Doculisp.Lib

type Arguments =
    | [<Mandatory; AltCommandLine "-from"; Unique>] SourceFile of string
    | [<Mandatory; AltCommandLine "-to"; Unique>] TargetFile of string
    | [<AltCommandLine ("-t", "-T", "-tst", "-Tst", "-TST"); Unique>]Test
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | TargetFile s -> "The output markdown file path and name."
            | SourceFile s -> "The toplevel markdown file to compile."
            | Test -> "Runs the compiler without outputting the file. It will show errors if there are any."

let private getDirectoryForFile (path: string) =
    let info =
        path
        |> FileInfo

    info.Directory.FullName


[<STAThread; EntryPoint>]
let main (parameters: string array) =
    let parser = ArgumentParser.Create<Arguments> (programName = "Doculisp.App.exe")

    try
        let results = parser.Parse parameters

        if results.IsUsageRequested then
            printfn $"%s{parser.PrintUsage()}"
            0
        else
            let target = results.GetResult TargetFile
            let source = results.GetResult SourceFile
            let isTest = results.Contains Test

            let target =
                let info =
                    target
                    |> FileInfo

                info.FullName

            let source =
                let info =
                    source
                    |> FileInfo

                info.FullName

            let currentDir =
                System.Reflection.Assembly.GetEntryAssembly().Location
                |> getDirectoryForFile

            let sourceDirectory =
                source
                |> getDirectoryForFile

            try
                printf $"%s{source} --> %s{target}"
                if isTest then printf " (TEST)"
                printfn ""

                Directory.SetCurrentDirectory sourceDirectory

                let buildResult =
                    source
                    |> Compiler.compile isTest target

                match buildResult with
                | Error errorValue ->
                    eprintfn $"%s{errorValue}"
                    1
                | Ok _ ->
                    printfn $"\nSuccessfully built \"%s{target}\""
                    0
            finally
                Directory.SetCurrentDirectory currentDir
    with
    | :? ArguParseException as e ->
        eprintfn $"%s{e.Message}"
        1
    | e ->
        eprintfn $"%A{e}"
        1
