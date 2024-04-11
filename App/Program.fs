open System
open System.IO
open System.Text
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

let getWriter (testRun: bool, path: string) =
    if testRun then
        let sb = StringBuilder ()
        (new StringWriter (sb)) :> TextWriter
    else
        (new StreamWriter (path, false)) :> TextWriter

let getFile (path: string) =
    try
        path
        |> File.ReadAllText
        |> fun s -> s.ToCharArray ()
        |> Array.toSeq
        |> Ok
    with
    | e ->
        $"%A{e}"
        |> Error


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


            use writer = getWriter (isTest, target)

            let currentDir =
                let info =
                    System.Reflection.Assembly.GetEntryAssembly().Location
                    |> FileInfo

                info.Directory.FullName

            let sourceDirectory =
                let info =
                    source
                    |> FileInfo

                info.Directory.FullName

            try
                Directory.SetCurrentDirectory sourceDirectory

                writer.WriteLine "<!-- Generated Document do not edit! -->\n"

                let buildResult =
                    source
                    |> getFile
                    |> Document.map source
                    |> Tokenizer.parse source
                    |> SymantecBuilder.build source
                    |> Externals.load
                    |> SymantecTree.processBuildResult writer

                writer.WriteLine "<!-- Generated Document do not edit! -->"
                writer.Flush ()

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
