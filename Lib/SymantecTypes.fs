module Doculisp.Lib.SymantecTypes

type TableOfContentsDefinition =
    | NoTable
    | Unlabeled
    | Labeled
    | Numbered
    | NumberedAndLabeled
    | Bulleted
    | BulletedAndLabeled
    
type Heading =
    {
        Depth: int
        Value: string
        Coordinate: Coordinate
    }
    
type TableOfContentsInfo =
    {
        Label: string
        Title: string
        Link: string
    }

type LoadState<'a> =
    | Waiting
    | Loaded of 'a
    
type External =
    {
        Path: string
        Label: string
        Content: LoadState<Content>
        Index: int
    }
    member this.Title with get () =
        match this.Content with
        | Waiting -> ""
        | Loaded content -> content.Title
        
    member this.Link with get () =
        match this.Content with
        | Waiting -> ""
        | Loaded content -> content.Link

    member this.IsLoaded with get () =
        match this.Content with
        | Waiting -> false
        | Loaded _ -> true
        
    member this.TableInfo with get () =
        {
            Label = this.Label
            Title = this.Title
            Link = this.Link 
        }
    
and Part =
    | Markdown of Value
    | Heading of Heading
    | External of External
    
and Content =
    {
        Title: string
        Subtitle: string option
        Link: string
        Table: TableOfContentsDefinition
        Coordinate: Coordinate
        Parts: Part list
    }
    member this.TableInfo with get () =
        this.Parts
        |> List.filter (fun part ->
            match part with
            | External _ -> true
            | _ -> false
        )
        |> List.map (fun (External e) -> e.TableInfo)

type Tree =
    | Empty
    | Content of Content
