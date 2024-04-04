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
    
and External =
    {
        Path: string
        Label: string
        Content: LoadState<Tree>
        Index: int
    }
    member this.Title with get () =
        match this.Content with
        | Waiting
        | Loaded Empty -> ""
        | Loaded (Content content) ->
            content.Title
        
    member this.Link with get () =
        match this.Content with
        | Waiting
        | Loaded Empty -> ""
        | Loaded (Content content) ->
            content.Link

    member this.IsLoaded with get () =
        match this.Content with
        | Waiting -> false
        | Loaded _ -> true

    member this.HasContent with get () =
        match this.Content with
        | Waiting
        | Loaded Empty -> false
        | _ -> true
        
    member this.TableInfo with get () =
        {
            Label = this.Label
            Title = this.Title
            Link = this.Link 
        }
    
and Part =
    | Markdown of Value
    | Heading of Heading
    | ContentPlaceHolder of Coordinate
    member this.Coordinate with get () =
        match this with
        | Markdown value -> value.Coordinate
        | Heading heading -> heading.Coordinate
        | ContentPlaceHolder coordinate -> coordinate

and Content =
    {
        Title: string
        Subtitle: string option
        Link: string
        Table: TableOfContentsDefinition
        Coordinate: Coordinate
        Parts: Part list
        Externals: External list
    }
    member this.TableInfo with get () =
        this.Externals
        |> List.map _.TableInfo

and Tree =
    | Empty
    | Content of Content
