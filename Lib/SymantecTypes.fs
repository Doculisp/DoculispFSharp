module Doculisp.Lib.SymantecTypes

type ExternalDefinition =
    {
        LineNumber: int
        Path: string
        Sections: SectionDefinition list
    }

and SectionDefinition =
    {
        Title: string
        Subtitle: string
        Link: string
    }
