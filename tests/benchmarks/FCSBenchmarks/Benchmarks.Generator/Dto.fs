module BenchmarkGenerator.Dto

open System

[<CLIMutable>]
type RangeDto =
    {
        Code1 : int64
        Code2 : int64
    }

[<CLIMutable>]
type FSharpReferenceDto =
    {
        OutputFile: string
        Options : FSharpProjectOptionsDto
    }

and [<CLIMutable>] FSharpProjectOptionsDto =
    {
        ProjectFileName : string
        ProjectId : string option
        SourceFiles : string[]
        OtherOptions: string[]
        ReferencedProjects: FSharpReferenceDto[]
        IsIncompleteTypeCheckEnvironment : bool
        UseScriptResolutionRules : bool
        LoadTime : DateTime
        Stamp: int64 option
    }

[<CLIMutable>]
type AnalyseFileDto =
    {
        FileName: string
        FileVersion: int
        SourceText: string
        Options: FSharpProjectOptionsDto
    }

type BenchmarkActionDto =
    | AnalyseFile of AnalyseFileDto

[<CLIMutable>]    
type BenchmarkConfig =
    {
        ProjectCacheSize : int
    }
    with static member makeDefault () = {ProjectCacheSize = 200}
    
[<CLIMutable>]
type BenchmarkInputsDto =
    {
        Actions : BenchmarkActionDto list
        Config : BenchmarkConfig
    }
    