/// This file is shared between Benchmarks.Generator that serializes inputs using these DTO types,
/// and Benchmarks.Runner that deserializes them using these DTO types.
/// It's shared via link rather than a library due to problems in referencing a separate project in Benchmarks.Generator
module Benchmarks.Common.Dtos

open System

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
    