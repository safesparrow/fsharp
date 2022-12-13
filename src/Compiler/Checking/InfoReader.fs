module internal FSharp.Compiler.InfoReader

open FSharp.Compiler.Import
open FSharp.Compiler.Infos
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text

/// An InfoReader is an object to help us read and cache infos. 
/// We create one of these for each file we typecheck. 
type InfoReader(_g: TcGlobals, _amap: ImportMap) as this =

    static member ExcludeHiddenOfMethInfos
        (_g: TcGlobals)
        (_amap: ImportMap)
        (_m: range)
        (_pinfos: MethInfo list list)
        : MethInfo list
        =
        failwith ""

    static member ExcludeHiddenOfPropInfos
        (_g: TcGlobals)
        (_amap: ImportMap)
        (_m: range)
        (_pinfos: PropInfo list list)
        : PropInfo list
        = 
        failwith ""

let ExcludeHiddenOfMethInfos
    (g: TcGlobals)
    (amap: ImportMap)
    (m: range)
    (minfos: MethInfo list list)
    : MethInfo list
    = 
    InfoReader.ExcludeHiddenOfMethInfos g amap m minfos

let ExcludeHiddenOfPropInfos
    (g: TcGlobals)
    (amap: ImportMap)
    (m: range)
    (pinfos: PropInfo list list)
    : PropInfo list
    = 
    InfoReader.ExcludeHiddenOfPropInfos g amap m pinfos
