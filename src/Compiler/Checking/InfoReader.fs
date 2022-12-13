module internal FSharp.Compiler.InfoReader

type TcGlobals = int
type ImportMap = int
type MethInfo = int
type PropInfo = int
type range = int

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
