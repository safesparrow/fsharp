module internal FSharp.Compiler.InfoReader

type TcGlobals = int
type ImportMap = int
type MethInfo = int
type PropInfo = int
type range = int

type InfoReader =
    new: g: TcGlobals * amap: ImportMap -> InfoReader
    static member ExcludeHiddenOfMethInfos:
        g: TcGlobals -> amap: ImportMap -> m: range -> minfos: MethInfo list list -> MethInfo list
    static member ExcludeHiddenOfPropInfos:
        g: TcGlobals -> amap: ImportMap -> m: range -> pinfos: PropInfo list list -> PropInfo list

val ExcludeHiddenOfMethInfos: g: TcGlobals -> amap: ImportMap -> m: range -> minfos: MethInfo list list -> MethInfo list
val ExcludeHiddenOfPropInfos: g: TcGlobals -> amap: ImportMap -> m: range -> pinfos: PropInfo list list -> PropInfo list
