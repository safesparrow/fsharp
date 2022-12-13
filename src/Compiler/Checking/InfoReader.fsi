module internal FSharp.Compiler.InfoReader

open FSharp.Compiler.Import
open FSharp.Compiler.Infos
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text

/// An InfoReader is an object to help us read and cache infos.
/// We create one of these for each file we typecheck.
type InfoReader =

    /// Get the declared IL fields of a type, not including inherited fields
    new: g: TcGlobals * amap: ImportMap -> InfoReader

    /// Exclude methods from super types which have the same signature as a method in a more specific type.
    static member ExcludeHiddenOfMethInfos:
        g: TcGlobals -> amap: ImportMap -> m: range -> minfos: MethInfo list list -> MethInfo list

    /// Exclude properties from super types which have the same name as a property in a more specific type.
    static member ExcludeHiddenOfPropInfos:
        g: TcGlobals -> amap: ImportMap -> m: range -> pinfos: PropInfo list list -> PropInfo list

/// Exclude methods from super types which have the same signature as a method in a more specific type.
val ExcludeHiddenOfMethInfos: g: TcGlobals -> amap: ImportMap -> m: range -> minfos: MethInfo list list -> MethInfo list

/// Exclude properties from super types which have the same name as a property in a more specific type.
val ExcludeHiddenOfPropInfos: g: TcGlobals -> amap: ImportMap -> m: range -> pinfos: PropInfo list list -> PropInfo list
