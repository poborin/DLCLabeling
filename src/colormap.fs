// ts2fable 0.8.0
module rec ColorMap

open Fable.Core

[<Erase>]
type Alpha =
    | Alpha of float
    | Range of float array

[<StringEnum>]
type Format =
    | Hex
    | RgbaString 
    | Rgba
    | Float

type [<AllowNullLiteral>] IColormapSpec =
    abstract colormap: string option with get, set
    abstract format: Format option with get, set
    abstract nshades: int option with get, set
    // abstract alpha: Alpha option with get, set

// [<Import("colormap", from="colormap")>]
[<Import("default", from="colormap")>]
let colormap(spec: IColormapSpec): string array = jsNative