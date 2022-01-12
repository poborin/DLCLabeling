// ts2fable 0.8.0
module rec panzoom
open Fable.Core
open Browser.Types

type Function = System.Action

let [<Import("*","panzoom")>] panzoom: PanZoom.IExports = jsNative

module PanZoom =

    [<AllowNullLiteral>]
    type IExports =
        [<Emit("$0($1...)")>]
        abstract createPanZoom: domElement: HTMLElement * ?options: PanZoomOptions -> PanZoom

    type [<AllowNullLiteral>] Bounds =
        abstract left: float with get, set
        abstract top: float with get, set
        abstract right: float with get, set
        abstract bottom: float with get, set

    type [<AllowNullLiteral>] TransformOrigin =
        abstract x: float with get, set
        abstract y: float with get, set

    type [<AllowNullLiteral>] Transform =
        abstract x: float with get, set
        abstract y: float with get, set
        abstract scale: float with get, set

    type [<AllowNullLiteral>] PanZoomController =
        abstract getOwner: (unit -> Element) with get, set
        abstract applyTransform: (Transform -> unit) with get, set

    [<Erase>]
    type BoundsOption =
        | Boolean of bool
        | Bounds of Bounds

    type [<AllowNullLiteral>] PanZoomOptions =
        abstract filterKey: (unit -> bool) option with get, set
        abstract bounds: BoundsOption option with get, set
        abstract maxZoom: float option with get, set
        abstract minZoom: float option with get, set
        abstract boundsPadding: float option with get, set
        abstract zoomDoubleClickSpeed: float option with get, set
        abstract zoomSpeed: float option with get, set
        abstract initialX: float option with get, set
        abstract initialY: float option with get, set
        abstract initialZoom: float option with get, set
        abstract pinchSpeed: float option with get, set
        abstract beforeWheel: (WheelEvent -> unit) option with get, set
        abstract beforeMouseDown: (MouseEvent -> unit) option with get, set
        abstract autocenter: bool option with get, set
        abstract onTouch: (TouchEvent -> unit) option with get, set
        abstract onDoubleClick: (Event -> unit) option with get, set
        abstract smoothScroll: bool option with get, set
        abstract controller: PanZoomController option with get, set
        abstract enableTextSelection: bool option with get, set
        abstract disableKeyboardInteraction: bool option with get, set
        abstract transformOrigin: TransformOrigin option with get, set

    type [<AllowNullLiteral>] PanZoom =
        abstract dispose: (unit -> unit) with get, set
        abstract moveBy: (float -> float -> bool -> unit) with get, set
        abstract moveTo: (float -> float -> unit) with get, set
        abstract smoothMoveTo: (float -> float -> unit) with get, set
        abstract centerOn: (obj option -> unit) with get, set
        abstract zoomTo: (float -> float -> float -> unit) with get, set
        abstract zoomAbs: (float -> float -> float -> unit) with get, set
        abstract smoothZoom: (float -> float -> float -> unit) with get, set
        abstract smoothZoomAbs: (float -> float -> float -> unit) with get, set
        abstract getTransform: (unit -> Transform) with get, set
        abstract showRectangle: (ClientRect -> unit) with get, set
        abstract pause: (unit -> unit) with get, set
        abstract resume: (unit -> unit) with get, set
        abstract isPaused: (unit -> bool) with get, set
        abstract on: (string -> (PanZoom -> unit) -> unit) with get, set
        abstract off: (string -> Function -> unit) with get, set
        abstract fire: (string -> unit) with get, set
        abstract getMinZoom: (unit -> float) with get, set
        abstract setMinZoom: (float -> float) with get, set
        abstract getMaxZoom: (unit -> float) with get, set
        abstract setMaxZoom: (float -> float) with get, set
        abstract getTransformOrigin: (unit -> TransformOrigin) with get, set
        abstract setTransformOrigin: (TransformOrigin -> unit) with get, set
        abstract getZoomSpeed: (unit -> float) with get, set
        abstract setZoomSpeed: (float -> unit) with get, set