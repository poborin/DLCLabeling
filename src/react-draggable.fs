namespace Feliz.ReactDraggable

open Feliz
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Fable.React

type IDraggableProperty = interface end

module Interop =
    let inline mkDraggableAttr (key: string) (value: obj) : IDraggableProperty = unbox (key, value)

[<StringEnum>] 
[<RequireQualifiedAccess>]
type  DraggablePropsAxis =
    | Both
    | X
    | Y
    | None

[<Erase>]
type Offset =
    | Absolute of float
    | Relative of string

[<AllowNullLiteral>]
type IDraggableData =
    abstract node: HTMLElement with get, set
    abstract x: float with get, set
    abstract y: float with get, set
    abstract deltaX: float with get, set
    abstract deltaY: float with get, set
    abstract lastX: float with get, set
    abstract lastY: float with get, set

[<AllowNullLiteral>]
type IDraggableEventHandler =
    abstract event: MouseEvent with get, set
    abstract data: IDraggableData with get, set

    // [<Emit "$0($1...)">] 
    // abstract Invoke: e: MouseEvent * data: IDraggableData -> unit

[<Erase>]
type draggable =
    /// If set to `true`, will allow dragging on non left-button clicks.
    static member inline allowAnyClick (value: bool) =
        Interop.mkDraggableAttr "allowAnyClick" value

    /// Determines which axis the draggable can move. This only affects
    /// flushing to the DOM. Callbacks will still include all values.
    static member inline axis (value: DraggablePropsAxis) =
        Interop.mkDraggableAttr "axis" value

    /// Specifies movement boundaries. Accepted values:
    /// - `parent` restricts movement within the node's offsetParent
    ///    (nearest node with position relative or absolute), or
    /// - a selector, restricts movement within the targeted node
    /// - An object with `left, top, right, and bottom` properties.
    ///   These indicate how far in each direction the draggable
    ///   can be moved.
    static member inline bounds (?left: float, ?top: float, ?right: float, ?bottom: float) =
        let props = {|left = left; top = top; right = right; bottom = bottom|}
        Interop.mkDraggableAttr "bounds" props

    /// Specifies a selector to be used to prevent drag initialization. The string is passed to
    /// Element.matches, so it's possible to use multiple selectors like `.first, .second`.
    /// Example: '.body'
    static member inline cancel (element: string) =
        Interop.mkDraggableAttr "cancel" element

    /// Class names for draggable UI.
    /// Default to 'react-draggable'
    static member inline defaultClassName (name: string) =
        Interop.mkDraggableAttr "defaultClassName" name

    /// Class names for draggable UI.
    /// Default to 'react-draggable-dragging'
    static member inline defaultClassNameDragging (name: string) =
        Interop.mkDraggableAttr "defaultClassNameDragging" name

    /// Class names for draggable UI.
    /// Default to 'react-draggable-dragged'
    static member inline defaultClassNameDragged (name: string) =
        Interop.mkDraggableAttr "defaultClassNameDragged" name

    /// Specifies the `x` and `y` that the dragged item should start at.
    /// This is generally not necessary to use (you can use absolute or relative
    /// positioning of the child directly), but can be helpful for uniformity in
    /// your callbacks and with css transforms.
    static member inline defaultPosition (x: float, y: float) =
        Interop.mkDraggableAttr "defaultPosition" {|x = x; y = y|}

    /// Specifies the x and y that dragging should snap to.
    static member inline grid (x: float, y: float) = 
        Interop.mkDraggableAttr "grid" (ResizeArray [| x; y |])

    /// Specifies a selector to be used as the handle that initiates drag.
    /// Example: '.handle'
    static member inline handle (handle: string) =
        Interop.mkDraggableAttr "handle" handle

    /// If desired, you can provide your own offsetParent for drag calculations.
    /// By default, we use the Draggable's offsetParent. This can be useful for elements
    /// with odd display types or floats.
    static member inline offsetParent (parent: HTMLElement) =
        Interop.mkDraggableAttr "offsetParent" parent

    /// Called whenever the user mouses down. Called regardless of handle or
    /// disabled status
    static member inline onMouseDown (event: MouseEvent -> unit) =
        Interop.mkDraggableAttr "onMouseDown" (fun e -> event(e))

    /// Called when dragging starts. If `false` is returned any handler,
    /// the action will cancel.
    [<Emit("[\"onStart\", $0]")>]
    static member inline onStart (handler: MouseEvent -> IDraggableData -> unit) =
        Interop.mkDraggableAttr "onStart" handler

    /// Called while dragging.
    [<Emit("[\"onDrag\", $0]")>]
    static member inline onDrag (handler: MouseEvent -> IDraggableData -> unit) =
        Interop.mkDraggableAttr "onDrag" handler

    /// Called when dragging stops.
    [<Emit("[\"onStop\", $0]")>]
    static member inline onStop (handler: MouseEvent -> IDraggableData -> unit) =
        Interop.mkDraggableAttr "onStop" handler

    /// If running in React Strict mode, ReactDOM.findDOMNode() is deprecated.
    /// Unfortunately, in order for <Draggable> to work properly, we need raw access
    /// to the underlying DOM node. If you want to avoid the warning, pass a `nodeRef`
    /// as in this example:
    /// 
    /// ```js
    /// function MyComponent() {
    ///   const nodeRef = React.useRef(null);
    ///   return (
    ///     <Draggable nodeRef={nodeRef}>
    ///       <div ref={nodeRef}>Example Target</div>
    ///     </Draggable>
    ///   );
    /// }
    /// ```
    ///
    /// This can be used for arbitrarily nested components, so long as the ref ends up
    /// pointing to the actual child DOM node and not a custom component.
    ///
    /// For rich components, you need to both forward the ref *and props* to the underlying DOM
    /// element. Props must be forwarded so that DOM event handlers can be attached. 
    /// For example:
    ///
    /// ```js
    ///   const Component1 = React.forwardRef(function (props, ref) {
    ///     return <div {...props} ref={ref}>Nested component</div>;
    ///   });
    ///
    ///   const nodeRef = React.useRef(null);
    ///   <DraggableCore onDrag={onDrag} nodeRef={nodeRef}>
    ///     <Component1 ref={nodeRef} />
    ///   </DraggableCore>
    /// ```
    /// 
    /// Thanks to react-transition-group for the inspiration.
    ///
    /// `nodeRef` is also available on <DraggableCore>.
    static member inline nodeRef (ref: IRefValue<HTMLElement>) =
        Interop.mkDraggableAttr "nodeRef" ref

    /// Much like React form elements, if this property is present, the item
    /// becomes 'controlled' and is not responsive to user input. Use `position`
    /// if you need to have direct control of the element.
    static member inline position (x: float, y: float) = 
        Interop.mkDraggableAttr "position" {|x = x; y = y|}

    /// A position offset to start with. Useful for giving an initial position
    /// to the element. Differs from `defaultPosition` in that it does not
    /// affect the position returned in draggable callbacks, and in that it
    /// accepts strings, like `{x: '10%', y: '10%'}`.
    static member inline positionOffset (x: Offset, y: Offset) =
        Interop.mkDraggableAttr "positionOffset" {|x = x; y = y|}

    /// Specifies the scale of the canvas your are dragging this element on. This allows
    /// you to, for example, get the correct drag deltas while you are zoomed in or out via
    /// a transform or matrix in the parent of this element.
    static member inline scale (scale: float) =
        Interop.mkDraggableAttr "scale" scale

    static member inline child (child: ReactElement) =
        Interop.mkDraggableAttr "children" child

[<Erase>]
type ReactDraggable =
    static member inline draggable (properties: #IDraggableProperty list) =
        Interop.reactApi.createElement(importDefault "react-draggable", createObj !!properties)