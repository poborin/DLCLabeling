module Labeling

open Feliz
open Feliz.UseElmish
open Feliz.Bulma
open Elmish
open ConfigDecoder
open Fable.Core.JsInterop
open System
open panzoom
open panzoom.PanZoom
open Data
open Utils
open Browser.Types
open Feliz.ReactDraggable

type ProjectFile = { FileName: string; ImageBlob: Browser.Types.Blob; DisplayUrl: string }
type LabelDrag = { Individual: string; Bodypart: string; X: float; Y: float }
type ImageTransformation = { X: float; Y: float; Scale: float }

type State = { 
    Config: MinimalConfig 
    ShowQuickView: bool
    LoadedImages: ProjectFile list
    LabeledData: LabeledData list
    SelectedImage: ProjectFile option
    ImageTransformation: ImageTransformation
    LabelDrag: LabelDrag
    PanZoom: PanZoom option
    ErrorMessage: string option }

type Props = {| Config: MinimalConfig |}

type Msg = 
    | AddPanZoom
    | ToggleQuickView
    | ImageLoaded of ProjectFile
    | CSVLoaded of string
    | SelectImage of ProjectFile
    | DisplayLabels of LabeledData list
    | OnImageTransform of ImageTransformation
    | OnLabelDrag of LabelDrag
    | OnLabelDragStart
    | OnLabelDragStop
    | CreatePanZoom of PanZoom
    | LogError of exn

let init (props: Props) = { 
        Config = props.Config; 
        ShowQuickView = false; 
        LoadedImages = List.empty;
        LabeledData = List.empty;
        SelectedImage = None;
        ImageTransformation = { X = 0.0; Y = 0.0; Scale = 1.0 }
        LabelDrag = { Individual = ""; Bodypart = ""; X = 0.; Y = 0.}
        PanZoom = None;
        ErrorMessage = None }, Cmd.none

let onPanZoom (pz: PanZoom) dispatch =
    pz.on "transform" (fun e -> 
        let t = e.getTransform()
        dispatch (OnImageTransform { X = t.x; Y = t.y; Scale = t.scale } )
    )

let selectLoadedFile state file =
    match state.SelectedImage with
    | Some file -> Cmd.none
    | None -> 
        let element = Browser.Dom.document.getElementById("canvasImage")
        let options: PanZoomOptions = !!{| maxZoom = Some 5.; minZoom = Some 1.; bounds = Some true; boundsPadding = Some 1.|}
        let pz = panzoom.createPanZoom(element, options)
        Cmd.batch [   
            Cmd.ofMsg (SelectImage file)
            Cmd.ofMsg (CreatePanZoom pz)
            Cmd.ofSub (onPanZoom pz)
        ]

let update props msg state =
    match msg with 
    | AddPanZoom -> state, Cmd.none
    | ToggleQuickView -> { state with ShowQuickView = not state.ShowQuickView }, Cmd.none
    | ImageLoaded file -> 
        { state with LoadedImages = state.LoadedImages |> List.append [file] |> List.sortBy (fun x -> x.FileName) }, selectLoadedFile state file
    | CSVLoaded content ->
        state, Cmd.OfAsync.either LabeledData.AsyncDecode content DisplayLabels LogError
    | SelectImage file -> 
        { state with SelectedImage = Some file }, Cmd.none
    | DisplayLabels labels ->
        { state with LabeledData = labels }, Cmd.none
    | OnImageTransform transform ->
        { state with ImageTransformation = transform}, Cmd.none
    | OnLabelDrag drag ->
        { state with LabelDrag = drag }, Cmd.none
    | OnLabelDragStart -> 
        match state.PanZoom with
        | Some pz -> 
            printfn "- pz pause"
            pz.pause()
        | None -> 
            printfn "no panzoom" |> ignore
        state, Cmd.none
    | OnLabelDragStop -> 
        match state.PanZoom with
        | Some pz -> 
            printfn "+ pz resume"
            pz.resume()
        | None -> 
            printfn "no panzoom" |> ignore
        state, Cmd.none
    | CreatePanZoom pz -> { state with PanZoom = Some pz }, Cmd.none
    | LogError e ->
        printfn "Error: %s" e.Message
        state, Cmd.none

let loadFile dispatch (fileName: string, blob: Browser.Types.Blob) =
    let reader = Browser.Dom.FileReader.Create()

    match fileName with
    | EndsWith ".png" _ ->
        reader.onload <- (fun ev ->
            let disaplayUrl = ev.target?result
            let file = { FileName = fileName; ImageBlob = blob; DisplayUrl = disaplayUrl }
            dispatch (ImageLoaded file)
        )
        reader.readAsDataURL(blob)
    | EndsWith ".csv" _ ->
        reader.onload <- (fun ev -> 
            let text = ev.target?result
            dispatch (CSVLoaded text)
        )
        reader.readAsText(blob)
    | _ -> ()

let loadProjectFiles dispatch (fileEvent: Browser.Types.Event) =    
    let isProjectFile (file: Browser.Types.File) =
        match file.name with
        | EndsWith ".png" _ -> true
        | EndsWith ".csv" _ -> true
        | EndsWith ".h5" _ -> true
        | _ -> false

    let fileNameBlob (x: Browser.Types.File) = x.name, x.slice()

    let fileList: Browser.Types.FileList = !!fileEvent.target?files
    [|0 .. fileList.length - 1|] 
    |> Array.rev
    |> Array.filter (fileList.item >> isProjectFile)
    |> Array.map (fileList.item >> fileNameBlob)
    |> Array.iter (loadFile dispatch)

let getFileDisplayUrl file =
    match file with
    | Some x -> x.DisplayUrl
    | None -> "https://bulma.io/images/placeholders/1280x960.png"

let getFileName file = 
    match file with
    | Some x -> x.FileName
    | None -> "placeholder"

let selectedBorder state file =
    match Option.contains file state.SelectedImage with
    | true ->
        prop.style [
            style.border(3, borderStyle.solid, "hsl(171, 100%, 41%)")
            style.borderRadius 2
        ]
    | false -> prop.style [style.border(3, borderStyle.solid, color.transparent)]

let miniViews state dispathc =
    state.LoadedImages |> List.toSeq |> Seq.map (fun x ->
        Bulma.column [
            Bulma.column.is2
            prop.children [
                Bulma.image [
                    selectedBorder state x
                    prop.children [
                        Html.img [
                            prop.src x.DisplayUrl
                            prop.onClick (fun _ -> x |> SelectImage |> dispathc)
                        ]
                    ]
                ]
            ]
        ]
    )

let getSvgCircle dispatch transform individual bodypart (coordinate: Coordinates) (radius: int) (config: MinimalConfig) (opacity: float) =
    let circle = Svg.circle [
        svg.id $"%s{individual}.%s{bodypart}"
        svg.cx coordinate.X
        svg.cy coordinate.Y
        svg.r radius
        svg.fill config.BodyColors.[bodypart]
        svg.fillOpacity opacity
        svg.stroke config.IndividualColors.[individual]
        svg.strokeWidth 3
        prop.style [ style.position.defaultStatic ] :?> ISvgAttribute
        svg.children [
            Svg.title $"%s{individual}\n%s{bodypart}"
        ]
    ] 

    let image = Browser.Dom.document.getElementById("canvasImage") :?> HTMLImageElement
    let scale = image.clientWidth / image.naturalWidth * transform.Scale

    ReactDraggable.draggable [
        draggable.child circle
        draggable.scale scale
        draggable.onDrag (fun _ d ->
            { Individual = individual; Bodypart = bodypart; X = d.x; Y = d.y } |> OnLabelDrag |> dispatch
            // printfn "%f %f" d.x d.y
        )
        draggable.onStart (fun _ _ ->
            printfn "+ drag started"
            OnLabelDragStart |> dispatch
        )
        draggable.onStop (fun _ _ ->
            printfn "- drag stopped"
            OnLabelDragStop |> dispatch
        )
    ]

let getSvgLine (c1: Coordinates) (c2: Coordinates) strokeColor (opacity: float) = 
    Svg.line [
        svg.x1 c1.X
        svg.y1 c1.Y
        svg.x2 c2.X
        svg.y2 c2.Y
        svg.stroke strokeColor
        svg.strokeOpacity opacity
        svg.strokeWidth 2
    ]

let svgElements dispatch transform (config: MinimalConfig) (labeledData: LabeledData list) selectedImage (labelDrag: LabelDrag) =
    match selectedImage with
    | Some image ->
        match labeledData with
        | [] -> [| Html.none |]
        | _ ->  let grouped = labeledData |> List.find (fun x -> 
                    match x.FileName  with
                    | EndsWith image.FileName _ -> true
                    | _ -> false)

                let circles = grouped.Labels
                            |> Map.toArray
                            |> Array.map (fun (i, m) -> 
                                    m 
                                    |> Map.toArray
                                    |> Array.choose (fun (b, c) -> 
                                        match c with
                                        | Some x -> 
                                            getSvgCircle dispatch transform i b x 10 config 0.6 
                                            |> Some
                                        | _ -> None
                                    )
                                )
                            |> Array.reduce Array.append

                let lines = config.Individuals
                            |> Array.map (fun i -> 
                                let individual = grouped.Labels.[i]
                                config.Skeleton 
                                |> Array.map (fun xs ->
                                    xs 
                                    |> Array.map (fun x -> 
                                        match (i, x, individual.[x]) with
                                        | di, db, Some c when di = labelDrag.Individual && db = labelDrag.Bodypart -> Some { c with X = c.X + labelDrag.X; Y = c.Y + labelDrag.Y }
                                        | _, _, c -> c
                                    )
                                    |> Array.pairwise
                                    |> Array.choose (fun (c1, c2) -> 
                                        match (c1, c2) with
                                        | Some c1, Some c2 -> getSvgLine c1 c2 "grey" 0.9 |> Some
                                        | _ -> unbox None
                                    )
                                )
                                |> Array.reduce Array.append
                            )
                            |> Array.reduce Array.append

                Array.concat [lines; circles]
    | None -> [| Html.none |]

[<ReactComponent>]
let LabelingCanvas props =
    let state, dispatch = React.useElmish(init props, update props, [| |])
    Html.div [
        Bulma.navbar [
            Bulma.color.isPrimary
            prop.children [
                Bulma.navbarBrand.div [
                    Bulma.navbarItem.a [
                        Bulma.icon [ Html.i [ prop.className "fas fa-draw-polygon fa-2x" ] ]
                    ]
                ]
                Bulma.navbarMenu [
                    Bulma.navbarStart.div [
                        Bulma.navbarItem.div [
                            Bulma.navbarItem.hasDropdown
                            Bulma.navbarItem.isHoverable
                            prop.children [
                                Bulma.navbarLink.a [ prop.text "File" ]
                                Bulma.navbarDropdown.div [
                                    Bulma.navbarItem.a [ 
                                        Html.p  "Load image folder" 
                                        Html.input [
                                            prop.custom ("webkitdirectory", "true")
                                            prop.type'.file
                                            prop.className "file-input"
                                            prop.onChange (fun ev -> 
                                                loadProjectFiles dispatch ev
                                            )
                                        ]
                                    ]
                                    Bulma.navbarDivider []
                                    Bulma.navbarItem.a [ prop.text "Save" ]
                                ]
                            ]
                        ]
                        Bulma.navbarItem.a [ prop.text "Documentation" ]
                    ]
                    Bulma.navbarEnd.div [
                        Bulma.navbarItem.div [
                            Bulma.buttons [
                                Bulma.button.a [
                                    Bulma.icon [ 
                                        prop.onClick (fun _ -> ToggleQuickView |> dispatch )
                                        prop.children [
                                            Html.i [ prop.className "fas fa-cogs" ] 
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

        QuickView.quickview [
            if state.ShowQuickView then yield quickview.isActive
            yield prop.children [
                QuickView.header [
                    Html.div "Config"
                    Bulma.delete [ prop.onClick (fun _ -> ToggleQuickView |> dispatch) ]
                ]
                QuickView.body [
                    QuickView.block "TODO: display config parameters"
                ]
            ]
        ]

        Bulma.columns [

            Bulma.column [
                prop.children [
                    Bulma.hero [
                        Bulma.heroBody [
                            Bulma.title [
                                prop.text (getFileName state.SelectedImage)
                            ]
                            Bulma.image [
                                // Bulma.image.isFullWidth
                                prop.id "canvasContainer"
                                prop.style [
                                    style.overflow.hidden
                                    style.userSelect.none
                                ]
                                prop.children [
                                    Svg.svg [
                                        prop.custom ("transform-origin", "0px 0px 0px") :?> ISvgAttribute
                                        svg.transform [
                                            transform.translate (state.ImageTransformation.X, state.ImageTransformation.Y)
                                            transform.scale state.ImageTransformation.Scale
                                        ]
                                        svg.viewBox (0, 0, 1920, 1080) // TODO: get actual image size
                                        prop.style [ style.position.absolute; style.zIndex 100 ] :?> ISvgAttribute
                                        svg.children [
                                            yield! (svgElements dispatch state.ImageTransformation state.Config state.LabeledData state.SelectedImage state.LabelDrag)
                                        ]
                                    ]
                                    Html.img [
                                        prop.style [
                                            style.position.relative
                                        ]
                                        prop.id "canvasImage"
                                        prop.src (getFileDisplayUrl state.SelectedImage)
                                    ]
                                ]
                            ]
                            Divider.divider [
                                divider.text "loaded images"
                            ]
                            Bulma.columns [
                                prop.style [
                                    style.overflowX.scroll
                                ]
                                prop.children [
                                    yield! (miniViews state dispatch)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            
            Bulma.column [
                Bulma.column.is2
                prop.children [
                    Html.p [
                        Html.strong "Individuals"
                    ]
                    Bulma.select [
                        state.Config.Individuals |> Array.map (fun x -> Html.option x) |> prop.children
                    ]
                    Html.p [
                        Html.strong "Body parts"
                    ]
                    Html.div [
                        state.Config.Multianimalbodyparts 
                        |> Array.map (fun x -> 
                            Bulma.field.div [ 
                                Checkradio.radio [ prop.id x; prop.name "radio" ]
                                Html.label [ prop.htmlFor x; prop.text x ]
                            ])
                        |> prop.children
                    ]
                ]
            ]
        ]
    ]