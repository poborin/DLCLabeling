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
open Feliz.UseListener

type ProjectFile = { FileName: string; ImageBlob: Browser.Types.Blob; DisplayUrl: string }
type LabelDrag = { Individual: Individual; Bodypart: Bodypart; X: float; Y: float }
type ImageTransformation = { X: float; Y: float; Scale: float }

type State = { 
    Config: MinimalConfig 
    ShowQuickView: bool
    LoadedImages: ProjectFile list
    LabeledData: LabeledData list
    SelectedImage: ProjectFile option
    SelectedLabel: (Individual * Bodypart)
    ImageTransformation: ImageTransformation
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
    | OnLabelSelected of (Individual * Bodypart)
    | OnDeleteAnnotation of (Individual * Bodypart)
    | OnNewAnnotation of (Individual * Bodypart * Coordinates option)
    | OnConfigUpdate of MinimalConfig
    | CreatePanZoom of PanZoom
    | GenerateCSV
    | SaveCSV of string
    | LogError of exn

let init (props: Props) = { 
        Config = props.Config; 
        ShowQuickView = false; 
        LoadedImages = List.empty;
        LabeledData = List.empty;
        SelectedImage = None;
        SelectedLabel = (props.Config.Individuals.[0], props.Config.Multianimalbodyparts.[0])
        ImageTransformation = { X = 0.0; Y = 0.0; Scale = 1.0 }
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
        let bounds: BoundsOption = Boolean(true)
        let options: PanZoomOptions = !!{| maxZoom = Some 5.; minZoom = Some 1.; bounds = Some bounds; boundsPadding = Some 1.|}
        let pz = panzoom.createPanZoom(element, options)
        Cmd.batch [   
            Cmd.ofMsg (SelectImage file)
            Cmd.ofMsg (CreatePanZoom pz)
            Cmd.ofSub (onPanZoom pz)
        ]

let deleteSeletedLabel state label =
    match state.SelectedImage with
    | Some file ->
        let i, b = label
        let grouped = state.LabeledData |> List.find (fun x -> 
            match x.FileName  with
            | EndsWith file.FileName _ -> true
            | _ -> false)

        match grouped.Labels.ContainsKey i with
        | true -> 
            let newIndivifdual = grouped.Labels.[i].Remove b
            let newLabels = grouped.Labels.Add (i, newIndivifdual)
            state.LabeledData |> List.map (fun x -> 
                match x.FileName  with
                | EndsWith file.FileName _ -> 
                    let (individual, bodypart ) = label
                    let circleId = $"%s{individual}.%s{bodypart}"
                    let circle = Browser.Dom.document.getElementById(circleId)
                    circle.removeAttribute("transform")
                    circle.removeAttribute("class")
                    
                    { x with Labels = newLabels }
                | _ -> x)
        | false -> state.LabeledData
        
    | None -> state.LabeledData

let download fileName fileContent =
    let anchor = Browser.Dom.document.createElement "a"
    let encodedContent = fileContent |> sprintf "data:text/csv;charset=utf-8,%s" |> Fable.Core.JS.encodeURI
    anchor.setAttribute("href",  encodedContent)
    anchor.setAttribute("download", fileName)
    anchor.click()

let findLabels selectedImage (labeledData: LabeledData list) map =
    match selectedImage with
    | Some image ->
        match labeledData with
        | [] -> labeledData
        | _ ->  labeledData |> List.map (fun x -> 
                    match x.FileName  with
                    | EndsWith image.FileName _ -> map x
                    | _ -> x)
                
    | None -> labeledData

let updateLabeledData selectedImage labeledData drag = 
    let updateLabel drag (labels: Map<Individual,Map<Bodypart,option<Coordinates>>>) =
        let newValue =
            labels.[drag.Individual]
            |> Map.change
                drag.Bodypart
                (fun b -> 
                    match b with
                    | Some (Some c) -> 
                        printfn "coordinates = %A,\ndeltaDrag = %A" c drag
                        Some (Some { c with OffsetX = c.OffsetX + drag.X; OffsetY = c.OffsetY + drag.Y})
                    | _ -> None)
        labels |> Map.change
                    drag.Individual
                    (fun i -> 
                        match i with 
                        | Some i -> Some newValue
                        | None -> None)
    
    let map x = 
        let newLabels = updateLabel drag x.Labels
        { x with Labels = newLabels}

    findLabels selectedImage labeledData map

let addNewLabeledData selectedImage labeledData newLabel =
    let addNewLabel (individual, bodypart, coordinates) (labels: Map<Individual,Map<Bodypart,option<Coordinates>>>) =
        let newValue =
            labels.[individual]
            |> Map.add bodypart coordinates
        labels |> Map.add individual newValue
    
    let map x =
        let (individual, bodypart, coordinates) = newLabel
        let newLabels = addNewLabel newLabel x.Labels
        
        match x.Labels.ContainsKey individual with
        | true -> 
            match x.Labels.[individual].ContainsKey bodypart with
            | true ->
                match x.Labels.[individual].[bodypart] with 
                | Some b -> x
                | None -> { x with Labels = newLabels}
            | false -> { x with Labels = newLabels}
        | false -> { x with Labels = newLabels}
    
    findLabels selectedImage labeledData map

let update props msg state =
    match msg with 
    | AddPanZoom -> state, Cmd.none
    | ToggleQuickView -> { state with ShowQuickView = not state.ShowQuickView }, Cmd.none
    | ImageLoaded file -> 
        { state with LoadedImages = state.LoadedImages |> List.append [file] |> List.sortBy (fun x -> x.FileName) }, selectLoadedFile state file
    | CSVLoaded content ->
        state, Cmd.OfAsync.either CSVData.AsyncDecode content DisplayLabels LogError
    | SelectImage file -> 
        { state with SelectedImage = Some file }, Cmd.none
    | DisplayLabels labels ->
        { state with LabeledData = labels }, Cmd.none
    | OnImageTransform transform ->
        { state with ImageTransformation = transform}, Cmd.none
    | OnLabelDrag drag ->
        let updatedLabels = updateLabeledData state.SelectedImage state.LabeledData drag
        {state with LabeledData = updatedLabels}, Cmd.none
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
    | OnLabelSelected selectedLabel ->
        { state with SelectedLabel = selectedLabel}, Cmd.none
    | OnDeleteAnnotation selectedLabel ->
        let labeledData = deleteSeletedLabel state selectedLabel
        {state with LabeledData = labeledData}, Cmd.none
    | OnNewAnnotation newLabel ->
        let newLabels = addNewLabeledData state.SelectedImage state.LabeledData newLabel
        { state with LabeledData = newLabels }, Cmd.none
    | OnConfigUpdate config ->
        { state with Config = config }, Cmd.none
    | CreatePanZoom pz -> { state with PanZoom = Some pz }, Cmd.none
    | GenerateCSV -> 
        let encode = CSVData.AsyncEncode state.Config
        state, Cmd.OfAsync.either encode state.LabeledData SaveCSV LogError
    | SaveCSV csv-> 
        download $"CollectedData_%s{state.Config.Scorer}" csv
        state, Cmd.none
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

let getSvgCircle dispatch transform individual bodypart (coordinate: Coordinates) (config: MinimalConfig) =
    let circle = Svg.circle [
        prop.style [ style.position.defaultStatic ] :?> ISvgAttribute
        svg.children [
            Svg.title $"%s{individual}\n%s{bodypart}"
        ]
        svg.cx coordinate.X
        svg.cy coordinate.Y
        svg.fill config.BodyColors.[bodypart]
        svg.fillOpacity config.Alphavalue
        svg.id $"%s{individual}.%s{bodypart}"
        svg.onClick (fun _ -> OnLabelSelected (individual, bodypart) |> dispatch)
        svg.r config.Dotsize
        svg.stroke config.IndividualColors.[individual]
        svg.strokeWidth 3
    ] 

    let image = Browser.Dom.document.getElementById("canvasImage") :?> HTMLImageElement
    let scale = image.clientWidth / image.naturalWidth * transform.Scale

    ReactDraggable.draggable [
        draggable.child circle
        draggable.scale scale
        draggable.onDrag (fun _ d ->
            { Individual = individual; Bodypart = bodypart; X = d.deltaX; Y = d.deltaY } |> OnLabelDrag |> dispatch
        )
        draggable.onStart (fun _ _ ->
            printfn "+ drag started"
            OnLabelDragStart |> dispatch
        )
        draggable.onStop (fun c d ->
            printfn "- drag stopped"
            OnLabelDragStop |> dispatch
        )
    ]

let getSvgLine (c1: Coordinates) (c2: Coordinates) strokeColor (opacity: float) = 
    Svg.line [
        svg.x1 (c1.X + c1.OffsetX)
        svg.y1 (c1.Y + c1.OffsetY)
        svg.x2 (c2.X + c2.OffsetX)
        svg.y2 (c2.Y + c2.OffsetY)
        svg.stroke strokeColor
        svg.strokeOpacity opacity
        svg.strokeWidth 2
    ]

let svgElements dispatch transform (config: MinimalConfig) (labeledData: LabeledData list) selectedImage =
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
                                            getSvgCircle dispatch transform i b x config
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
                                        match individual |> Map.containsKey x with
                                        | true -> individual.[x]
                                        | false -> None
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

    React.useListener.onKeyDown(fun ev ->
        match ev.key with
        | "Backspace" | "Delete" -> OnDeleteAnnotation state.SelectedLabel |> dispatch
        | x -> printfn "%s" x
    )
    React.useListener.onContextMenu(fun ev ->
        ev.preventDefault()
    )

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
                                    Bulma.navbarItem.a [ 
                                        prop.text "Save"
                                        prop.onClick (fun _ -> 
                                            GenerateCSV |> dispatch
                                        )
                                    ]
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
                    QuickView.block [
                        prop.children [
                            Bulma.hero [
                                Bulma.heroBody [
                                    Html.p [
                                        Html.strong "Labels size"
                                    ]
                                    Slider.slider [ 
                                        slider.isFullWidth
                                        color.isPrimary
                                        prop.min 1
                                        prop.max 24
                                        prop.value state.Config.Dotsize
                                        prop.onChange (fun v -> 
                                            let config = { state.Config with Dotsize = v }
                                            OnConfigUpdate config |> dispatch
                                        )
                                        ]
                                    Html.p [
                                        Html.strong "Labels opacity"
                                    ]
                                    Slider.slider [ 
                                        slider.isFullWidth; 
                                        color.isPrimary
                                        prop.min 0.1
                                        prop.max 1
                                        prop.step 0.05
                                        prop.value state.Config.Alphavalue
                                        prop.onChange (fun v -> 
                                            let config = { state.Config with Alphavalue = v }
                                            OnConfigUpdate config |> dispatch
                                        )
                                    ]
                                ]
                            ]
                        ]
                    ]     
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
                                prop.onContextMenu (fun ev ->
                                    let image = Browser.Dom.document.getElementById("canvasImage") :?> HTMLImageElement
                                    let scale = image.clientWidth / image.naturalWidth * state.ImageTransformation.Scale
                                    let boundingRect = image.getBoundingClientRect()

                                    let (individual, bodypart) = state.SelectedLabel
                                    let coordinates = { X = (ev.clientX - boundingRect.left) / scale;
                                                        Y = (ev.clientY - boundingRect.top) / scale;
                                                        OffsetX = 0.0;
                                                        OffsetY = 0.0}

                                    OnNewAnnotation (individual, bodypart, Some coordinates) |> dispatch
                                )
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
                                            yield! (svgElements dispatch state.ImageTransformation state.Config state.LabeledData state.SelectedImage)
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
                        prop.onChange(fun (e: Event) ->
                            let i = (e.target :?> Browser.Types.HTMLSelectElement).selectedIndex
                            let individual = state.Config.Individuals.[i]
                            let (_, bodypart) = state.SelectedLabel
                            OnLabelSelected (individual, bodypart) |> dispatch
                        )
                        prop.children [
                            yield! state.Config.Individuals
                                    |> Array.mapi (fun i x -> 
                                        let individual, _ = state.SelectedLabel
                                        match state.SelectedLabel with
                                        | (i, _) when individual = x -> Html.option [
                                                prop.value i
                                                prop.text x
                                                prop.selected true
                                            ]
                                        | _ -> Html.option [
                                            prop.value i
                                            prop.text x
                                        ]
                                    )
                        ]
                    ]
                    Html.p [
                        Html.strong "Body parts"
                    ]
                    Html.div [
                        prop.children [
                            yield! state.Config.Multianimalbodyparts 
                                    |> Array.map (fun x -> 
                                        let check = prop.onChange(fun (b: String) -> 
                                            let (individual, _) = state.SelectedLabel
                                            OnLabelSelected (individual, b) |> dispatch
                                        )
                                        let props = match state.SelectedLabel with
                                                    | (_, b) when b = x -> [ prop.id x; prop.value x; prop.name "radio"; prop.custom("checked", "checked"); check]
                                                    | _ -> [ prop.id x; prop.value x; prop.name "radio"; check]
                                        Bulma.field.div [ 
                                            Checkradio.radio props
                                            Html.label [ prop.htmlFor x; prop.text x ]
                                        ])
                        ]

                    ]
                ]
            ]
        ]
    ]