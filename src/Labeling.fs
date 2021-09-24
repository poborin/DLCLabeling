module Labeling

open Feliz
open Feliz.UseElmish
open Feliz.Bulma
open Elmish
open ConfigDecoder
open Fable.Core.JsInterop
open System

type ImageFile = { FileName: string; ImageBlob: Browser.Types.Blob; DisplayUrl: string }

type State = { 
    Config: MinimalConfig 
    ShowQuickView: bool
    LoadedImages: ImageFile list
    // LoadedCSVUrl: string option
    // LoadedH5Url: string option
    SelectedImage: ImageFile option
    ErrorMessage: string option }

type Props = { Config: MinimalConfig }

type Msg = 
    | DisaplayConfig of MinimalConfig
    | DisaplayConfigFailed
    | ToggleQuickView
    | FileLoaded of ImageFile
    | SelectImage of ImageFile

let init props = { 
    Config = props.Config; 
    ShowQuickView = false; 
    LoadedImages = [];
    SelectedImage = None;
    ErrorMessage = None }, Cmd.none

let selectLoadedFile state file =
    match state.SelectedImage with
    | Some file -> Cmd.none
    | None -> Cmd.ofMsg(SelectImage file)
 
let update props msg state =
    match msg with 
    | DisaplayConfig config -> state, Cmd.none
    | DisaplayConfigFailed -> state, Cmd.none
    | ToggleQuickView -> { state with ShowQuickView = not state.ShowQuickView }, Cmd.none
    | FileLoaded file -> 
        { state with LoadedImages = state.LoadedImages |> List.append [file] |> List.sortBy (fun x -> x.FileName) }, selectLoadedFile state file
    | SelectImage file -> 
        { state with SelectedImage = Some file }, Cmd.none

let loadImage onLoad (fileName: string, blob: Browser.Types.Blob) =
    let reader = Browser.Dom.FileReader.Create()

    reader.onload <- (fun ev -> 
        let disaplayUrl = ev.target?result //|> unbox |> System.Convert.ToBase64String
        let file = { FileName = fileName; ImageBlob = blob; DisplayUrl = disaplayUrl }
        onLoad file)
                       
    reader.readAsDataURL(blob)

let loadImages onLoad (fileEvent: Browser.Types.Event) =
    let fileNameBlob (x: Browser.Types.File) = x.name, x.slice()

    let fileList: Browser.Types.FileList = !!fileEvent.target?files
    [|0 .. fileList.length - 1|] 
    |> Array.rev
    |> Array.map (fileList.item >> fileNameBlob)
    |> Array.iter (loadImage onLoad)

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
    | false -> prop.style [style.border(3, borderStyle.dashed, color.white)]

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
                            prop.onClick (fun _ -> 
                                Console.Write(x.FileName)
                                x |> SelectImage |> dispathc)
                        ]
                    ]
                ]
            ]
        ]
    )

let labelingCanvas = React.functionComponent("LabelingCanvas", fun props -> 
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
                                                let onLoad = (FileLoaded >> dispatch)
                                                loadImages onLoad ev
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
                                Bulma.image.isFullWidth
                                prop.children [
                                    Html.img [
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
                    Bulma.control.p [
                        prop.children [
                            Bulma.select [
                                state.Config.Individuals |> Array.map (fun x -> Html.option x) |> prop.children
                            ]
                        ]
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
)