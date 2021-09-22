module Labeling

open Feliz
open Feliz.UseElmish
open Feliz.Bulma
open Elmish
open ConfigDecoder
open Fable.Core.JsInterop
open System.Collections.Generic

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
        printf "file loaded: %A" file
        { state with LoadedImages = state.LoadedImages |> List.append [file] }, selectLoadedFile state file
    | SelectImage file -> 
        printf "file selected: %A" file
        { state with SelectedImage = Some file }, Cmd.none

let loadImage onLoad (fileName: string, blob: Browser.Types.Blob) =
    printf "%s: %i" fileName blob.size
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
    |> Array.map (fileList.item >> fileNameBlob)
    |> Array.iter (loadImage onLoad)

let getFileDisplayUrl file =
    printf "selected file %A" file
    match file with
    | Some x -> x.DisplayUrl
    | None -> "https://bulma.io/images/placeholders/1280x960.png"

let getFileName file = 
    match file with
    | Some x -> x.FileName
    | None -> "placeholder"

let miniViews state =
    state.LoadedImages |> List.toSeq |> Seq.map (fun x ->
        Bulma.image [            
            Bulma.image.isFullWidth
            prop.children [
                Html.image [
                    prop.src x.DisplayUrl
                ]
            ]
        ]
    )

let labelingCanvas = React.functionComponent("LabelingCanvas", fun props -> 
    let state, dispatch = React.useElmish(init props, update props, [| |])
    printf "state %A" state
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
                    Bulma.section [
                        Bulma.hero [
                            prop.text (getFileName state.SelectedImage)
                        ]
                        Bulma.image [
                            Bulma.image.isFullWidth
                            prop.children [
                                Html.image [
                                    prop.src (getFileDisplayUrl state.SelectedImage)
                                ]
                            ]
                        ]
                    ]
                    Html.div [
                        // prop.style [
                        //     style.overflowX.scroll
                        //     style.width.fitContent
                        // ]
                        // prop.children [
                        //     yield! miniViews state
                        // ]
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