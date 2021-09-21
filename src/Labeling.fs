module Labeling

open Feliz
open Feliz.UseElmish
open Feliz.Bulma
open Elmish
open ConfigDecoder
open Fable.Core.JsInterop
open System.Collections.Generic

type State = { 
    Config: MinimalConfig 
    ShowQuickView: bool
    LoadedImages: string list
    // LoadedCSVUrl: string option
    // LoadedH5Url: string option
    ErrorMessage: string option }

type Props = { Config: MinimalConfig }

type Msg = 
    | DisaplayConfig of MinimalConfig
    | DisaplayConfigFailed
    | ToggleQuickView
    | FileLoaded of {| FileName: string; ImageBlob: Browser.Types.Blob; DisplayUrl: string |}

let init props = { 
    Config = props.Config; 
    ShowQuickView = false; 
    LoadedImages = []; 
    ErrorMessage = None }, Cmd.none

let update props msg state =
    match msg with 
    | DisaplayConfig config -> state, Cmd.none
    | DisaplayConfigFailed -> state, Cmd.none
    | ToggleQuickView -> { state with ShowQuickView = not state.ShowQuickView }, Cmd.none
    | FileLoaded file -> { state with LoadedImages = state.LoadedImages |> List.append [file.DisplayUrl] }, Cmd.none

let loadImage (fileName: string, blob: Browser.Types.Blob) =
    printf "%s: %i" fileName blob.size
    let fs = Browser.Dom.FileReader.Create()

    fs.onload <- (fun e -> printf "reader returned"
                           let res = e.target?result
                           let storageItem = sprintf "imp_%s" fileName
                           Browser.WebStorage.localStorage.setItem(storageItem, res)
                           let disaplayUrl = Browser.WebStorage.localStorage.getItem(storageItem)
                           let msg = FileLoaded {| FileName = fileName; ImageBlob = blob; DisplayUrl = disaplayUrl |}
                           Cmd.ofMsg(msg) |> ignore)
                       
    fs.readAsDataURL(blob)

let loadImages (fileEvent: Browser.Types.Event) =
    let fileNameBlob (x: Browser.Types.File) = x.name, x.slice()

    let fileList: Browser.Types.FileList = !!fileEvent.target?files
    [|0 .. fileList.length - 1|] 
    |> Array.map (fileList.item >> fileNameBlob)
    |> Array.iter loadImage

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
                                            prop.onChange loadImages
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
                        Bulma.image [
                            Bulma.image.isFullWidth
                            prop.children [
                                Html.image [
                                    prop.src "https://bulma.io/images/placeholders/1280x960.png"
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