module Labeling

open Feliz
open Feliz.UseElmish
open Feliz.Bulma
open Elmish
open ConfigDecoder
open System

type State = { 
    Config: MinimalConfig }

type Props = { Config: MinimalConfig }

type Msg = 
    | DisaplayConfig of MinimalConfig
    | DisaplayConfigFailed

let init props = { Config = props.Config }, Cmd.none

let update props msg state =
    match msg with 
    | DisaplayConfig config -> state, Cmd.none
    | DisaplayConfigFailed -> state, Cmd.none
 
let labelingCanvas = React.functionComponent("LoadConfigForm", fun props -> 
    let state, dispatch = React.useElmish(init props, update props, [| |])

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
                Divider.divider [
                    prop.text "Images"
                ]
                Bulma.label [
                    
                    prop.children [
                        Bulma.fileInput [
                            
                        ]
                        Html.span [
                            prop.className "file-cta"
                            prop.children [
                                Html.span [
                                    prop.className "file-label"
                                    prop.text "Load images"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
)