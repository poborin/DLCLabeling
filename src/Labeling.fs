module Labeling

open Feliz
open Feliz.UseElmish
open Elmish

type State = { 
    Config: obj
    Individuals: list<string> }

type Props = { Config: obj }

type Msg = 
    | ParseConfig of obj
    | ParseConfigFailed

let init props = { Config = props.Config; Individuals = [] }, Cmd.none

let update props msg state =
    match msg with 
    | ParseConfig config -> state, Cmd.none
    | ParseConfigFailed -> state, Cmd.none

let labelingCanvas = React.functionComponent("LoadConfigForm", fun props -> 
    let state, dispatch = React.useElmish(init props, update props, [| |])

    Html.div [ 
        Html.h1 [
            prop.text (state.Config.ToString())
        ]
    ]
)