module Main

open Feliz
open Feliz.UseElmish
open Elmish

type State = { CurrentConfig: string option }

type Msg =
    | LoadConfig of string

let init = { CurrentConfig = None }, Cmd.none

let update msg state =
    match msg with 
    | LoadConfig config -> 
        { state with CurrentConfig = Some(config) }, Cmd.none

[<ReactComponent>]
let DLCLabeling() =
    let state, dispatch = React.useElmish(init, update, [| |])
    match state.CurrentConfig with
    | Some config -> 
        Labeling.labelingCanvas { Config = config } 
    | None -> 
        LoadConfig.configLoader { LoadConfig = fun config -> config |> LoadConfig |> dispatch} 

open Browser.Dom

ReactDOM.render(DLCLabeling(), document.getElementById "feliz-app")