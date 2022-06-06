module Main

open Feliz
open Feliz.UseElmish
open Elmish
open ConfigDecoder

type State = { CurrentConfig: MinimalConfig option }

type Msg =
    | LoadConfig of MinimalConfig

// #if DEBUG
// let init = { CurrentConfig = Some MinimalConfig.Stub }, Cmd.none
// #else
let init = { CurrentConfig = None }, Cmd.none
// #endif


let update msg state =
    match msg with 
    | LoadConfig config -> 
        { state with CurrentConfig = Some(config) }, Cmd.none

[<ReactComponent>]
let DLCLabeling() =
    let state, dispatch = React.useElmish(init, update, [| |])
    match state.CurrentConfig with
    | Some config -> 
        Labeling.LabelingCanvas {| Config = config |}
    | None -> 
        LoadConfig.ConfigLoader {| LoadConfig = fun config -> config |> LoadConfig |> dispatch |} 

open Browser.Dom

ReactDOM.render(DLCLabeling(), document.getElementById "feliz-app")