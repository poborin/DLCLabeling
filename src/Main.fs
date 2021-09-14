module Main

open Feliz
open Feliz.UseElmish
open Elmish
open Feliz.Bulma
open System.IO
open YamlDotNet.RepresentationModel
open System.Collections

type Msg =
    | LoadConfig

type State = { Config : Generic.IList<YamlDocument> }

let read yaml = 
    use reader = new StringReader(yaml)
    let stream = YamlStream()
    stream.Load(reader)
    stream.Documents

let init() = { Config = read "" }, Cmd.none

let update msg state =
    match msg with
    | LoadConfig -> { state with Config = read "" }, Cmd.none

[<ReactComponent>]
let Counter() =
    let state, dispatch = React.useElmish(init, update, [| |])
    Html.div [  
        Bulma.button.button [
            Bulma.color.isPrimary
            prop.text "LoadConfig"
            prop.onClick (fun _ -> dispatch LoadConfig)
        ]
    ]

open Browser.Dom

ReactDOM.render(Counter(), document.getElementById "feliz-app")