module Main

open Feliz
open Feliz.UseElmish
open Elmish
open Feliz.Bulma
open System.IO
open System.Collections

type Msg =
    | LoadConfig of string

type LoadConfigState = { 
    Config : Generic.IList<string> 
}

let read yaml = 
    // use reader = new StringReader(yaml)
    // let stream = YamlStream()
    // stream.Load(reader)
    // stream.Documents
    new Generic.List<string>()

let init() = { 
    Config = new Generic.List<string>() }, Cmd.none

let update msg state =
    match msg with
    | LoadConfig (path) -> { state with Config = read path }, Cmd.none

[<ReactComponent>]
let ConfigLoader() =
    let state, dispatch = React.useElmish(init, update, [| |])
    Html.div [  
        prop.className "center-screen"
        prop.children [
            Html.div [
                prop.className "file is-normal is-boxed"
                prop.children [
                    Html.label [
                        prop.className "file-label"
                        prop.children [
                            Html.input[
                                prop.className "file-input"
                                prop.name "config"
                                prop.type'.file
                                // prop.defaultValue "config.yaml"
                                prop.onInput (fun ev ->
                                    let file = (ev.target :?> Browser.Types.HTMLInputElement).files.Item(0)
                                    let reader = Browser.Dom.FileReader.Create()
                                    reader.onload <- fun evt ->
                                    (*
                                        Negotiate/assume the onload target is a FileReader
                                        Result is a string containg file contents:
                                        https://developer.mozilla.org/en-US/docs/Web/API/FileReader/readAsText
                                    *)
                                    dispatch (LoadConfig (string (evt.target :?> Browser.Types.FileReader).result))
                                )
                            ]
                            Html.span [
                                prop.className "file-cta"
                                prop.children [
                                    Html.span [
                                        prop.className "file-label"
                                        prop.text "Choose config.yaml"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

open Browser.Dom

ReactDOM.render(ConfigLoader(), document.getElementById "feliz-app")