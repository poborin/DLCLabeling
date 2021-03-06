module LoadConfig

open Feliz
open Feliz.UseElmish
open Elmish
open System
open Thoth.Json
open ConfigDecoder

type State = { IsConfigLoaded: bool }

type Props = {| LoadConfig: MinimalConfig -> unit |}

type Msg =
    | LoadConfig of string
    | LoadConfigError

let init() = { IsConfigLoaded = false }, Cmd.none

let update (props: Props) msg state =
    match msg with 
    | LoadConfig config ->
        let objConfig = JsYaml.load(config)
        let miniConfig = Decode.fromValue "$" MinimalConfig.Decoder objConfig
        match miniConfig with
        | Ok result -> { state with IsConfigLoaded = true }, Cmd.ofSub(fun _-> props.LoadConfig(result))
        | Error _ -> { state with IsConfigLoaded = false }, Cmd.ofMsg(LoadConfigError)
        
    | LoadConfigError -> { state with IsConfigLoaded = false }, Cmd.none

[<ReactComponent>]
let ConfigLoader props = 
    let state, dispatch = React.useElmish(init, update props, [| |])

    Html.div [  
        prop.className "center-screen"
        prop.children [
            Html.div [
                // https://safe-stack.github.io/docs/recipes/client-server/upload-file-from-client/
                // Bulma.file [
                //     Bulma.fileLabel.label [
                //         Bulma.fileInput [
                //             // prop.onChange (handleFileEvent onLoad)
                //         ]
                //         Bulma.fileCta [
                //             Bulma.fileLabel.label "Choose a file..."
                //         ]
                //     ]
                // ]
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


                                    reader.onerror <- fun evt ->
                                        dispatch LoadConfigError

                                    reader.readAsText(file)
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
