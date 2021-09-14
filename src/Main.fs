module Main

open Feliz
open Feliz.UseElmish
open Elmish
open Feliz.Bulma

type Msg =
    | Increment
    | Decrement

type State = { Count : int }

let init() = { Count = 0 }, Cmd.none

let update msg state =
    match msg with
    | Increment -> { state with Count = state.Count + 1 }, Cmd.none
    | Decrement -> { state with Count = state.Count - 1 }, Cmd.none

[<ReactComponent>]
let Counter() =
    let state, dispatch = React.useElmish(init, update, [| |])
    Html.div [  
        Html.h1 state.Count
        Bulma.button.button [
            Bulma.color.isPrimary
            prop.text "Increment"
            prop.onClick (fun _ -> dispatch Increment)
        ]

        Bulma.button.button [
            Bulma.color.isDark
            prop.text "Decrement"
            prop.onClick (fun _ -> dispatch Decrement)
        ]
    ]

open Browser.Dom

ReactDOM.render(Counter(), document.getElementById "feliz-app")