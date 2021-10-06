module Data

open Utils
open Feliz

type LabeledData = 
    {
        FileName: string
        Labels: {| Bodypart: string
                   Coordinate: {|X: float; Y: float|} option
                   Individual: string
                   Scorer: string
                |} list
    }

    static member AsyncDecode (csvFile: string) =
        let initRecord = {|
            Scorers = Array.empty<string>
            Individuals = Array.empty<string>
            Bodyparts = Array.empty<string>
            Files = List.empty<string>
        |}

        async {
            let lines = (initRecord, csvFile.Split([|'\n'|])) 
                        ||> Array.fold (fun acc x -> 
                            match x with
                            | Prefix "scorer," rest -> {| acc with Scorers = rest.Split(',') |}
                            | Prefix "individuals," rest -> {| acc with Individuals = rest.Split(',') |}
                            | Prefix "bodyparts," rest -> {| acc with Bodyparts = rest.Split(',') |}
                            | Prefix "coords" _ -> acc
                            | row -> {| acc with Files = acc.Files @ [row] |}
                            )

            return lines.Files |> List.map (fun x -> 
                    let values = x.Split(',')
                    let labels = values.[1 .. values.Length]
                                    |> Array.toList
                                    |> List.pairwise
                                    |> List.map (fun (x, y) -> 
                                        match (parseFloat x, parseFloat y) with
                                        | (Some x, Some y) -> Some {| X = x; Y = y |}
                                        | _ -> None
                                    )
                                    |> List.mapi (fun i x -> 
                                        let scorer = lines.Scorers.[i * 2 ]
                                        let individual = lines.Individuals.[i * 2 ]
                                        let bodypart = lines.Bodyparts.[i * 2]
                                        {| Coordinate = x; Scorer = scorer; Individual = individual; Bodypart = bodypart |}
                                    )
                    { FileName = values.[0]; Labels = labels }
                )
        }

type LabeledData with
    member this.SvgCircles (radius: int) fillColor strokeColor (opacity: float) =
        this.Labels |> List.choose (fun x ->
            match x.Coordinate with
            | Some c ->
                Svg.circle [
                    svg.id ($"%s{x.Individual}.%s{x.Bodypart}")
                    svg.cx c.X
                    svg.cy c.Y
                    svg.radius radius
                    svg.fill fillColor
                    svg.fillOpacity (opacity * 0.8)
                    svg.stroke strokeColor
                    svg.strokeOpacity opacity
                ] |> Some
            | None -> None
        )