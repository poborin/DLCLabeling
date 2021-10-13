module Data

open Utils
open Feliz
open ConfigDecoder
open System.Collections.Generic

type Coordinate =
    { 
        X: float;
        Y: float
    }

type Label =
    { 
        Bodypart: string
        Coordinate: Coordinate option
        Individual: string
        Scorer: string
    }

type LabeledData = 
    {
        FileName: string
        Labels: Label list
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
                                    |> Array.chunkBySize 2
                                    |> Array.map (fun pair -> 
                                        match (parseFloat pair.[0], parseFloat pair.[1]) with
                                        | (Some x, Some y) -> Some { X = x; Y = y }
                                        | _ -> None
                                    )
                                    |> Array.mapi (fun i x -> 
                                        let scorer = lines.Scorers.[i * 2 ]
                                        let individual = lines.Individuals.[i * 2 ]
                                        let bodypart = lines.Bodyparts.[i * 2]
                                        { Coordinate = x; Scorer = scorer; Individual = individual; Bodypart = bodypart }
                                    )
                                    |> Array.toList
                    { FileName = values.[0]; Labels = labels }
                )
        }

    member this.SvgCircles (radius: int) (fillColors: IDictionary<string, string>) (strokeColor: IDictionary<string, string>) (opacity: float) =
        let circles ls =
            ls |> List.choose (fun x ->
            match x.Coordinate with
            | Some c ->
                Svg.circle [
                    svg.id $"%s{x.Individual}.%s{x.Bodypart}"
                    svg.cx c.X
                    svg.cy c.Y
                    svg.r radius
                    svg.fill fillColors.[x.Bodypart]
                    svg.fillOpacity opacity
                    svg.stroke strokeColor.[x.Individual]
                    svg.strokeWidth 3
                    prop.style [ style.position.defaultStatic ] :?> ISvgAttribute
                    svg.children [
                        Svg.title $"%s{x.Individual}\n%s{x.Bodypart}"
                    ]
                ] |> Some
            | None -> None
        )
        
        this.Labels 
        |> List.groupBy (fun x -> x.Individual)
        |> List.map (fun (x, ls) -> 
            Svg.g [
                svg.id x
                svg.children [
                    yield! ls |> circles
                ]
            ] 
        )

    member this.Skeleton config strokeColor (opacity: float) =
        let svgLine c1 c2 strokeColor (opacity: float) = 
            Svg.line [
                svg.x1 c1.X
                svg.y1 c1.Y
                svg.x2 c2.X
                svg.y2 c2.Y
                svg.stroke strokeColor
                svg.strokeOpacity opacity
                svg.strokeWidth 2
            ]
            
        let groups = this.Labels
                        |> List.groupBy (fun x -> x.Individual)
                        |> List.map (fun (i, ls) -> 
                            let bs = ls 
                                    |> List.map (fun l -> (l.Bodypart, l.Coordinate))
                                    |> Map.ofList
                            (i, bs)
                        )
                        |> Map.ofList

        config.Individuals
        |> Array.map (fun i -> 
            let individual = groups.[i]
            config.Skeleton 
            |> Array.map (fun xs ->
                xs 
                |> Array.map (fun x -> individual.[x])
                |> Array.pairwise
                |> Array.choose (fun (c1, c2) -> 
                    match (c1, c2) with
                    | Some c1, Some c2 -> svgLine c1 c2 strokeColor opacity |> Some
                    | _ -> None
                )
            )
            |> Array.reduce Array.append
        )
        |> Array.reduce Array.append