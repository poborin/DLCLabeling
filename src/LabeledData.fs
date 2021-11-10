module Data

open Utils
open Feliz
open ConfigDecoder
open System.Collections.Generic


type Coordinates =
    { 
        X: float;
        Y: float
    }

type Label =
    { 
        Bodypart: string
        Coordinates: Coordinates option
        Individual: string
        Scorer: string
    }

type LabeledData = 
    {
        FileName: string
        Labels: Map<string,Map<string, Coordinates option>>
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
                                        | _ -> unbox None
                                    )
                                    |> Array.mapi (fun i x -> 
                                        let scorer = lines.Scorers.[i * 2 ]
                                        let individual = lines.Individuals.[i * 2 ]
                                        let bodypart = lines.Bodyparts.[i * 2]
                                        { Coordinates = x; Scorer = scorer; Individual = individual; Bodypart = bodypart }
                                    )
                                    |> Array.groupBy (fun x -> x.Individual)
                                    |> Array.map (fun (i, ls) -> 
                                        let bs = ls 
                                                |> Array.map (fun l -> (l.Bodypart, l.Coordinates))
                                                |> Map.ofArray
                                        (i, bs)
                                    )
                                    |> Map.ofArray
                    { FileName = values.[0]; Labels = labels }
                )
        }

    static member Encode (config: MinimalConfig) (data: LabeledData list) =
        let bodyparts = config.Bodyparts
        let individuals = config.Individuals

        let coordinate c =
            match c with
            | Some c -> $"%f{c.X},%f{c.Y}"
            | None -> ","

        // individuals |> Array.map (fun i ->
        //     bodyparts |> Array.map (fun b ->
        //         data |> map
        //     )
        // )

        ""