module Data

open Utils
open ConfigDecoder
open System.Collections.Generic

type Coordinates =
    { 
        X: double;
        Y: double
    }

type LabeledData = {
    FileName: string;
    Labels: Map<string,Map<string, Coordinates option>>
} 

type CSVData =
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
                                        {| Coordinates = x; Scorer = scorer; Individual = individual; Bodypart = bodypart |}
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

    static member AsyncEncode config data =
        async {
            let bodyparts = config.Multianimalbodyparts
            let individuals = config.Individuals

            let coordinate c =
                match c with
                | Some c -> $"%f{c.X},%f{c.Y}"
                | None -> ","

            let scorerHeader = Array.create (individuals.Length * bodyparts.Length * 2) config.Scorer 
                                |> Array.append [|"scorer"|]
                                |> String.concat ","
            
            let individualsHeader = individuals
                                    |> Array.map (fun x -> Array.create (bodyparts.Length * 2) x)
                                    |> Array.reduce Array.append
                                    |> Array.append [|"individuals"|]
                                    |> String.concat ","

            let bodypartsHeader = individuals
                                    |> Array.map (fun _ ->
                                        bodyparts
                                        |> Array.map (fun x -> [|x; x|])
                                        |> Array.reduce Array.append
                                    )
                                    |> Array.reduce Array.append
                                    |> Array.append [|"bodyparts"|]
                                    |> String.concat ","

            let coordiantesHeader = Array.create (individuals.Length * bodyparts.Length) [|"x"; "y"|]
                                    |> Array.reduce Array.append
                                    |> Array.append [|"coords"|]
                                    |> String.concat ","

            let res = data 
                        |> List.toArray
                        |> Array.map (fun d ->
                                        d.Labels
                                        |> Map.toArray
                                        |> Array.map (fun (_, g) -> g
                                                                    |> Map.toArray
                                                                    |> Array.map (fun (_, c) -> c)
                                                    )
                                        |> Array.reduce Array.append
                                        |> Array.map coordinate
                                        |> Array.append [|d.FileName|]
                                        |> String.concat ","
                                    )
                        |> Array.append [|scorerHeader; individualsHeader; bodypartsHeader; coordiantesHeader|]
                        |> String.concat "\n"

            return res
        }