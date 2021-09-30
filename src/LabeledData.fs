module Data
open System

type LabeledData = 
    static member Decode (csvFile: string) =
        let (|Prefix|_|) (p:string) (s:string) =
            if s.StartsWith(p) then
                Some(s.Substring(p.Length))
            else
                None

        let parseFloat (s: string) =
            match Double.TryParse(s) with 
            | true, n -> Some n
            | _ -> None

        let initRecord = {|
            Scorers = Array.empty<string>
            Individuals = Array.empty<string>
            Bodyparts = Array.empty<string>
            Files = List.empty<string>
        |}

        let lines = (initRecord, csvFile.Split([|'\n'|])) 
                    ||> Array.fold (fun acc x -> 
                        match x with
                        | Prefix "scorer," rest -> {| acc with Scorers = rest.Split(',') |}
                        | Prefix "individuals," rest -> {| acc with Individuals = rest.Split(',') |}
                        | Prefix "bodyparts," rest -> {| acc with Bodyparts = rest.Split(',') |}
                        | Prefix "coords" _ -> acc
                        | row -> {| acc with Files = acc.Files @ [row] |}
                        )
        try 
            let result = lines.Files |> List.map (fun x -> 
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
                    {| FileName = values.[0]; Labels = labels |}
                )

            Ok result
        with
        | e -> Error e.Message