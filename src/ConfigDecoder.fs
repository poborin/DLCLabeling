module ConfigDecoder

open Thoth.Json

type MinimalConfig = 
    { Individuals: array<string>
      Uniquebodyparts: array<string>
      Multianimalbodyparts: array<string>
      Skeleton: array<array<string>>
      Bodyparts: string
      SkeletonColor: string
      Dotsize: int
      Alphavalue: float
      Colormap: string
    }

    static member Decoder: Decoder<MinimalConfig> = 
        Decode.object
            (fun get -> 
                {
                    Individuals = get.Required.Field "individuals" (Decode.array Decode.string)
                    Uniquebodyparts = get.Required.Field "uniquebodyparts" (Decode.array Decode.string)
                    Multianimalbodyparts = get.Required.Field "multianimalbodyparts" (Decode.array Decode.string)
                    Skeleton = get.Required.Field "skeleton" (Decode.array (Decode.array Decode.string))
                    Bodyparts = get.Required.Field "bodyparts" Decode.string
                    SkeletonColor = get.Required.Field "skeleton_color" Decode.string
                    Dotsize = get.Required.Field "dotsize" Decode.int
                    Alphavalue = get.Required.Field "alphavalue" Decode.float
                    Colormap = get.Required.Field "colormap" Decode.string
                })

    static member Stub =
        { Individuals = [|"individual1"; "individual2"|]
          Uniquebodyparts = [|"part1"; "part2"|]
          Multianimalbodyparts = [|"arm"; "leg"|]
          Skeleton = [|[|"arm"; "leg"|]|]
          Bodyparts = "MULTI!"
          SkeletonColor = "black"
          Dotsize = 12
          Alphavalue = 0.7
          Colormap = "rainbow"
        }