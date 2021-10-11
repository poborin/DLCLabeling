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
          Skeleton = [|[|"right_shoulder"; "right_elbow"|];
                    [|"left_ankle"; "left_toes"|];
                    [|"right_shoulder"; "shoulder_blades"|];
                    [|"forehead"; "neck"|];
                    [|"left_shoulder"; "left_elbow"|];
                    [|"lower_back"; "left_hip"|];
                    [|"right_ankle"; "right_toes"|];
                    [|"neck"; "left_shoulder"|];
                    [|"shoulder_blades"; "lower_back"|];
                    [|"right_elbow"; "right_wrist"|];
                    [|"right_wrist"; "right_base_knuckle"|];
                    [|"lower_back"; "right_hip"|];
                    [|"forehead"; "nose"|];
                    [|"left_elbow"; "left_wrist"|];
                    [|"nose"; "neck"|];
                    [|"left_wrist"; "left_base_knuckle"|];
                    [|"left_hip"; "left_knee"|];
                    [|"neck"; "shoulder_blades"|];
                    [|"right_base_knuckle"; "right_first_knuckle"|];
                    [|"right_hip"; "right_knee"|];
                    [|"left_knee"; "left_ankle"|];
                    [|"neck"; "right_shoulder"|];
                    [|"left_base_knuckle"; "left_first_knuckle"|];
                    [|"left_shoulder"; "shoulder_blades"|];
                    [|"right_knee"; "right_ankle"|]|]
          Bodyparts = "MULTI!"
          SkeletonColor = "black"
          Dotsize = 12
          Alphavalue = 0.7
          Colormap = "rainbow"
        }