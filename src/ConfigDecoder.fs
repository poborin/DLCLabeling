module ConfigDecoder

open Fable.Core.JsInterop
open Thoth.Json
open ColorMap

type MinimalConfig = 
    { 
      Scorer: string
      Individuals: array<string>
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
                  Scorer = get.Required.Field "scorer" Decode.string
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

    member this.BodyColors =
      let spec: IColormapSpec = !!{| colormap = this.Colormap; format = Hex; nshades = this.Multianimalbodyparts.Length |}
      let map = colormap(spec)
      this.Multianimalbodyparts 
      |> Array.mapi (fun i x -> (x, map.[i]))
      |> dict

    member this.IndividualColors =
      let nshades = if this.Individuals.Length > 8 then this.Individuals.Length else 8
      let spec: IColormapSpec = !!{| colormap = "jet"; format = Hex; nshades = nshades |}
      let map = colormap(spec)
      this.Individuals
      |> Array.mapi (fun i x -> (x, map.[i]))
      |> dict

    static member Stub =
        { 
          Scorer = "Pavel"
          Individuals = [|"individual1"; "individual2"; "individual3"; "individual4"; "individual5"; "individual6"; "individual7"; "individual8"|]
          Uniquebodyparts = [|"part1"; "part2"|]
          Multianimalbodyparts = [|"forehead";
                                    "nose";
                                    "neck";
                                    "right_shoulder";
                                    "right_elbow";
                                    "right_wrist";
                                    "right_base_knuckle";
                                    "right_first_knuckle";
                                    "left_shoulder";
                                    "left_elbow";
                                    "left_wrist";
                                    "left_base_knuckle";
                                    "left_first_knuckle";
                                    "shoulder_blades";
                                    "lower_back";
                                    "right_hip";
                                    "right_knee";
                                    "right_ankle";
                                    "right_toes";
                                    "left_hip";
                                    "left_knee";
                                    "left_ankle";
                                    "left_toes"|]
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