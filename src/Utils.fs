module Utils

open System

let (|EndsWith|_|) expected (name: string) = 
    let result = name.EndsWith (expected, StringComparison.CurrentCultureIgnoreCase)
    match result with
    | true -> Some true
    | _ -> None

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None
        
let parseFloat (s: string) =
    match Double.TryParse(s) with 
    | true, n -> Some n
    | _ -> None