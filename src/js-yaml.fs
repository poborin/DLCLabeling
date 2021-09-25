module rec JsYaml

#nowarn "3390" // disable warnings for invalid XML comments

open System
open Fable.Core
open Fable.Core.JS

[<ImportMember("js-yaml")>]
let load(yaml: string): string = jsNative