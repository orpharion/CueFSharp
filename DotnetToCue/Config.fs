module CueFSharp.DotnetToCue.Config

open System
open System.Reflection

open Reference


type DotnetTypes () = 
    member val Filter = fun (t: Type) -> true with get, set

type Dotnet() =
    member val Types = DotnetTypes () with get, set

type CueModule()=
    member val DomainNamer = defaultDomainNamer with get, set

type Cue() = 
    member val Module = CueModule() with get, set
    member val IgnoreClassMethods = true with get, set
    member val ReferenceTypesAsNullable = true with get, set

type Write () = 
    member val RootModule = "" with get, set
    member val RootDir = "" with get, set

type Config () = 
    member val Dotnet = Dotnet () with get, set
    member val Cue = Cue () with get, set
    member val Write = Write() with get, set
