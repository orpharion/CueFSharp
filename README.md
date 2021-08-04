# Cue F#

The Cue F# library (CueFSharp.dll) implements (most of) the [CUE](https://cuelang.org) abstract syntax tree and transforms .NET types and assemblies (using reflection) to CUE sources.

It is designed to work with assemblies loaded in a [MetadataLoadContext](https://docs.microsoft.com/en-us/dotnet/api/system.reflection.metadataloadcontext?view=dotnet-plat-ext-5.0), but _should_ work with assemblies loaded normally, or for reflection.

The library is still very preliminary, and many .NET features are unsupported.
Raise issues and make requests if there's anything you need!

# Example usage

```fsharp
open System
open System.Reflection
open System.IO

open CueFSharp.DotnetToCue.Config
open CueFSharp.DotnetToCue.Assembly
open CueFSharp.DotnetToCue.Register
open CueFSharp.DotnetToCue.Reference

let TypeFilterer (t: Type) =
    t.FullName.StartsWith "Target"

/// CUE organises modules by domain. We override the default domain name generator. 
let domain (a: Assembly) =
    if a.GetName().Name = "Target" then
        defaultDomainName "target-renamed" (a.GetName().Name) (a.GetName().Version.ToString())
    else
        DotnetToCue.Reference.defaultDomainNamer (a)

[<Literal>]
let DllDir =
    "DllDirectory"

[<Literal>]
let DllName = "Target.dll"

[<EntryPoint>]
let main argv =
    let asm = load (Path.Combine(DllDir, DllName))
    let cfg = Config()
    cfg.Cue.Module.DomainNamer <- domain
    cfg.Dotnet.Types.Filter <- TypeFilterer
    cfg.Write.RootDir <- "dist"
    cfg.Write.RootModule <- domain asm
    let registry = Registry.New(Some(cfg))
    ignore (registry.Assembly asm)
    registry.Write ()
    0
```