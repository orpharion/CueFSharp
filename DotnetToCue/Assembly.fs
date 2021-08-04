module CueFSharp.DotnetToCue.Assembly

open System
open System.Reflection
open System.IO
open System.Runtime.InteropServices

/// load an Assembly into a MetadataLoadContext.
let load (dll: string) =
    let dir = Path.GetDirectoryName dll

    [ dir
      (Environment.GetEnvironmentVariable("PATH")) ]
    |> String.concat (Path.PathSeparator |> string)
    |> fun p -> Environment.SetEnvironmentVariable("PATH", p)

    let assemblyPaths =
        let rtd = RuntimeEnvironment.GetRuntimeDirectory()
        let runtimeAssemblies = Directory.GetFiles(rtd, "*.dll")
        Array.append runtimeAssemblies [| dll |] // todo is this necessary?

    let resolver = PathAssemblyResolver(assemblyPaths)
    let mlc = new MetadataLoadContext(resolver)
    mlc.LoadFromAssemblyPath(dll)
