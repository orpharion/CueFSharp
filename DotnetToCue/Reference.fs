module CueFSharp.DotnetToCue.Reference

open System
open System.Reflection

open CueFSharp.Cue.Ast
open Ast

let Definition (name: string) = "#" + name // todo move somewhere else?

let attributeIsCompany (attr: CustomAttributeData) = 
    attr.AttributeType.FullName = "System.Reflection.AssemblyCompanyAttribute"

let companyName (a: Assembly) = 
    (a.GetCustomAttributesData() |> Seq.cast |> Seq.find attributeIsCompany).ConstructorArguments.[0].Value.ToString()

let defaultDomainName (company: string) (assemblyName: string) (version: string) =
    (match company with
     | "" -> assemblyName + ".com"
     | _ -> company + ".com")
        .ToLower()
        .Replace(" ", "-")
    + $"/{assemblyName}/v{version}"

// domain determines the CUE namespace from the assembly.
// We determine the module domain from the AssemblyCompanyAttribute.
let defaultDomainNamer (a: Assembly) =
    defaultDomainName
        (companyName a)
        (a.GetName().Name)
        (a.GetName().Version.ToString())

// Path is interpreted according to C# namespace rules.
// So far, we are only concerned with paths to Types, so it is assumed the expression is a #Definition.
// todo - what to do with version?
let tryReference (d: (Assembly -> string)) (t: Type) =
    match t.FullName with
    | null -> None
    | f ->
        if not (t.FullName.StartsWith t.Namespace) then
            failwith (
                sprintf
                    "tryReference(t: Type = %O ): not (t.FullName = %s).StartsWith (t.Namespace = %s)"
                    t
                    t.FullName
                    t.Namespace
            )

        let mdl = d (t.Assembly) // todo(ado): cache
        let pkgDirs = t.Namespace.Split "."

        let pkg =
            { PackageIdent.Directory = pkgDirs
              Name = Ident.New(Array.last pkgDirs) }


        let selectors =
            let e = t.FullName.[t.Namespace.Length + 1..].Replace("[]", "Array") // Hacky fix for C# Array types

            match e.Length with
            | 0 -> Some(Array.empty)
            | _ ->
                if (Seq.filter ((=) '.') >> Seq.length) e <> 0 then
                    None
                else
                    let labels = e.Split "+"
                    // As we are only concerned with paths,
                    Some(labels |> Array.map Definition) // todo break this out into a function

        match selectors with
        | Some s ->
            Some(
                { AbsoluteValueIdent.Module = mdl
                  Package = pkg
                  Value = { PathLabels = s } }
            )
        | None -> None
