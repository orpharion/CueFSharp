/// It is a fundamental assumption that type.FullPath -> Type is functional:
/// at worst, a many->1 mapping.
/// todo(ado): unsure if FullName will be unique under different
/// combinations of <Namespace>.<[...Parent Classes]>.<Type>. Asumming true.
/// todo: Consider many->1 case.
module CueFSharp.DotnetToCue.Register

open System
open System.IO
open System.Reflection
open System.Collections.Generic

open CueFSharp.Cue.Ast
open Ast
open IRegister
open Type
open Scalars
open Module
open Config

type CueExpression (expr: Object) =
    inherit System.Attribute()
    member this.Expr = cast(expr)(expr.GetType())

let FindOrNew (key: 'K) (dict: Dictionary<'K, 'V>) (builder: unit -> 'V) =
    let (found, valueFound) = dict.TryGetValue key

    if found then
        valueFound
    else
        let value = builder ()
        dict.Add(key, value)
        value



type DotnetTypeFullName = string
type References = Dictionary<DotnetTypeFullName, AbsoluteValueIdent>

// The registry records and caches
type Registry =
    { Modules: Modules
      References: References
      Config: Config }

    member private r.TypeReferencer =
        Reference.tryReference r.Config.Cue.Module.DomainNamer

    static member New(cfg: Option<Config>) =
        { Modules = new Modules()
          References = new References()
          Config =
              match cfg with
              | Some c -> c
              | None -> Config() }

    member private r.TryFindReference(t: Type) =
        match t.FullName with
        | null -> None
        | f ->
            let found, value = r.References.TryGetValue f
            if found then Some(value) else None


    member private r.AddReference (fullName: string) (ref: AbsoluteValueIdent) = r.References.Add(fullName, ref)


    member private r.FindFile(ref: AbsoluteValueIdent) =
        let pkgs =
            FindOrNew ref.Module r.Modules (fun () -> new Packages())

        let pkg =
            FindOrNew
                ref.Package
                pkgs
                (fun () ->
                    { PackageSource.Ident = ref.Package
                      Files = new Files() })

        let filepath = ref.ToFilePath(None)

        let file =
            FindOrNew
                filepath
                pkg.Files
                (fun () ->
                    { File.Filename = filepath
                      Preamble =
                          { Preamble.Comments = None
                            Attributes = [||]
                            Package =
                                { Package.Name = ref.Package.Name
                                  Comments = None }
                            ImportDecl =
                                { ImportDecl.Specs = new ImportSpecs()
                                  Comments = None } }
                      Decls = new Decls() })

        file

    member r.AddExpr (ref: AbsoluteValueIdent) (expr: IExpr) =
        let file = r.FindFile ref
        file.Decls.Add(ref.ToLocalExpr expr)
        
    member r.GetExprFromAttribute(attrs: IEnumerable<CustomAttributeData>) =
        let name = typeof<CueExpression>.FullName
        attrs
        |> Seq.tryFind(fun attr -> attr.AttributeType.FullName = name)
        |> fun attr ->
            match attr with
            | Some a ->
                match Seq.tryHead a.ConstructorArguments with
                | Some arg -> Some(arg.Value :?> string |> Ident.New)
                | None -> None
            | None -> None

    member r.TypeContextual(t: ContextualType) =
        // no need to add builtin primitive identities, so always by-value.
        if isPrimitive t.Type then
            Kind t.Type
        else if isNullable t.Type && isPrimitive t.Type then
            Kind (unwrapNullable t.Type)
        else
            let defReference =
                match r.TryFindReference t.Type with
                // The type reference is already registered.
                | Some dRef -> Some(dRef)
                | None ->
                    match r.TypeReferencer t.Type with
                    // The type is referenceable, so register it in its own defining context and parse it.
                    | Some dRef ->
                        r.AddReference t.Type.FullName dRef
                        match r.GetExprFromAttribute t.Type.CustomAttributes with
                        | Some expr ->
                            expr :> IExpr
                        | None ->
                            {
                              ContextualType.Type = t.Type
                              Context = dRef
                            }
                            |> NewExpr r
                        |> r.AddExpr dRef

                        Some(dRef)
                    // The type is unreferenceable.
                    | None -> None

            match defReference with
            | Some dRef ->
                let file = r.FindFile t.Context
                // Get the identifer and import relative to the referenced context.
                let (ident, import) = dRef.ToIdentRelativeTo t.Context

                // Add the import, if necessary
                match import with
                | Some i -> ignore (file.Preamble.ImportDecl.Specs.TryAdd(i.Path.Value, i))
                | None -> ()

                ident
            // Return the expression itself.
            | None -> NewExpr r t
    // Add type value in appropriate source file
    member r.Type(t: Type) =
        r.TypeContextual
            { ContextualType.Type = t
              Context =
                  match r.TypeReferencer t with
                  | Some dRef -> dRef
                  | None -> failwith "failed to determine type reference." }

    member r.Assembly (asm: Assembly) =
        asm.GetTypes() |> Array.filter r.Config.Dotnet.Types.Filter |> Array.map r.Type

    member r.Write () =
        // todo write cue.mod in all pkgs
 
        let cueMod = NewRoot r.Config.Write.RootModule
        cueMod.Write(r.Config.Write.RootDir) r.Config.Write.RootModule
                

        for KeyValue (mdl, pkgs) in r.Modules do
            let mdlDir = 
                if mdl = r.Config.Write.RootModule then
                    r.Config.Write.RootDir
                else
                    Path.Join(r.Config.Write.RootDir, "cue.mod", "gen")
            (NewRoot mdl).Write (mdlDir) r.Config.Write.RootModule
            for KeyValue (_, pkg) in pkgs do
                for KeyValue (_, (file: File)) in pkg.Files do
                    file.Write(mdlDir) r.Config.Write.RootModule    


    interface IRegistry with
        member r.Config = r.Config
        member r.GetExprFromAttribute t = r.GetExprFromAttribute t
        member r.Type t = r.Type t
        member r.TypeContextual t = r.TypeContextual t
        member r.AddReference fullName ref = r.AddReference fullName ref
        member r.AddExpr ref expr = r.AddExpr ref expr
