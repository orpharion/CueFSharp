/// Additional useful Cue AST structures.
/// todo: remove redundant identifiers
module CueFSharp.DotnetToCue.Ast

open System
open System.IO
open System.Collections.Generic
open CueFSharp.Cue.Ast
open CueFSharp.Cue.Token
open CueFSharp.Cue.Format

let ToINode a = a :> INode
let ToIDecl a = a :> IDecl
let ToIExpr a = a :> IExpr

type LabelStrings = string []

let NewBottomLit (msg: string) =
    { BottomLit.Comments =
          Some(
              { Comments.Groups =
                    [| { CommentGroup.Doc = false
                         List = [| { Comment.Text = msg } |] } |] }
          ) }

// assuming that float.ToString operates the same way.
type BasicLit with
    static member NewFloat(f: float) =
        { Kind = Token.FLOAT
          Value = f.ToString()
          Comments = None }

    static member NewInt(i: int) =
        { Kind = Token.INT
          Value = i.ToString()
          Comments = None }


// todo for these utilities, really should distinguish between labels and StringLiterals.
type Field with
    static member New (label: string) (value: IExpr) =
        { Label = Ident.New label
          Optional = false
          Value = value
          Comments = None
          Attrs = [||] }

type StructLit with
    /// NewUnary creates a unary (single field) struct.
    static member NewUnary (label: string) (value: IExpr) =
        { StructLit.Elts = [| Field.New label value :> IDecl |]
          Comments = None }

    /// NewFolded folds the labels into unary structs terminating in the value.
    static member NewFolded (labels: LabelStrings) (value: IExpr) =
        let rec loop (paths: LabelStrings) =
            match paths with
            | [||] -> failwith "can't create folded StructLit with no labels"
            | [| p |] -> StructLit.NewUnary p value
            | _ -> StructLit.NewUnary(Array.last paths) (loop paths.[..paths.Length - 2] :> IExpr)

        loop labels

let NewFoldedStructLitOrExpr (labels: LabelStrings) (value: IExpr) =
    match labels with
    | [||] -> value
    | _ -> StructLit.NewFolded labels value :> IExpr

type SelectorExpr with
    static member NewFrom (from: IExpr) (selectors: LabelStrings) =
        let rec loop (paths: LabelStrings) =
            match paths with
            | [||] -> failwith "can't create SelectorExpr with no selectors"
            | [| p |] ->
                { SelectorExpr.X = from
                  SelectorExpr.Sel = Ident.New p
                  Comments = None }
            | _ ->
                { SelectorExpr.X = loop paths.[..paths.Length - 2]
                  SelectorExpr.Sel = Ident.New(Array.last paths)
                  Comments = None }

        loop selectors

let NewSelectorExprOrIdentFrom (from: Option<IExpr>) (selectors: LabelStrings) =
    let root, sels =
        match from with
        | Some e -> e, selectors
        | None ->
            match selectors with
            | [||] -> failwith "can't create SelectorExpr or Ident with no selectors or root expression"
            | _ -> Ident.New selectors.[0] :> IExpr, selectors.[1..]

    match sels with
    | [||] -> root
    | _ -> SelectorExpr.NewFrom root sels :> IExpr

let tryToNodeWith (f: INode -> 'a) (d: 'a) a =
    match a with
    | None -> d
    | Some a -> f (a :> INode)
// Mutable file
type Decls = ResizeArray<IDecl>
type ImportSpecs = Dictionary<string, ImportSpec>

type ImportDecl =
    { Specs: ImportSpecs
      Comments: Option<Comments> }
    interface IDecl with
        member this.Print(level: int) =
            match this.Specs.Count with
            | 0 -> ""
            | _ ->
                "import ("
                + (join level (this.Specs.Values |> Seq.cast |> Seq.map ToINode))
                + ")"

        member this.Comments = this.Comments
        member this.DeclNode = ()

// We restructure the File type to make it easier to generate.
type Preamble =
    { Comments: Option<Comments>
      Attributes: Attribute []
      Package: Package
      ImportDecl: ImportDecl }
    member this.ToNodes() =
        seq {
            tryToNodeWith (fun a -> seq { a }) Seq.empty this.Comments
            this.Attributes |> Array.map ToINode |> Seq.cast

            (if this.Package.Name.Name <> "" then
                 seq { this.Package :> INode }
             else
                 Seq.empty)

            seq { this.ImportDecl :> INode }
        }
        |> Seq.concat

type File =
    { Filename: string
      Preamble: Preamble
      Decls: Decls }
    member this.Print() =
        seq {
            (this.Preamble.ToNodes())
            (this.Decls |> Seq.cast |> Seq.map ToINode)
        }
        |> Seq.concat
        |> join 0

    member this.Write (rootDir: string) (rootModule: string) =
        let content = this.Print()

        let filename =
            if rootModule <> ""
               && this.Filename.StartsWith rootModule then
                this.Filename.[rootModule.Length..]
            else
                this.Filename

        let target = System.IO.Path.Join(rootDir, filename)

        let pathSep = target.Split "/"

        let dir =
            String.Join("/", pathSep.[..pathSep.Length - 2])

        ignore (IO.Directory.CreateDirectory(dir))
        System.IO.File.WriteAllText(target, content)

let IdentEmpty = Ident.New("")
/// todo: CUE limitation on PathComponent excludes `:`
type PathComponent = string

/// Package identifier relative to a Module
type PackageIdent =
    { Directory: PathComponent []
      Name: Ident }

    override t.ToString() =
        String.Join("/", t.Directory)
        + (match Array.tryLast t.Directory with
           | Some (d) ->
               if d = t.Name.Name then
                   ""
               else
                   $":{t.Name.Name}"
           | None -> t.Name.Name)

let packageIdentEmpty = { Directory = [||]; Name = IdentEmpty }

type FilePath = string
type Files = Dictionary<FilePath, File>

type PackageSource =
    { Ident: PackageIdent
      Files: Dictionary<string, File> } // All files must have package clauses with this name to be valid.

type ModuleIdent = string

/// moduleIdentEmpty for builtins.
let moduleIdentEmpty = ""



/// Value ident relative to its package.
type ValueIdent =
    { PathLabels: LabelStrings }
    member t.ToExpr(from: Option<IExpr>) =
        match from with
        | Some (e) -> SelectorExpr.New e t.PathLabels
        | None ->
            match t.PathLabels with
            | [||] -> failwith "Cant identify value with no selectors from nowhere."
            | [| v |] -> Ident.New v :> IExpr
            | _ -> SelectorExpr.New(Ident.New t.PathLabels.[0] :> IExpr) t.PathLabels.[1..]

    override t.ToString() = t.ToExpr(None).ToString()

    member t.ToString(from: Option<IExpr>) = t.ToExpr(from).ToString()

let valueIdentEmpty = { PathLabels = [||] }
/// Absolute Value Ident
/// note: Module with package invalid.
type AbsoluteValueIdent =
    { Module: ModuleIdent
      Package: PackageIdent
      Value: ValueIdent }
    // todo I'm not sure this is ever actually used. Review.
    // Most functions don't actually work properly for builtins.
    static member NewBuiltinRoot(ident: string) =
        { Module = moduleIdentEmpty
          Package = packageIdentEmpty
          Value = { PathLabels = [| ident |] } }

    member t.AbsolutePackageIdentString =
        t.Module
        + (if t.Module <> moduleIdentEmpty
              && t.Package <> packageIdentEmpty then
               "/"
           else
               "")
        + t.Package.ToString()

    member t.AbsoluteIdentString =
        t.AbsolutePackageIdentString
        + (if t.Package <> packageIdentEmpty
              && t.Value <> valueIdentEmpty then
               "."
           else
               "")
        + t.Value.ToString()

    member t.ImportSpec =
        { ImportSpec.Name = t.Package.Name
          Path = BasicLit.NewString t.AbsolutePackageIdentString
          Comments = None }

    // todo override package identifier?
    member t.ImportedIdent =
        NewSelectorExprOrIdentFrom(Some(t.Package.Name :> IExpr)) t.Value.PathLabels


    // We don't implement CUE's scoping rules.
    // Instead, we always reference from the package root.
    // However, we still take the context as argument for possible future use.
    member private t.LocalRootIdent =
        NewSelectorExprOrIdentFrom None t.Value.PathLabels

    override t.ToString() = t.AbsoluteIdentString

    member t.IsBuiltin() = t.Module = moduleIdentEmpty

    member t.IsBuiltinPackage() =
        t.Module = moduleIdentEmpty
        && t.Package <> packageIdentEmpty

    member t.ToFilePath(filename: Option<string>) =
        if t.IsBuiltin() then
            failwith (sprintf "cannot determine filepath for builtin %O" t)

        let filename =
            (match filename with
             | Some (f) -> f
             | None -> t.Package.Name.Name)
            + ".cue"

        let dir =
            Path.Join(Array.append [| t.Module |] t.Package.Directory)

        Path.Join(dir, filename)

    // Create nested structs in this package if necessary
    // Not sure if this is accurate for CUE
    // todo this disallows root lists, which probably aren't necessary
    member t.ToLocalExpr(value: IExpr) =
        match t.Value.PathLabels with
        | [||] -> failwith "need at least one label!"
        | [| p |] -> Field.New p value :> IDecl
        | _ ->
            NewFoldedStructLitOrExpr t.Value.PathLabels.[1..] value
            |> Field.New t.Value.PathLabels.[0]
            :> IDecl

    // Returns the identifer and, if necessary, the import spec for it.
    member t.ToIdentRelativeTo(ctx: AbsoluteValueIdent) =
        if t.Module = ctx.Module && t.Package = ctx.Package then
            t.LocalRootIdent, None
        else
            t.ImportedIdent, Some(t.ImportSpec)

type Packages = Dictionary<PackageIdent, PackageSource>
type Modules = Dictionary<ModuleIdent, Packages>
