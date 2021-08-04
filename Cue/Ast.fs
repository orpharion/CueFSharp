/// See cuelang.org/go/cue/ast
module rec CueFSharp.Cue.Ast

open CueFSharp.Cue.Format

type private Token = CueFSharp.Cue.Token.Token

type INode =
    abstract member Print : int -> string
    abstract member Comments : OComments

type IDecl =
    inherit INode
    abstract member DeclNode : unit

type IExpr =
    inherit INode
    inherit IDecl
    abstract member ExprNode : unit

type ILabel =
    inherit INode
    abstract member LabelNode : unit

type IClause =
    inherit INode
    abstract member ClauseNode : unit

// omitting Clause

// Comments

type Comments =
    { Groups: CommentGroup [] }
    interface INode with
        member this.Comments = Some(this)

        member this.Print(level: int) =
            join level (this.Groups |> Array.map (fun c -> c :> INode))

type Comment =
    { Text: string }
    interface INode with
        member this.Comments = None
        member this.Print(level: int) = indent level + "// " + this.Text

type private OComments = Option<Comments>

type CommentGroup =
    { Doc: bool
      List: Comment [] }
    interface INode with
        member this.Comments = None

        member this.Print(level: int) =
            join level (this.List |> Array.map (fun c -> c :> INode))

    interface IDecl with
        member this.DeclNode = ()

// let getComments (n: ^T when ^T: (member Comments: Comments)) = (n.Comments :> INode).CommentInfo

type Attribute =
    { Text: string
      Comments: OComments }
    interface IDecl with
        member this.Print(level: int) = $"@{this.Text}"
        member this.DeclNode = ()
        member this.Comments = this.Comments


type Field =
    { Label: ILabel
      Optional: bool
      Value: IExpr // TODO No TokenPos: Value must be an StructLit with one field.
      Attrs: Attribute []
      Comments: OComments }
    interface IDecl with
        member this.Print(level: int) =
            let f =
                match this.Label with
                | :? Alias as a -> $"[{(a :> INode).Print level}]"
                | _ -> this.Label.Print level

            $"""{f}{if this.Optional then "?" else ""}: {this.Value.Print level}"""

        member this.DeclNode = ()
        member this.Comments = this.Comments

type Alias =
    { Ident: Option<Ident>
      Expr: IExpr
      Comments: OComments }
    interface INode with
        member this.Print(level: int) =
            (match this.Ident with
             | Some i -> (i :> INode).Print level + "="
             | None -> "")
            + this.Expr.Print level

        member this.Comments = this.Comments

    interface IExpr with
        member this.ExprNode = ()
        member this.DeclNode = ()

    interface ILabel with
        member this.LabelNode = ()

type Comprehension =
    { Clauses: IClause []
      Value: StructLit
      Comments: OComments }
    interface IExpr with
        member this.ExprNode = ()

    interface IDecl with
        member this.Print(level: int) =
            let c =
                join level (this.Clauses |> Array.map (fun c -> c :> INode))

            $"{c} {(this.Value :> INode).Print level}"

        member this.DeclNode = ()
        member this.Comments = this.Comments

type BottomLit =
    { Comments: OComments }
    interface IExpr with
        member this.Print(level: int) = "_|_"
        member this.DeclNode = ()
        member this.ExprNode = ()
        member this.Comments = this.Comments

type Ident =
    { Name: string
      // todo scope and node?
      Comments: OComments }
    interface INode with
        member this.Print(level: int) = this.Name
        member this.Comments = this.Comments

    interface ILabel with
        member this.LabelNode = ()

    interface IExpr with
        member this.DeclNode = ()
        member this.ExprNode = ()

type BasicLit =
    { Kind: Token
      Value: string
      Comments: OComments }
    interface INode with
        // todo proper quote checking.
        member this.Print(level: int) =
            if this.Kind = Token.STRING then
                $"\"{this.Value}\""
            else
                this.Value

        member this.Comments = this.Comments

    interface IExpr with
        member this.DeclNode = ()
        member this.ExprNode = ()

    interface ILabel with
        member this.LabelNode = ()

    // todo quotes
    static member NewString(str: string) =
        { Kind = Token.STRING
          Value = str
          Comments = None }

    static member NewNull() =
        { Kind = Token.NULL
          Value = "null"
          Comments = None }

    static member NewLit (tok: Token) (s: string) =
        { Kind = tok
          Value = s
          Comments = None }

    static member NewBool(b: bool) =
        if b then
            { Kind = Token.TRUE
              Value = "true"
              Comments = None }
        else
            { Kind = Token.FALSE
              Value = "false"
              Comments = None }

// omitting Interpolation

type StructLitEltInArray =
    | EltDecl of IDecl
    | EltLabelString of string
    | EltToken of Token
    | EltEmbedding of EmbedDecl

type StructLit =
    { Elts: IDecl []
      Comments: OComments }
    interface IExpr with
        member this.Print(level: int) =
            $"{{{join level (this.Elts |> Array.map (fun c -> c :> INode))}}}"

        member this.Comments = this.Comments
        member this.DeclNode = ()
        member this.ExprNode = ()

    /// See New
    static member private CaptureOptional(fields: StructLitEltInArray []) =
        match fields.[0] with
        | EltToken t ->
            match t with
            | Token.OPTION -> true, fields.[1..]
            | _ -> false, fields
        | _ -> false, fields
    /// See New
    static member private CaptureExpr (label: ILabel) (fields: StructLitEltInArray []) =
        let optional, fieldsRem =
            match fields.[0] with
            | EltToken t ->
                match t with
                | Token.OPTION -> true, fields.[1..]
                | _ -> failwith (sprintf "invalid token %s" (t.GetType().Name))
            | _ -> false, fields

        let fieldsRem =
            match fieldsRem.[0] with
            | EltToken t ->
                match t with
                | Token.COLON -> fieldsRem.[1..]
                | _ -> fieldsRem
            | _ -> fieldsRem

        let expr =
            match fieldsRem.[0] with
            | EltDecl d ->
                match d with
                | :? IExpr as e -> e
                | _ -> failwith (sprintf "unsupported expression type %s" (d.GetType().Name))
            | _ -> failwith (sprintf "label not matched with expression")

        ({ Field.Label = label
           Value = expr
           Comments = None
           Optional = optional
           Attrs = [||] },
         fieldsRem.[1..])
    /// See New
    static member private CaptureElt(fields: StructLitEltInArray []) =
        match fields.[0] with
        | EltDecl d ->
            let df = (d, fields.[1..])

            match d with
            | :? Field -> df
            | :? CommentGroup -> df
            | :? Ellipsis -> df
            // | :? LetClause -> (sd, ())

            | :? ILabel as l ->
                StructLit.CaptureExpr l fields.[1..]
                |> fun (fld, flds) -> (fld :> IDecl, flds)
            | _ -> failwith (sprintf "unsupported label type %s" (d.GetType().Name))
        | EltEmbedding e -> e :> IDecl, fields.[1..]
        | EltLabelString s ->
            StructLit.CaptureExpr(Ident.New s) fields.[1..]
            |> fun (fld, flds) -> (fld :> IDecl, flds)
        | _ -> failwith (sprintf "unsupported label type %s" (fields.[0].GetType().Name))

    /// New StructLit from an array of components, which include both valid elements runs of ILabel, [Token.OPTION,] [Token.COLON,] IExpr.
    /// Panics on error.
    /// Token.ISA (::) is unsupported. See cuelang.org/go/internal/core/compile/compile.go@[569:].
    static member New(fields: StructLitEltInArray []) =
        let mutable fieldsRem = fields
        let mutable decls = [||]

        while fieldsRem.Length > 0 do
            let (decl, _fieldsRem) = StructLit.CaptureElt fieldsRem
            fieldsRem <- _fieldsRem
            decls <- Array.append decls [| decl |]

        { StructLit.Elts = decls
          Comments = None }

let Embed (x: IExpr) =
    Embedding { EmbedDecl.Expr = x; Comments = None }

type Embedding = Embedding of EmbedDecl


type ListLit =
    { Elts: IExpr []
      Comments: OComments }
    interface INode with
        member this.Print(level: int) =
            $"{[ join level (this.Elts |> Array.map (fun c -> c :> INode)) ]}"

        member this.Comments = this.Comments

    interface ILabel with
        member this.LabelNode = ()

    interface IExpr with
        member this.DeclNode = ()
        member this.ExprNode = ()

type Ellipsis =
    { Type: Option<IExpr>
      Comments: OComments }
    interface INode with
        member this.Print(level: int) =
            "..."
            + match this.Type with
              | None -> ""
              | Some e -> e.Print level

        member this.Comments = this.Comments

    interface IExpr with
        member this.DeclNode = ()
        member this.ExprNode = ()

type ForClause =
    { Key: Option<Ident>
      Value: Option<Ident>
      Source: IExpr
      Comments: OComments }
    interface IClause with
        member this.Print(level: int) =
            let k =
                match this.Key with
                | Some (i) -> (i :> INode).Print(level)
                | None -> "_"

            let v =
                match this.Value with
                | Some (i) -> (i :> INode).Print(level)
                | None -> "_"

            $"for {k}, {v} in {this.Source.Print(level)}"

        member this.Comments = this.Comments
        member this.ClauseNode = ()
// omitting IfClause
// omitting LetClause
// omitting ParenExpr

type SelectorExpr =
    { X: IExpr
      Sel: ILabel
      Comments: OComments }
    interface IExpr with
        member this.Print(level: int) =
            $"{this.X.Print level}.{this.Sel.Print level}"

        member this.Comments = this.Comments
        member this.DeclNode = ()
        member this.ExprNode = ()

    static member New (x: IExpr) (sel: string []) : IExpr =
        let rec loop x sel : IExpr =
            match sel with
            | [||] -> x
            | _ ->
                (loop
                    { X = x
                      Sel = Ident.New(sel.[0])
                      Comments = None }
                    sel.[1..])

        loop x sel // todo use string[]

// omitting IndexExpr
// omitting SliceExpr
type CallExpr =
    { Fun: IExpr
      Args: IExpr []
      Comments: OComments }
    interface IExpr with
        member this.Print(level: int) =
            $"{(this.Fun :> INode).Print level}({join level this.Args})"

        member this.Comments = this.Comments
        member this.ExprNode = ()
        member this.DeclNode = ()

    static member New (func: IExpr) (args: IExpr []) =
        { CallExpr.Fun = func
          Args = args
          Comments = None }
// omitting UnaryExpr
type BinaryExpr =
    { X: IExpr
      Op: Token
      Y: IExpr
      Comments: OComments }
    interface IExpr with
        member this.Comments = this.Comments

        member this.Print(level: int) =
            $"({(this.X :> INode).Print level} {this.Op.ToString()} {(this.Y :> INode).Print level})"

        member this.DeclNode = ()
        member this.ExprNode = ()


type Ident with
    static member New(name: string) = { Name = name; Comments = None }

// Declarations

type ImportSpec =
    { Name: Ident
      Path: BasicLit
      Comments: OComments }
    interface INode with
        member this.Print(level: int) =
            $"{(this.Name :> INode).Print level} {(this.Path :> INode).Print level}"

        member this.Comments = this.Comments

    static member New (name: Ident) (importPath: string) =
        // todo literal.string.quote.
        { Name = name
          Path = BasicLit.NewString importPath
          Comments = None }

// omitting BadDecl

type ImportDecl =
    { Specs: ImportSpec []
      Comments: OComments }
    interface IDecl with
        member this.Print(level: int) =
            match this.Specs with
            | [||] -> ""
            | _ ->
                let c =
                    join (level + 1) (this.Specs |> Array.map (fun c -> c :> INode))

                $"import ({c})"

        member this.Comments = this.Comments
        member this.DeclNode = ()

type ISpec =
    inherit INode
    abstract SpecNode : unit

type EmbedDecl =
    { Expr: IExpr
      Comments: OComments }
    interface IDecl with
        member this.Print(level: int) = this.Expr.Print level
        member this.Comments = this.Comments
        member this.DeclNode = ()

// Files and packages

type File =
    { Filename: string
      Decls: IDecl []
      Imports: ImportSpec []
      // omitting Unresolved
      Comments: OComments }
    interface INode with
        member this.Print(level: int) =
            $"{join level (this.Imports |> Array.map (fun c -> c :> INode))}"
            + $"{join level (this.Decls |> Array.map (fun c -> c :> INode))}" // todo fix package order.

        member this.Comments = this.Comments

type Package =
    { Name: Ident
      Comments: OComments }
    interface IDecl with
        member this.Print(level: int) =
            $"package {(this.Name :> INode).Print level}"

        member this.Comments = this.Comments
        member this.DeclNode = ()
