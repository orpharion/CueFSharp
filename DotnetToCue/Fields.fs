module CueFSharp.DotnetToCue.Fields

open System.Reflection

open CueFSharp.Cue.Ast
open CueFSharp.DotnetToCue.Scalars
open Ast
open IRegister
open CueFSharp.Cue.Token

let FieldDecl (reg: IRegistry) (m: MemberInfo) (ctx: AbsoluteValueIdent) =

    let t =
        match m with
        | :? FieldInfo as fi -> fi.FieldType
        | :? PropertyInfo as pi -> pi.PropertyType
        | _ -> failwith $"Unexpected MemberInfo: {m.GetType}"
        
    let f =
        match reg.GetExprFromAlias(m) with
        | Some expr -> expr
        | None ->
            reg.TypeContextual
                {
                  ContextualType.Type = unwrapNullable t
                  Context = ctx
                }
        |> fun expr ->
            match m.GetCustomAttributesData() |> Seq.tryFind(fun attr -> attr.AttributeType.Name = "DefaultValueAttribute") with
            | Some defaultAttr ->
                let value = defaultAttr.ConstructorArguments.[0].Value
                let valueExpr = cast(value)(value.GetType());
                $"*{valueExpr.Print(0)} | {expr.Print(0)}" |> Ident.New :> IExpr
            | _ -> expr
        |> Field.New m.Name


    if reg.Config.Cue.NullableFieldAsUnion && isNullable t then
        Some(
            { f with
                  Value =
                      { BinaryExpr.X = f.Value
                        Op = OR
                        Y = BasicLit.NewNull()
                        Comments = None } } 
        )
    else
        Some({ f with Optional = isNullable t })

/// Field values are always referenced by label.
/// todo(ado):
/// - access and mutability
/// - reference vs. value extremely sloppy atm.
/// - optional - i.e. not required to be present (as opposed to be present and null)
/// todo - check that all pi.Name are valid cue idents. Should be the case.
let Field (reg: IRegistry) (fi: FieldInfo) (ctx: AbsoluteValueIdent) =
    if fi.IsPublic && not fi.IsStatic then
        FieldDecl reg fi ctx
    else
        None

let Fields (reg: IRegistry) (fis: FieldInfo []) (ctx: AbsoluteValueIdent) =
    fis |> Array.choose (fun fi -> Field reg fi ctx)

// todo - CanWrite properties?
let Property (reg: IRegistry) (pi: PropertyInfo) (ctx: AbsoluteValueIdent) =
    let isStatic =
        pi.GetAccessors()
        |> Seq.tryFind (fun m -> m.IsStatic)
        |> Option.isSome

    if pi.CanRead && not isStatic then
        FieldDecl reg pi ctx
    else
        None

let Properties (reg: IRegistry) (pis: PropertyInfo []) (ctx: AbsoluteValueIdent) =
    pis
    |> Array.choose (fun pi -> Property reg pi ctx)
