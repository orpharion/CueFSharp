module CueFSharp.DotnetToCue.Fields

open System.Reflection

open CueFSharp.Cue.Ast
open Ast
open IRegister

/// Field values are always referenced by label.
/// todo(ado):
/// - access and mutability
/// - reference vs. value extremely sloppy atm.
/// - optional - i.e. not required to be present (as opposed to be present and null)
/// todo - check that all pi.Name are valid cue idents. Should be the case.
let Field (reg: IRegistry) (fi: FieldInfo) (ctx: AbsoluteValueIdent) =
    if fi.IsPublic then

        let n =
            (reg.TypeContextual
                { ContextualType.Type = fi.FieldType
                  Context = ctx })

        Some(Field.New fi.Name n)
    else
        None

let Fields (reg: IRegistry) (fis: FieldInfo []) (ctx: AbsoluteValueIdent) =
    fis |> Array.choose (fun fi -> Field reg fi ctx)

// todo - CanWrite properties?
let Property (reg: IRegistry) (pi: PropertyInfo) (ctx: AbsoluteValueIdent) =
    if pi.CanRead then
        let n =
            (reg.TypeContextual
                { ContextualType.Type = pi.PropertyType
                  Context = ctx })

        Some(CueFSharp.Cue.Ast.Field.New pi.Name n)
    else
        None

let Properties (reg: IRegistry) (pis: PropertyInfo []) (ctx: AbsoluteValueIdent) =
    pis
    |> Array.choose (fun pi -> Property reg pi ctx)
