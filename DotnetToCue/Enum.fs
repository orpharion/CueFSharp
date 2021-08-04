module CueFSharp.DotnetToCue.Enum

open System
open System.Reflection

open CueFSharp.Cue.Ast
open CueFSharp.DotnetToCue.Ast
open IRegister
open Scalars

// Enums fields are mapped as definitions, rather than values.
// Enums are value types, and we don't yet consider nullable value types.
// todo(ado): non-scalar element types!!
let Enum (reg: IRegistry) (t: ContextualType) =
    // todo(ado): bases
    // todo(ado): check references updated correctly.
    let cast o = cast o (t.Type.GetEnumUnderlyingType())

    // todo this is black magic here.
    let literals =
        Array.filter (fun (f: FieldInfo) -> f.IsLiteral) (t.Type.GetFields())

    let fieldDecls =
        literals
        |> Array.map (fun (f: FieldInfo) -> Field.New f.Name (cast (f.GetRawConstantValue())) :> IDecl)

    let enumMap =
        { StructLit.Elts = fieldDecls
          Comments = None }

    /// this is rather hacky
    let enumMapName = Array.last t.Context.Value.PathLabels + "Enum"

    let ref =
        { AbsoluteValueIdent.Module = t.Context.Module
          Package = t.Context.Package
          Value =
              { ValueIdent.PathLabels =
                    Array.append t.Context.Value.PathLabels.[..t.Context.Value.PathLabels.Length - 2] [| enumMapName |] } }

    let vIdent = Ident.New "v"

    let enum =
        CallExpr.New
            (Ident.New "or")
            ([| { ListLit.Elts =
                      [| { Comprehension.Clauses =
                               [| { ForClause.Key = None
                                    Value = Some(vIdent)
                                    Source = ref.Value.ToExpr None
                                    Comments = None } |]
                           Comments = None
                           Value =
                               { StructLit.Elts = [| vIdent :> IDecl |]
                                 Comments = None } } |]
                  Comments = None }
                :> IExpr |])

    reg.AddReference(t.Type.FullName + "Enum") ref
    reg.AddExpr ref (enumMap :> IExpr)
    enum

let isEnum (t: Type) =
    // Must inherit from System.Enum
    // TODO possible inheritance chain
    String.Equals(t.BaseType.FullName, "System.Enum", StringComparison.InvariantCulture)
