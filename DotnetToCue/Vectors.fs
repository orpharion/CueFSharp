module CueFSharp.DotnetToCue.Vectors

open System
open System.Collections

open CueFSharp.Cue.Ast
open Ast
open IRegister

// this is used by Enum{System.Collections.ObjectModel.ReadOnlyCollection`1[GsaAPI.Double6]}
let isLiteral (t: Type) =
    t.FullName.StartsWith "System.Collections.ObjectModel.ReadOnlyCollection`1"

let isDict (t: Type) =
    Array.contains (typedefof<IDictionary>) (t.GetInterfaces())
    || t = typeof<IDictionary>

let isList (t: Type) =
    let interfaces = t.GetInterfaces()
    let interfaceNames = interfaces |> Array.map (fun I -> I.FullName)
    t.GetInterfaces() |> Array.exists (fun I -> I.FullName = "System.Collections.IEnumerable")

    // Array.contains (typeof<IEnumerable>) interfaces || Array.contains (typeof<IList>) interfaces

// Since CUE structs only support string labels, whereas C# implements dictionaries as a hash table. int32 hashable / etc, we todo
let Dict (reg: IRegistry) (t: ContextualType) =
    // todo determine if this is sufficient for identifying contained type.
    let keyValueTypes =
        if t.Type.GenericTypeArguments.Length = 2 then
            Some(t.Type.GenericTypeArguments.[0], t.Type.GenericTypeArguments.[1])
        else
            None
    let (keyExpr, valueExpr) =
        match keyValueTypes with
        | Some (k, v) ->
            (reg.TypeContextual(
                { ContextualType.Type = k
                  Context = t.Context }
             ),
             reg.TypeContextual(
                 { ContextualType.Type = v
                   Context = t.Context }
             ))
        | None ->
            (Ident.New "string" :> IExpr, Ident.New "_" :> IExpr)


    let keyExpr =
        { Alias.Ident = None;
         Expr = keyExpr;
         Comments = None
        }
        // { ListLit.Elts =
        //       [| { Ellipsis.Type = Some(keyExpr)
        //            Comments = None }
        //          :> IExpr |]
        //   Comments = None }

    { StructLit.Elts =
          [| { Field.Label = keyExpr :> ILabel
               Value = valueExpr
               Comments = None
               Optional = false
               Attrs = [||] }
             :> IDecl |]
      Comments = None }


let List (reg: IRegistry) (t: ContextualType) =
    // todo determine if this is sufficient for identifying contained type.
    let eltType =
        if t.Type.GenericTypeArguments.Length = 1 then
            Some(t.Type.GenericTypeArguments.[0])
        elif t.Type.HasElementType then
            Some(t.Type.GetElementType())
        else
            None
    // pass the context through
    let eltExpr =
        match eltType with
        | Some e ->
            reg.TypeContextual(
                { ContextualType.Type = e
                  Context = t.Context }
            )
        | None -> NewBottomLit $"unknown list element type" :> IExpr

    { ListLit.Elts =
          [| { Ellipsis.Type = Some(eltExpr)
               Comments = None }
             :> IExpr |]
      Comments = None }


let Vector (reg: IRegistry) (t: ContextualType) =
    if isDict t.Type then
        Some(Dict reg t :> IExpr)
    elif isList t.Type then
        Some(List reg t :> IExpr) // todo
    else
        None
