module CueFSharp.DotnetToCue.Scalars

open System

open CueFSharp.Cue.Ast
open Ast

// is checks if Type is a CUE scalar (as opposed to an F# primitive).
let isPrimitive (t: Type) =
    t.IsPrimitive
    || Array.contains t.FullName [| "System.String"; "System.Void"|]

let isNullable (t: Type) =
    match t with
    | null -> false
    | _ ->
        match t.GenericTypeArguments with
        | null -> false
        | _ -> not t.IsGenericTypeDefinition && t.GenericTypeArguments.Length > 0 && t.Name.Contains("Nullable")
    
let unwrapNullable (t: Type) =
    match isNullable t with
    | true -> t.GenericTypeArguments.[0]
    | false -> t

type Literal =
    | Bool of bool
    // | Bottom of Exception
    | Float of float
    | Int of int
    | Null of Object // todo
    | String of string

let literal value =
    match value with
    | Bool v -> BasicLit.NewBool v
    | Float v -> BasicLit.NewFloat v
    | Int v -> BasicLit.NewInt v
    | Null v -> BasicLit.NewNull()
    | String v -> BasicLit.NewString v

let Kinds =
    Map(
        [ "System.Boolean", Ident.New "bool" :> IExpr
          "System.Double", Ident.New "float" :> IExpr
          "System.Int32", Ident.New "int32" :> IExpr
          "System.String", Ident.New "string"  :> IExpr
          "System.Void", BasicLit.NewNull() :> IExpr]
    )

let Kind (t: Type) =
    let k = Kinds.TryFind t.FullName

    match k with
    | Some s -> s
    | None -> NewBottomLit(sprintf "Kind %s not understood" t.FullName) :> IExpr

// // todo: cast will panic downcast fails.
let cast (o: Object) (t: Type) =
    match t.FullName with
    | "System.Boolean" ->
        let b = o :?> bool
        BasicLit.NewBool b :> IExpr
    | "System.Double" -> BasicLit.NewFloat(o :?> float) :> IExpr
    | "System.Int32" -> BasicLit.NewInt(o :?> int32) :> IExpr
    | "System.String" -> BasicLit.NewString(o :?> string) :> IExpr
    | _ -> NewBottomLit "Unsupported literal!" :> IExpr
