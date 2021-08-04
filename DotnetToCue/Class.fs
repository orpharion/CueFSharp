module CueFSharp.DotnetToCue.Class

open System

open CueFSharp.Cue.Ast
open CueFSharp.Cue.Token
open Ast
open IRegister
open Util
open Fields
open Methods

[<Literal>]
let SystemObjectName = "System.Object"

// Comparable type
[<CustomEqualityAttribute>]
[<CustomComparisonAttribute>]
type CType =
    { Type: Type }
    interface IComparable with
        member ct.CompareTo(obj: obj) =
            match (tryUnbox<Type> obj) with
            | Some oT ->
                match oT.FullName = ct.Type.FullName with
                | true -> 0
                | false -> 1
            | None -> 1

    override ct.Equals(obj: obj) =
        match (ct :> IComparable).CompareTo(obj) with
        | 0 -> true
        | 1 -> false
        | _ -> failwith "invalid CompareTo result."

    override ct.GetHashCode() = ct.Type.GetHashCode()

let rec private getBases (t: Type) =
    if String.Equals(t.BaseType.FullName, SystemObjectName, StringComparison.InvariantCulture) then
        Set.empty
    else
        Set.add { Type = t } (getBases (t.BaseType))

// All reference types are nullable
// Only public properties are exported.
let Class (reg: IRegistry) (t: ContextualType) =

    // TODO
    // let embedded = Bases p t
    let attrDecl =
        { Attribute.Text = $"dotnet({{FullName:{t.Type.FullName}}})"
          Comments = None }
        :> IDecl

    let fieldDecls =
        (Fields reg (t.Type.GetFields()) t.Context)
        |> Array.map ToIDecl

    let propDecls =
        (Properties reg (t.Type.GetProperties()) t.Context)
        |> Array.map ToIDecl

    let methodInfos = filter (t.Type)

    let methodDecls =
        Methods reg methodInfos t.Context |> Array.map ToIDecl

    { BinaryExpr.X =
          { StructLit.Elts =
                Array.concat (
                    seq {
                        [| attrDecl |]
                        fieldDecls
                        propDecls
                        methodDecls
                    }
                )
            Comments = None }
      Op = OR
      Y = BasicLit.NewNull()
      Comments = None }
