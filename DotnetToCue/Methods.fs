module CueFSharp.DotnetToCue.Methods

open System
open System.Reflection

open IRegister
open CueFSharp.Cue.Ast
open CueFSharp.Cue.Token
open Util
open Ast

let expandPropertyNames (p: PropertyInfo) =
    Array.concat (
        seq {
            [| p.Name |]

            match p.GetGetMethod() with
            | null -> Array.empty
            | s -> [| s.Name |]

            match p.GetSetMethod() with
            | null -> Array.empty
            | s -> [| s.Name |]
        }
    )

/// We only export methods unassociated with properties.
let filterOutProperties (t: Type) =
    let p = t.GetProperties()
    let m = t.GetMethods()

    let propNames =
        Array.collect expandPropertyNames p |> Set.ofArray

    Array.filter (fun (m: MethodInfo) -> not (propNames.Contains m.Name)) m

/// Because we don't describe System.Object, we must also ignore its methods (which would otherwise
/// not be documentable.)
let filterOutObjectMethods (t: Type) (mis: MethodInfo []) =
    Array.filter
        (fun (m: MethodInfo) ->
            (m.DeclaringType.FullName //m.DeclaringType = t
             <> "System.Object" //typeof<System.Object>
             && m.DeclaringType.FullName <> "System.Exception")) //typeof<System.Exception>))
        mis

let filter (t: Type) =
    filterOutProperties t |> filterOutObjectMethods t

// todo decide what to do with system.void
let output (reg: IRegistry) (t: ContextualType) =
    let expr = reg.TypeContextual t
    let outField = Field.New "Out" expr

    match expr with
    | :? BasicLit as b ->
        match b.Kind with
        | Token.NULL -> None
        | _ -> Some(outField)
    | _ -> Some(outField)



let inputs (reg: IRegistry) (prms: ParameterInfo []) (ctx: AbsoluteValueIdent) =
    let inDecls =
        prms
        |> Array.map
            (fun prm ->
                (Field.New
                    prm.Name
                    (reg.TypeContextual
                        { ContextualType.Type = prm.ParameterType
                          Context = ctx })) :> IDecl)

    match inDecls.Length with
    | 0 -> None
    | _ ->
        Some(
            Field.New
                "In"
                ({ StructLit.Elts = inDecls
                   Comments = None })
        )

let method (reg: IRegistry) (m: MethodInfo) (ctx: AbsoluteValueIdent) =
    let elts =
        (seq {
            Some(Field.New "$id" (BasicLit.NewString "method"))

            inputs reg (m.GetParameters()) ctx

            output
                reg
                { ContextualType.Type = m.ReturnType
                  Context = ctx }
         })
        |> filterSomeTo ToIDecl
        |> Array.ofSeq

    { Field.Label = m.Name |> Ident.New
      Value =
          { StructLit.Elts = elts
            Comments = None }
      Comments = None
      Optional = true
      Attrs = [||] }

let Methods (reg: IRegistry) (mis: MethodInfo []) (ctx: AbsoluteValueIdent) =
    Array.map (fun m -> method reg m ctx) mis
