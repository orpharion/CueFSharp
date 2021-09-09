module CueFSharp.DotnetToCue.Type

open System

open CueFSharp.Cue.Ast
open Ast
open IRegister
open Enum
open Vectors

// New parses an unencountered Type (as do all parsing functions except c.Type<...>).
// For now, we aren't parsing Type->CUE.Value and comparing to Register, but assuming <path, Type> pairs unique and
// parsing is invariant.
// Only c.Type should ever recurse to c.New, otherwise we may get stuck in cycles.
let NewExpr (reg: IRegistry) (t: ContextualType)  =
    if t.Type.FullName = "System.Reflection.MethodBase" then
        NewBottomLit ($"Exlcuding System.Reflection.MethodBase") :> IExpr
    elif t.Type.FullName = "System.Reflection.MethodInfo" then
        NewBottomLit ($"Exlcuding System.Reflection.MethodBase") :> IExpr
    else 
        match Vector reg t with
        | Some n -> n
        | None ->
            // https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/reference-types
            // do any other reference types need consideration?
            if t.Type.IsClass then
                Class.Class reg t
            elif t.Type.IsValueType then
                // https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/value-types
                if isEnum t.Type then
                    Enum reg t  :> IExpr
                    // NewBottomLit ($"Enum unsupported {t.Type.FullName}") :> IExpr
                else
                    NewBottomLit ($"Unknown .NET Value Type {t.Type.FullName}") :> IExpr
            else
                NewBottomLit ($"Unknown .NET Reference Type {t.Type.FullName}") :> IExpr

