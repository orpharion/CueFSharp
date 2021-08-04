module CueFSharp.DotnetToCue.IRegister

open System
open CueFSharp.Cue.Ast
open Ast

// For composite literals, there is no path to return. 
// todo not sure if this is used now.
type Node = 
    | Value of IExpr
    // A reference can only be resolved into an expression once it's scope has been determined, so 
    // we pass the absolute position so it can be resolved.
    | Reference of AbsoluteValueIdent
    with member t.ToExprFrom (ctx: AbsoluteValueIdent) = ""

type ContextualType = 
    {
        Type: Type
        Context: AbsoluteValueIdent
    }

type IRegistry = 
   abstract member TypeContextual: ContextualType -> IExpr
   abstract member Type: Type -> IExpr
   abstract member AddReference: string -> AbsoluteValueIdent -> unit
   abstract member AddExpr: AbsoluteValueIdent -> IExpr -> unit