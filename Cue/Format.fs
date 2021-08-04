/// AST formatting utilities.
module CueFSharp.Cue.Format

open System

[<Literal>]
let private Indent = "    "

let indent (level: int) = String.replicate level Indent

[<Literal>]
let ElementSeparator = "\n"

let sep (level: int) = ElementSeparator + indent level 

let inline join< ^a when ^a : (member Print : int -> string)> (level: int) (values: ^a seq) =
    let sepl = sep level
    let f' v = (^a : (member Print : int -> string) (v, level + 1))
    sepl + String.Join (sepl, values |> Seq.map f')
