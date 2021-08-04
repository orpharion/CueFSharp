module CueFSharp.DotnetToCue.Util

let inline tryUnbox<'a> (x: obj) =
    match x with
    | :? 'a as result -> Some(result)
    | _ -> None

let inline filterSomeTo< ^a, ^b> (f: ^a -> ^b) (s: ^a Option seq)  =
    s
    |> Seq.collect
        (fun o ->
            match o with
            | Some v -> seq { f v }
            | None -> Seq.empty)

