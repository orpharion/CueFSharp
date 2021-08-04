module CueFSharp.DotnetToCue.Module

open CueFSharp.Cue.Ast
open Ast

let NewRoot (domain: string) =
    let s =
        { StructLit.Elts = [| Field.New "module" (BasicLit.NewString domain :> IExpr) |]
          Comments = None }

    { File.Filename = $"{domain}/cue.mod/module.cue"
      Preamble =
          { Preamble.Comments = None
            Attributes = [||]
            Package =
                { Package.Name = Ident.New ""
                  Comments = None }
            ImportDecl =
                { ImportDecl.Specs = new ImportSpecs()
                  Comments = None } }
      Decls =
          new Decls(
              seq {
                  { EmbedDecl.Expr =
                        StructLit.New [| EltLabelString "module"
                                         EltDecl(BasicLit.NewString domain) |]
                    Comments = None }
                  :> IDecl
              }
          ) }
