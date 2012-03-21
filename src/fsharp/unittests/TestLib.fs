//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2011 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

namespace TestLibrary
  module LambdaCalculus =

    exception LambdaFailure of string

    module Syntax =
      type Binder = string

      type Expression = Variable of Binder
                      | Lambda   of (Binder * Expression)
                      | Apply    of (Expression * Expression)

      let rec stringOfExpression (e : Expression) : string =
        match e with
        | Variable x     -> x
        | Lambda (x, e)  -> "lambda " + x + " . " + stringOfExpression e
        | Apply (e1, e2) -> "(" + stringOfExpression e1 + ") @ (" + stringOfExpression e2 + ")"


    module Evaluation =

      open Syntax

      module Environment =
        type Env = Map<Binder, Expression>

        exception EnvironmentFailure of string

        let add (g : Env)(x : Binder)(e : Expression) = Map.add x e g

        let lookup (g : Env)(x : Binder) =
          try Map.find x g
              with _ -> raise (EnvironmentFailure <| "No binding for `" + (stringOfExpression <| Variable x) + "`.")

      open Environment

      exception EvalFailure = LambdaFailure

      let rec eval (g : Env)(e : Expression) : Expression =
        match e with
        | Variable x     -> lookup g x
        | Lambda _       -> e
        | Apply (e1, e2) -> match eval g e1 with
                            | Lambda (x, e) -> eval (add g x (eval g e2)) e1
                            | _             -> raise <| EvalFailure "Unexpected operator in application; need a lambda."

  module OtherTests =
    type Point = { x : int
                   y : int
                 }

    let showPoint (p : Point) = sprintf "(%A,%A)" p.x p.y 

    type Shape (initVertices : list<Point>) =
      let mutable vertices = initVertices

      let True _  = true
      let Id   x  = x

      // using this for everything is a bit artificial, but it ought to cover
      // quite a few patterns of members interacting
      member this.addFilterMap (pr : Point -> bool)(f : Point -> Point)(ps : list<Point>) : unit =
        match ps with
        | []      -> ()
        | p :: ps -> if pr p
                        then vertices <- (f p) :: vertices
                     this.addFilterMap pr f ps

      member this.getVertices () = vertices

      member this.clearVertices () = vertices <- []

      // new vertex
      member this.addVertex (p : Point) = this.addFilterMap True Id [p]

      // swallow another shape's vertices
      member this.subsume (s : Shape) = List.iter this.addVertex (s.getVertices ())

      member this.map (f : Point -> Point) =
        let ps = this.getVertices ()
        this.clearVertices ()
        this.addFilterMap True f ps

      member this.transpose () =
        let swap p =
          { x = p.y
            y = p.x
          }
        this.map swap

      // okay, this is silly; just to test mutual recursion of members
      member this.fold (f : 'a -> Point -> 'a)(acc : 'a) =
        match this.getVertices () with
        | []      -> acc
        | p :: ps -> f (this.refold f acc) p

      member this.refold (f : 'a -> Point -> 'a)(acc : 'a) =
        let ps = this.getVertices ()
        let set ps =
          this.clearVertices ()
          this.subsume (new Shape (ps))
        match ps with
        | []      -> ()
        | _ :: ps -> set ps
        let res = this.fold f acc
        set ps
        acc

      static member combine (s1 : Shape)(s2 : Shape) : Shape =
        let ps1 = s1.getVertices ()
        let ps2 = s2.getVertices ()
        new Shape (ps1 @ (ps2 |> List.filter (fun x -> not <| List.exists ((=) x) ps1)))
