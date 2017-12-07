module Interpreter
open System.IO
open TinyLang2.AbstractSyntax

let rec eval (e : TinyLang2.AbstractSyntax.Expression) : int =
    match e with
   | Num(n) -> n
   | Id(x) -> 0
   | Prim(op, e1, e2) ->
        match op with
        | "+" -> eval e1 + eval e2
        | "-" -> eval e1 - eval e2
        | "*" -> eval e1 * eval e2
        | "/" -> eval e1 / eval e2
        | _ -> raise <| Failure "Unknown primative"