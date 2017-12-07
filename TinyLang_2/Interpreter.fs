module Interpreter
open System.IO
open TinyLang2.AbstractSyntax
open Parse

type env = (string * int) list

let rec lookup id env = 
    match env with
    | (s, n)::rest -> if s = id then n else lookup id rest
    | [] ->
        printf "kaboom"
        failwith <| sprintf "%s is not in scope" id

let rec eval (e : TinyLang2.AbstractSyntax.Expression) scope : int =
    match e with
   | Num(n) -> n
   | Prim(op, e1, e2) ->
        match op with
        | "+" -> (eval e1 scope) + (eval e2 scope)
        | "-" -> (eval e1 scope) - (eval e2 scope)
        | "*" -> (eval e1 scope) * (eval e2 scope)
        | "/" -> (eval e1 scope) / (eval e2 scope)
        | _ -> raise <| Failure "Unknown primative"
    | Id id -> lookup id scope
    | Define (id, e1, e2) -> 
        let value = eval e1 scope
        eval e2 ((id, value)::scope)
let tiny s = eval (Parse.fromString s) []