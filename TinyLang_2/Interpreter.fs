module Interpreter
open System.IO
open TinyLang2.AbstractSyntax
open Parse

type Env = (string * int) list

let rec lookup id env =
    match env with
    | (s, n)::rest -> if s = id then n else lookup id rest
    | [] ->
        printf "kaboom"
        failwith <| sprintf "%s is not in scope" id

let rec evalStatement (s : Statement) (env : Env) : Env =
    let scope = [];
    match s with
    | PrintStm(expr) ->
        printfn "%d" <| evalExpression expr env
        env
    | Define(id, e) ->
       (id, evalExpression e env) :: env
    | CompoundStm(stm1, stm2) ->
        evalStatement stm1 env
        |> evalStatement stm2

and evalExpression (e : Expression) scope : int =
    match e with
    | Num(n) -> n
    | Prim(op, e1, e2) ->
        match op with
        | "+" -> (evalExpression e1 scope) + (evalExpression e2 scope)
        | "-" -> (evalExpression e1 scope) - (evalExpression e2 scope)
        | "*" -> (evalExpression e1 scope) * (evalExpression e2 scope)
        | "/" -> (evalExpression e1 scope) / (evalExpression e2 scope)
        | _ -> raise <| Failure "Unknown primative"
    | Id id -> lookup id scope

let tiny s = evalStatement(Parse.fromString s) []