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
    | Definition(id, e) ->
       (id, evalExpression e env) :: env
    | CompoundStm(stm1, stm2) ->
        evalStatement stm1 env
        |> evalStatement stm2
    | IfStm (expr, stm) ->
        match (evalExpression expr env) with
            | 0 -> env
            | _ ->
                evalStatement stm env |> ignore
                env
    | IfElseStm (expr, ifPart, elsePart) ->
        match (evalExpression expr env) with
            | 0 ->
                evalStatement elsePart env |> ignore
                env
            | _ ->
                evalStatement ifPart env |> ignore
                env
    | WhileStm (expr, stm) ->
        match (evalExpression expr env) with
        | 0 -> env
        | _ ->
            let loopEnv = evalStatement stm env
            evalStatement (WhileStm (expr, stm)) loopEnv

and evalExpression (e : Expression) scope : int =
    match e with
    | Num n -> n
    | Bool b -> if b then 1 else 0
    | Prim(op, e1, e2) ->
        let v1 = evalExpression e1 scope
        let v2 = evalExpression e2 scope
        match op with
        | "+" ->  v1 + v2
        | "-" -> v1 - v2
        | "*" -> v1 * v2
        | "/" -> v1 / v2
        | "<" -> if (v1 < v2) then 1 else 0
        | ">" -> if (v1 > v2) then 1 else 0
        | "=" -> if (v1 = v2) then 1 else 0
        | _ -> raise <| Failure "Unknown primative"
    | Id id -> lookup id scope

let tiny s = evalStatement(Parse.fromString s) []