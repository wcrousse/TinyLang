module Interpreter
open System.IO
open TinyLang2.AbstractSyntax

type 'a Env = (string * 'a) list
type Closure = Id * Id * Expression * Value Env
and Value = | Int of int | Closure of Closure

let rec lookup id env =
    match env with
    | (s, n)::rest -> if s = id then n else lookup id rest
    | [] ->
        printf "kaboom"
        failwith <| sprintf "%s is not in scope" id

let rec evalExpression (e : Expression) env : int =
    match e with
    | Num n -> n
    | Bool b -> if b then 1 else 0
    | Prim(op, e1, e2) ->
        let v1 = evalExpression e1 env
        let v2 = evalExpression e2 env
        match op with
        | "+" ->  v1 + v2
        | "-" -> v1 - v2
        | "*" -> v1 * v2
        | "/" -> v1 / v2
        | "<" -> if (v1 < v2) then 1 else 0
        | ">" -> if (v1 > v2) then 1 else 0
        | "=" -> if (v1 = v2) then 1 else 0
        | _ -> raise <| Failure "Unknown primative"
    | Id id ->  match lookup id env with
        | Int i -> i
        | _ -> failwith "bad times"

    | Invoke (funName, exprArgument) ->
        printfn "Invoking %s" funName
        let closure = lookup funName env
        match closure with
        | Closure (f, param, bodyExpression, delarationEnvironment) ->
            let argVal = Int(evalExpression exprArgument env)
            let functionEnvironment = (param, argVal) :: (funName, closure) :: delarationEnvironment
            evalExpression bodyExpression functionEnvironment
        | _ -> failwith <| sprintf "%s is not a function" funName
