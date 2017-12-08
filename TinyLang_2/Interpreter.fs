module Interpreter
open System.IO
open TinyLang2.AbstractSyntax
open Parse

type 'a Env = (string * 'a) list
type Closure = Id * Id * Expression * Value Env
and Value = | Int of int | Closure of Closure

let rec lookup id env =
    match env with
    | (s, n)::rest -> if s = id then n else lookup id rest
    | [] ->
        printf "kaboom"
        failwith <| sprintf "%s is not in scope" id

let rec evalStatement (s : Statement) (env : Value Env ) : Value Env =
    let scope = [];
    match s with
    | PrintStm(expr) ->
        printfn "%d" <| evalExpression expr env
        env
    | Definition(id, e) ->
       (id, Int(evalExpression e env)) :: env
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
            evalStatement (WhileStm (expr, stm)) loopEnv |> ignore
            env
    | FunDefinition (funName, paramName, fbody, context) ->
        printfn "Defining a function"
        let closure = Closure(funName, paramName, fbody, env)
        (funName, closure) :: env |>
        evalStatement context

and evalExpression (e : Expression) env : int =
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
    | EseqExpr(stm, expr) ->
        evalStatement stm env
        |> evalExpression expr
    | Invoke (funName, exprArgument) ->
        printfn "Invoking %s" funName
        let closure = lookup funName env
        match closure with
        | Closure (f, param, bodyExpression, delarationEnvironment) ->
            let argVal = Int(evalExpression exprArgument env)
            let functionEnvironment = (param, argVal) :: (funName, closure) :: delarationEnvironment
            evalExpression bodyExpression functionEnvironment
        | _ -> failwith <| sprintf "%s is not a function" funName
let tiny s = evalStatement(Parse.fromString s) []
let tinyf f = evalStatement(Parse.fromFile f) []