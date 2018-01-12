module Interpreter
open System.IO
// open TinyLang2.AbstractSyntax

type Id = string


type Expression =
    | Id of Id
    | Num of int
    | Bool of bool
    | Prim of string * Expression * Expression
    | LetBinding of Id * Expression * Expression
    | IfElse of Expression * Expression * Expression
    | FunDefinition of Id * Id * Expression * Expression
    | Invoke of Id * Expression

type 'a Env = (string * 'a) list
type Closure = Id * Id * Expression * Value Env
and Value = | Int of int | Closure of Closure

let rec lookup id env =
    match env with
    | (s, n)::rest -> if s = id then n else lookup id rest
    | [] ->
        printf "kaboom"
        failwith <| sprintf "%s is not in scope" id

 

let rec evalExpression (e : Expression) env  : int =
    match e with
    | Num n -> n
    | Bool b -> if b then 1 else 0
    | Prim(op, e1, e2) -> evalPrim op e1 e2 env
        
    | Id id ->  
        match lookup id env with
        | Int i -> i
        | _ -> failwith "bad times"

    | LetBinding (x, rightExpr, body) -> 
        let xValue = Int(evalExpression rightExpr env)
        let bodyEnv = (x, xValue) :: env
        evalExpression body bodyEnv

    | FunDefinition (fName, argName, fBody, inExpr) ->
        let closure = Closure(fName, argName, fBody, env)
        evalExpression inExpr ((fName, closure) :: env)

    | IfElse (conditionExpr, thenExpr, elseExpr) -> 
        (evalIf conditionExpr thenExpr elseExpr env)

    | Invoke (funName, exprArgument) ->
        evalInvoke funName exprArgument env

and evalPrim op e1 e2 env =
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

and evalIf conditionExpr thenExpr elseExpr env = 
    match (evalExpression conditionExpr env) with
    | 0 -> evalExpression elseExpr env
    | _ -> evalExpression thenExpr env

and evalInvoke fName argExpr env =
    let closure = lookup fName env
    match closure with
    | Closure (f, param, bodyExpression, delarationEnvironment) ->
        let argVal = Int(evalExpression argExpr env)
        let functionEnvironment = (param, argVal) :: (fName, closure) :: delarationEnvironment
        evalExpression bodyExpression functionEnvironment
    | _ -> failwith <| sprintf "%s is not a function" fName