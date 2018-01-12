namespace TinyLang2

module AbstractSyntax =
    type Id = string

    type Expression =
        | Id of Id
        | Num of int
        | Bool of bool
        | Prim of string * Expression * Expression
        | IdDefinition of Id * Expression * Expression
        | IfElseStm of Expression * Expression * Expression
        | FunDefinition of Id * Id * Expression * Expression
        | Invoke of Id * Expression