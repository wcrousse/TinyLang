namespace TinyLang2

module AbstractSyntax =
    type Id = string

    type Statement =
        | Define of Id * Expression
        | PrintStm of Expression
        | CompoundStm of Statement * Statement
        | IfStm of Expression * Statement

    and Expression =
        | Id of Id
        | Num of int
        | Prim of string * Expression * Expression
        | Bool of bool