namespace TinyLang2

module AbstractSyntax =
    type Id = string

    type Statement =
        | Definition of Id * Expression
        | PrintStm of Expression
        | CompoundStm of Statement * Statement
        | IfStm of Expression * Statement
        | WhileStm of Expression * Statement

    and Expression =
        | Id of Id
        | Num of int
        | Prim of string * Expression * Expression
        | Bool of bool