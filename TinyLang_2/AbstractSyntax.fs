namespace TinyLang2

module AbstractSyntax =
    type Id = string

    type Statement =
        | Definition of Id * Expression
        | PrintStm of Expression
        | CompoundStm of Statement * Statement
        | IfStm of Expression * Statement
        | IfElseStm of Expression * Statement * Statement
        | WhileStm of Expression * Statement
        | FunDefinition of Id * Id * Expression * Statement

    and Expression =
        | Id of Id
        | Num of int
        | Prim of string * Expression * Expression
        | Bool of bool
        | EseqExpr of Statement * Expression
        | Invoke of Id * Expression