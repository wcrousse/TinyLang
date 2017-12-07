﻿namespace TinyLang2

module AbstractSyntax =
    type Id = string

    type Statement =
        | Define of Id * Expression
        | PrintStm of Expression
        | CompoundStm of Statement * Statement

    and Expression =
        | Id of Id
        | Num of int
        | Prim of string * Expression * Expression
        | SeqExpression of Statement * Expression