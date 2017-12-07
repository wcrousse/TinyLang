namespace TinyLang2

module AbstractSyntax =
    type Expression =
        | Id of string
        | Num of int
        | Prim of string * Expression * Expression