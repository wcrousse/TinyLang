namespace TinyLang2

module AbstractSyntax =

    type Id = string 

    type Expression =
        | Define of Id * Expression 
        | Id of Id
        | Num of int
        | Prim of string * Expression * Expression
