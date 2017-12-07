module Parse

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing
open TinyLang2.AbstractSyntax

(* Plain parsing from a string, with poor error reporting *)

let fromString (str : string) : Statement =
    let lexbuf = LexBuffer<char>.FromString(str)
    try
      Parser.Main Lexer.Token lexbuf
    with
      | exn -> let pos = lexbuf.EndPos
               failwithf "%s near line %d, column %d\n"
                  (exn.Message) (pos.Line+1) pos.Column

(* Parsing from a text file *)

let fromFile (filename : string) =
    use reader = new StreamReader(filename)
    let lexbuf = LexBuffer<char>.FromTextReader reader
    try
      Parser.Main Lexer.Token lexbuf
    with
      | exn -> let pos = lexbuf.EndPos
               failwithf "%s in file %s near line %d, column %d\n"
                  (exn.Message) filename (pos.Line+1) pos.Column