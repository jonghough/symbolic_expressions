open Symex
open Core.Std

let formula_of_string s = Parser.parse_expression Lexer.lex (Lexing.from_string s)

let _ = 
        Out_channel.output_string stdout "enter expression: ";
        Out_channel.flush stdout;
        match In_channel.input_line stdin with
        | None -> failwith "No input given."
        | Some str ->
                print_endline (express (simplify (formula_of_string str)))
