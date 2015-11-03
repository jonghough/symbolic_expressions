(* Lexer script for tokenizing input into expression tokens *)
{
        open Parser
        exception Error of string
}

rule lex = parse
| [' ' '\t']+
        { lex lexbuf}
| ['\r' '\n']       {lex lexbuf}
| ['X' 'x']
        {X}
| ['0'-'9']+ as n
        {INT (int_of_string n)}
| '+'
        {ADD}
| '-' 
        {SUB}
| '*'
        {MUL}
| '/' 
        {DIV}
| '^'
        {POW}
| '(' 
        {LEFT}
| ')'
        {RIGHT}
| "cos" {COS}
| "sin" {SIN}
| "tan" {TAN}
| "sec" {SEC}
| "csc" {CSC}
| "cot" {COT}
| ['e' 'E']['^']  
        {EXP}
| "log" {LOG}
| 'd'   {DIF}
| "derivative"
		{DIF}
| "differentiate"
		{DIF}
| "integrate"
		{ITG}
| "integral"
		{ITG}
| eof
        {EOF}

