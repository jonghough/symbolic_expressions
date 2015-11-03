(* Parser script for parsing input into expressible form *)
%token  X
%token <int> INT
%token MUL
%token ADD
%token SUB
%token DIV
%token POW
%token LEFT
%token RIGHT
%token COS
%token SIN
%token TAN
%token COT
%token SEC
%token CSC
%token DIF
%token EXP
%token LOG
%token ITG
%token EOF

%{
        open Symex
        %}

%start parse_expression
%type <Symex.exp> parse_expression expr

%%


%public expr:
        | X                             { X }
        | INT                           { Int ($1) }
		| LEFT expr RIGHT               { ($2) }
        | expr ADD expr                 { Add($1, $3) }
        | expr MUL expr                 { Mul($1, $3) }
        | expr SUB expr                 { Sub($1, $3) }
        | expr DIV expr                 { Div($1, $3) }
        | expr POW expr                 { Pow($1, $3) }       
        | COS expr                      { Cos($2) }
        | SIN LEFT expr RIGHT           { Sin($3) }
        | TAN LEFT expr RIGHT           { Tan($3) }
        | COT LEFT expr RIGHT           { Cot($3) }
        | SEC LEFT expr RIGHT           { Sec($3) }
        | CSC LEFT expr RIGHT           { Csc($3) }
        | DIF LEFT expr RIGHT           { simplify(Dif($3)) }
        | EXP expr                      { E($2) }
        | LOG expr                      { Log($2) }
        | ITG expr						{ Itg($2) }
		parse_expression:
                | expr EOF         {$1}
