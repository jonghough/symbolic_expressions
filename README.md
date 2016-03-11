# symbolic_expressions
Just for fun evaluator of symbolic expressions in OCaml with Menhir. Handles trigonometric functions, derivatives and some integrals.

## to build

`ocamlbuild -use-menhir -tag thread -use-ocamlfind -pkg core Main.native`

##to run

`./Main.native`

##examples

### simple differentiation

`d(e^(3*X))`

returns

`3*(e^(3*X))`

### more complicated example

`d((X^2)+e^(cos(X)))`

returns

`(2*X+-Sin(X)*(e^(Cos(X))))`

The result is slightly ugly, but symbolically correct.

##Permissable expressions
* any integer value
* X - variable
* +,-,*,/,^  arithmetic operators
* Cos, Sin, Tan, Cot, Sec, Cosec  trig functions
* e^  exponential
* log natural logarithm
* d, differentiate, derivative  all differentiate the enclosed expression
* integral, integrate  integrate the enclosed expression
