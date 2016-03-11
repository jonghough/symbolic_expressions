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

### integration

`integral (X^4)`

returns

`X^5/5`

### more complex integration
Integration is much more difficult to handle than differentiation. However, the parser will take a good stab at solving a reasonably complex problem (one that can be evaluated by *integration by parts* usually).

e.g. we want to integrate *cos(x)x^2*

`integral((cos(X))*(X^2))`

result:

`Sin(X)*X^2-(-Cos(X)*X+Sin(X))`

##Permissable expressions
* any integer value
* X - variable
* +,-,*,/,^  arithmetic operators
* Cos, Sin, Tan, Cot, Sec, Cosec  trig functions
* e^  exponential
* log natural logarithm
* d, differentiate, derivative  all differentiate the enclosed expression
* integral, integrate  integrate the enclosed expression
