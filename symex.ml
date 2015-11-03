open Core.Std

(* Expression type - can be arithmetic operation, X variable, Infinity, *)
(* various functions													*)

type exp =
		| Add of exp * exp
		| Mul of exp * exp
		| Div of exp * exp
		| Int of int
		| Sub of exp * exp
		| Neg of exp
		| Pow of exp * exp
		| X 
		| Infinity
		| E   of exp
		| Log of exp
                (* trigonometric functions*)
		| Sin of exp
		| Cos of exp
		| Tan of exp
        | Cot of exp
        | Sec of exp
        | Csc of exp
        | Dif of exp
        | Itg of exp
		

(* Tests if the given expression contains the test sub-expression *)
let rec contains test expression =
        match expression with
        | a when a = test -> true
        | Add(a,b) -> contains test a || contains test b
        | Mul(a,b) -> contains test a || contains test b
        | Sub(a,b) -> contains test a || contains test b
        | Div(a,b) -> contains test a || contains test b
        | Neg a   -> contains test a
        | Pow(a,b) -> contains test a || contains test b
        | E a -> contains test a
        | Log a -> contains test a
        | Sin a -> contains test a
        | Cos a -> contains test a
        | Tan a -> contains test a
        | _ -> false

(* add expresisons e and d *)
let rec (+:) e d =
		match e, d with
		| Infinity, a | a, Infinity			-> Infinity
		| Int a, Int b						-> Int (a + b)
		| Int 0, a | a, Int 0				-> a
		| Int a, (Neg (Int b)) | (Neg (Int b)), Int a 
											-> Int (a - b)
		| e, Add(f,g) | Add(f,g), e			-> e +: f +: g
		| a, b when a = b					-> x (Int 2) a
		| Div(a,b), Div(c,d)				-> div ((x a d) +: (x b c))  (x b d)
		| a, Div(b,c) | Div(b,c), a			-> Div (Add(b, Mul(a,c)), c)
		| a, Sub(b,c) | Sub(b,c), a			-> Sub((a +: b), c)
		| a, Neg b | Neg b, a				-> Add(a, Neg b)
		| a, Mul(b,c) | Mul(b,c), a			-> Add(a, x b c)
		| a, b when a = b					-> x (Int 2) a
		| a, b								-> Add(a,b)


(* subtract expression e2 from expression e1 *)
and (-:) e1 e2 =
	match e1, e2 with
	| a, b when a = b						-> Int 0
	| a, b when a = Neg b || b = Neg a		-> Mul( Int 2, a)
	| a, b									-> Add(a,(Neg b))


(* multiply expression e1 by expression e2 *)
and x e1 e2 =
	match e1, e2 with
	| Infinity, a | a, Infinity				-> Infinity
	| Int 0, b | b, Int 0					-> Int 0
	| Int a, Int b							-> Int (a * b)
	| Int 1, a | a, Int 1					-> a
	| Pow(X,a),Pow(X,b)						-> Pow(X, Add(a,b))
	| Int a, Pow(X,b) | Pow(X,b), Int a		-> Mul(Int a, Pow(X,b))
	| Int a, b | b, Int a					-> Mul((Int a),b)
	| X, Mul(X,a) | Mul(X,a), X				-> Mul(a, (Pow(X,(Int 2))))
	| Mul(X,a) ,Mul(X,b)					-> x (x a b) (Pow(X,(Int 2)))
	| X, Pow(X,a) | Pow(X,a), X				-> Pow(X, (a +: (Int 1)))
	| X, a | a, X							-> Mul (a,X) 
	| Pow(a,b), Pow(d,c) when a = d			-> Pow(a, Add(b,c))
	| a, b									-> Mul(a,b)
 

(* divide expression e1 by expresison e2 *)
and div e1 e2 =
	match e1, e2 with
	| _, Infinity | _, Neg Infinity			-> Int 0
	| Neg a, Int 0							-> Neg Infinity
	| a, Int 0								-> Infinity
	| a, Int 1								-> a
	| X, X									-> Int 1
	| (Int n), X							-> Pow(X, Int (1-n))
	| a, X									-> Div(Mul (a, Int 1), X)
	| a, b									-> x a (Div( (Int 1), b))


(* reciprocal of an expression *)
and recip expression =
	match expression with
	| Div (a, b)							-> Div(b, a)
	| _										-> Div(Int 1, expression)


(* expression e1 raised to te power expression e2 *)
and (^:) e1 e2 =
	match e1, e2 with
	| a, Int 0								-> Int 1
	| Int a, Int b							-> Int (Int.of_float((Float.of_int a) **(Float.of_int b)))
	| a, Neg b								-> Div (Int 1, Pow (a,b))
	| a, Div(b,c)							-> Pow(Pow(a,b),Div(Int 1, c))
	| X, a									-> Pow (X, a)
	| Add(a,b), c							-> Pow(Add(a,b),c)
	| a,b									-> Pow(a,b)


(* differentiate expression e1 with respect to X. Before differentiating	*)
(* reorder the sub expressions to allow easier consolidaiton of the result	*)
and diff e1 =
	(* reorder and simplify the expression befor epattern matching *)
	match ( simplify (reorder e1)) with
	| Int a									-> (Int 0)
	| X										-> (Int 1)
	| Neg a									-> Neg (diff a)
	| Add(a,b)								-> Add((diff a), (diff b))
	| Sub(a,b)								-> Sub((diff a), (diff b))
	| Mul(Int a, X) | Mul(X, Int a)			-> Int a
	| Mul(Int a, Int b)						-> Int 0
	| Mul(Int a, b)							-> Mul((Int a), diff b)
	| Mul(X, X)								-> Mul((Int 2), X)
	| Mul(a, Add(b,c)) | Mul(Add(b,c), a)	-> Add(diff (Mul(a,b)), 
                (diff(Mul(a,c))))
	(* product rule: (uv)' = u'v + v'u *)
	| Mul(a,b)								-> Add(Mul(a, (diff b)), Mul(b, (diff a)))
	(* division rule: (u/v)' = (u'v - v'u)/v^2 *)
	| Div(a,b)								-> Div( Sub(Mul(b, diff a),
                Mul(a, diff b)), (Mul(b,b)))

	| Pow(X,Int a)							-> Mul((Int a), (Pow(X, (Int(a - 1)))))
	| Pow(a, b)								-> diff (E(Mul(b,Log a)))
	| E X									-> E X
	| E (Int a)								-> Int 0
	| E a									-> Mul(diff a, E a)
	| Log (Int a)							-> Int 1
	| Log X									-> Pow(X, Int (-1))
	| Log a									-> Div((diff a), a)
        | Sin a                             -> Mul((diff a), Cos a)
        | Cos a                             -> Mul((diff a), Neg (Sin a))
        | Tan a                             -> Mul((diff a), (Mul(Sec(a), Sec(a))))
        | Csc a                             -> Mul((diff a), Neg( Mul(Csc a,
                Cot a)))
        | Sec a                             -> Mul((diff a), Mul(Sec a, Tan
                a))
        | Cot a                             -> Mul ((diff a), Neg (Pow(Csc a,
                Int 2)))
        | a									-> a


(* integral of the product of expression a and expression b. For simple expressions *)
(* the integral is straightforward. Otherwise, if possible use integration by parts *)
(* else fail. TODO - find methods to integrate for more expression products.		*)
and handle_integral_of_mult a b =
        match a, b with
        | Int c, b							-> Mul(Int c, integral b)
        | a, Int c							-> Mul(integral b, Int c)
        | X, q | q, X						-> Sub(Mul(X, integral q), integral(integral q))
		| Pow(X, p), q | q, Pow(X, p)		-> Sub(Mul(Pow(X,p), integral q),
                integral(simplify( Mul(integral q, simplify(Pow(X, Sub(p,Int
                1)))))))
        | a, b								-> Itg(Mul(a,b))


(* Handles integration of some trig function expressions. Will not be able to handle *)
(* all such expressions. TODO - add a wider range of trig expressions to be evaluated*)
and handle_trig t =
        match t with
        | Mul(Sin a, Cos b) | Mul(Cos a, Sin b) when a = b 
											-> Div(Sin(Mul(Int 2,
        a)), Int 2)
        | Mul(Cos a, Cos b) when a = b		-> Div(Add(Cos(Mul(Int 2, a)), Int 1), Int 2)
        | Pow(Cos a, Int 2)					-> Div(Add(Cos(Mul(Int 2, a)), Int 1), Int 2)
		| t									-> Itg(t)
	
(* Integrates expression e. The range of expressions that can be integrated, for now, *)
(* is very slim. TODO - evluate integral of more expressions						  *)
and integral e =
        match e with
        | Mul(Int a, Mul(Int b, Int c)) | Mul(Mul(Int b, Int c),Int a)	
											-> Int(a*b*c)
        | Mul(Int a, b) | Mul(b, Int a)		-> Mul(Int a, integral b)
        | Add(Int a, b) | Add(b, Int a)		-> Add(Int a, integral b)
        | Sub(Int a, b)                     -> Sub(Int a, integral b)
        | Sub(b,Int a)						-> Sub(integral b, Int a)
        | Div(b, Int a)						-> Div(integral b, Int a)
        | Int a								-> Mul(Int a, X)
        | Neg a                             -> Neg ( integral a)
        | X									-> Pow(X, Int 2)
        | E X                               -> E X
        | E (Mul(a, X)) when contains X a = false       
											-> Div(E(Mul(a,X)),a)
        | E(Mul(X, a)) when contains X a = false        
											-> Div(E(Mul(a,X)),a)
        | E(Add(X, a)) when contains X a = false        
											-> integral (E(Add(a,X)))
        | E(Add(a,X)) when contains X a = false         
											-> E(Add(a,X))
        | E (Neg X)							-> Neg(E(Neg X))
        | Pow(X, Int a)						-> Div(Pow(X, Int (a+1)), Int (a+1))
        | Add(a,b)							-> Add(integral(a),integral(b))
        | Sub(a,b)							-> Sub(integral(a), integral(b))
        | Mul(a,b)							-> simplify(handle_integral_of_mult a b)
        | Sin X								-> Neg(Cos X)
        | Sin(Mul(a,X)) when contains X a = false       
											-> Div(Neg(Cos(Mul(a,X))),a)
        | Sin(Mul(X,a)) when contains X a = false       
											-> Div(Neg(Cos(Mul(a,X))),a)
        | Cos X                             -> Sin X
        | Tan X                             -> Log(Sec X)
        | Sec X                             -> Log(Add(Sec X, Tan X))
        | Csc X                             -> Log(Add(Csc X, Cot X))
        | Cot X                             -> Log(Sin X)
        | Log X                             -> Sub(Mul(X, Log X), X)
        | a									-> Itg a
        

(* Reorder the sub expressions of expression e, to simplify later calculations *)
and reorder e =
        match e with
        | Mul(a,Mul(b,c)) | Mul(Mul(b,c),a) when a = b && a = c
											-> Pow(simplify a, Int 3)
        | Mul(c, Mul(a, b)) | Mul(c, Mul(b, a)) when a = c 
											-> simplify(Mul(simplify(Mul(a,c)), simplify b))
        | Mul(Mul(a,b),c) | Mul(Mul(b,a),c) when a = c
											-> simplify(Mul(simplify(Mul(a,c)), simplify b))
        | Mul(Int a, Int b)					-> Mul(Int a, Int b)
        | Mul(a, Int b)						-> Mul(Int b, simplify a)
        | Mul(X,X)							-> Mul(X,X)
        | Mul(X, a)                         -> Mul(simplify a, X)
        | Mul(Pow(a,b), Pow(c,d))           -> Mul(Pow(simplify a, simplify b), Pow(simplify c, simplify d))
        | Mul(Pow(a,b), c)                  -> Mul(simplify c, Pow(simplify a, simplify b))
        | Mul(E(a), E(b))                   -> Mul(E(simplify a),E(simplify b))
        | Mul(E(a), b)                      -> Mul(simplify b, E(simplify a)) 
        | a                                 -> a


(* Simplifies the expression by merging and evaluating  subexpressions	*)
(* whenever possible.													*)
and simplify e = 
        let ordered = reorder e in
	match ordered with
	| Add(Int a, Int b)						-> Int (a+b)
	| Add(X,Int 0) | Add(Int 0,X)			-> X
	| Add(X, Int a) | Add(Int a, X)			-> Add(Int a, X)
	| Add(X,X)								-> simplify (Mul(Int 2, X))
	| Add(Mul(a,X),X) | Add(X,Mul(a,X))		-> simplify(Mul(Add(a,Int 1),X))
	| Add(Mul(a,X),Mul(b,X))				-> simplify(Mul(Add(a,b),X))
    | Mul(Int a, Int b)						-> Int (a * b)
	| Mul(X,X)								-> Pow(X, Int 2)
	| Mul(X, Int 0)							-> Int 0
	| Mul(X, Int 1)							-> X
	| Mul(X, Int a) | Mul(Int a, X)			-> Mul(Int a, X)
    | Mul(Int 0, a) | Mul(a, Int 0)			-> Int 0
	| Mul(Pow(X,a),X) | Mul(X, Pow(X,a))	-> Pow(X, simplify(Add(a,Int 1)))
	| Mul(Pow(X,a),Pow(X,b))				-> Pow(X,simplify(Add(a,b)))
    | Mul(a, Div(b,c)) | Mul(Div(b,c),a)		
											-> Div(simplify(Mul(a,b)),simplify c)
	| Mul(Neg a, Neg b)						-> simplify (Mul(a,b))
	| Mul(a, Neg b) | Mul(Neg b, a)			-> Neg( simplify (Mul(a,b)))
	| Mul(Pow(a,b),Pow(c, Neg d)) | Mul(Pow(a, Neg b), Pow(c,d)) when a=c && b=d	
											-> Int 1
	| Mul(Pow(a, Int b),Pow(c, Int d)) when a = c && b=(-1*d)			
											-> Int 1
	| Mul(E (Int a),E (Int b)) when a=(-1*b)	
											-> Int 1
	| Mul(E a, E(Neg b)) | Mul(E(Neg a),E b) when a=b				
											-> Int 1
    
    | Mul(E(a),E(b))						-> simplify(E(simplify(Add(a,b))))
	| Sub(a, Neg b)							-> simplify (Add(a,b))
    | Mul(a,b) when a = b					-> Pow((simplify a), Int 2)
	| Sub(a, b) when a=b					-> Int 0
	| Sub(Add(a,b),c) when a=c				-> simplify b
	| Sub(Add(a,b),c) when b=c				-> simplify a
    | Sub(Int a, Int b)						-> Int (a-b)
    | Sub(Neg a, b)							-> simplify (Mul(Int (-1), Add(a,b)))
    | Div(a,b) when a = b					-> Int 1
    | Div(a,b) when a = Neg b || b = Neg a  -> Int (-1)
    | Div(Int a, Int b)						-> Div(Int a, Int b)
    | Div(Int a, b)							-> Div(Int a, simplify b)
    | Div(a, Int b)							-> Div(simplify a, Int b)
	| Div(Sin(a),Cos(b)) when a = b			-> Tan a
	| Div(Cos(a),Sin(b)) when a = b			-> Cot a
    | Pow(Int a, Int b)						-> Int ( Int.of_float ((Float.of_int a) ** (Float.of_int b)))
    | Pow(X, Int 0)							-> Int 1
    | Pow(X, Int 1)							-> X
    | Pow(X, a)								-> Pow(X, simplify a)
    | Pow(a, (Log b)) when a = b			-> Int 1
    | Pow (a, Mul(c, (Log b))) | Pow(a, Mul((Log b), c)) when a = b			
											-> simplify c
    | E (Log (E (Int 1)))					-> Int 1
    | E (Int a)								-> E (Int a)
    | E a									-> E (simplify a)
    | Neg(Neg a)							-> a
        (* TRIGONOMETRICS *)
    | Div(Sin a, Cos b) when a = b          -> Tan(a)
    | Div(Cos a, Sin b) when a = b          -> Cot(a)
    | Mul(Cos a, Tan b) when a = b          -> Sin(a)
    | Div(Sin a, Tan b) when a = b          -> Cos(a)
    | Div(Tan a, Sin b) when a = b          -> Sec(a)
    | Mul(Sin a, Csc b) when a = b          -> Int 1
    | Mul(Cos a, Sec b) when a = b          -> Int 1
    | Mul(Tan a, Cot b) when a = b          -> Int 1
    | Cos a									-> Cos (simplify a)
    | Sin a									-> Sin (simplify a)
    | Tan a									-> Tan (simplify a)
    | Cot a									-> Cot (simplify a)
    | Csc a									-> Csc (simplify a)
    | Sec a									-> Sec (simplify a)
    | Add(Pow(Cos a, Int 2), Pow(Sin b, Int 2)) | Add(Pow(Sin b, Int 2), Pow( Cos a, Int 2)) when a = b             
											-> Int 1
    | Dif (Itg a)							-> a
    | Dif a									-> diff (simplify a)
    | Itg (Dif a)							-> simplify a
    | Itg a									-> integral (simplify a)
	| Add (a,b)								-> Add(simplify a, simplify b)
	| Mul (a,b)								-> Mul(simplify a, simplify b)
	| Sub (a,b)								-> Sub(simplify a, simplify b)
	| Div (a,b)								-> Div(simplify a, simplify b)
	| Pow (a,b)								-> Pow(simplify a, simplify b)
	| a										-> a
	

(* express the expression, i.e. express it as a string to be displayed *)
let rec  express e =
	match e with
	| Int a									-> string_of_int a
	| Infinity								-> "Infinity"
	| X										-> "X"
	
	| Add(a,b) -> 
		(match a, b with
		| a,b when a = Int 0				-> express b
		| a,b when b = Int 0				-> express a
		| Int c, Int d						-> express(Int (c + d))
		| X, X								-> "2X"
		| _,_								-> String.concat ["(";(express a); "+"; (express b); ")"])
	
	| Mul(a,b) ->
		(match a, b with
		| a,b when a = Int 1				-> express b
		| a,b when b = Int 1				-> express a
		| a,b when a = Int 0				-> express (Int 0)
		| a,b when b = Int 0				-> express (Int 0)
		| a,b when a = Int(-1)				-> String.concat["-";(express b)]
		| a,b when b = Int(-1)				-> String.concat["-";(express a)]
                | a,b when a = b			-> express (Pow(a, Int 2))
                | X, X						-> express (Pow(X,Int 2))
		| _,_								-> String.concat [(express a); "*"; (express b)])

	| Div(a,b) ->
		(match a,b with
		| a,b when a = b					-> express (Int 1)
		| Mul(c,X), Mul(d,X)				-> express (Div(c,d))
		| _,_								-> String.concat [(express a); "/"; (express b)])

	| Sub(a,b)								-> String.concat [(express a); "-"; (express b)]
	| Pow(X,Int 0)							-> express (Int 1)
	| Pow(X, Int 1)							-> express X
	| Pow(X, Int a) when a < 0				-> String.concat["(1/";(express (Pow(X, Int (-a))));")"]
	| Pow(a,b)								-> String.concat [(express a); "^"; (express b)]
	| Neg(a)								-> String.concat ["-";(express a)]
	| Log(a)								-> String.concat[ "Log(";(express a);")"]
	| E a									-> String.concat["(";"e^(";(express a);"))"]
    | Sin a									-> String.concat["Sin(";(express a);")"]
    | Cos a									-> String.concat["Cos(";(express a);")"]
	| Tan a									-> String.concat["Tan(";(express a);")"]
    | Sec a									-> String.concat["Sec(";(express a);")"]
    | Csc a									-> String.concat["Cosec(";(express a);")"]
    | Cot a									-> String.concat["Cot(";(express a);")"]
	| Itg a									-> String.concat["Integral(";(express a);")"]
    | _										-> "err"
