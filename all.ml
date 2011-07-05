open Syntax;;
open Reduce;;
open Unify;;
open Typescheme;;
open Poly;;
open Prog;;


let e =  
 App(Fun("x", Var "x"), int 1);;

type_of e;;
eval [] e;;



(*let print_answer = function
	  Value v ->  
		match v with
		  Constant (c ,e) -> 
			match c.name with
			  Name s -> print_string(s^"\n")
			| Int i -> print_string(string_of_int i ^ "\n")
			| _ -> print_string("Not name!\n")
		| _ -> print_string("Not constant!\n")
	| _ -> print_string("Not value!\n");;
*)

let print_answer = function
	  Value v ->
		(
	 		match v with
			  Constant (c, value) ->
				(
					match c.name with
					  Name s -> print_string(s^"\n")
					| Int i -> print_string(string_of_int i ^ "\n")
					| _ -> print_string("Not name!\n")
				)
			| Closure (var, ex, env) -> print_string("lambda exp!\n")
		)
	| Error -> print_string("Not value!\n");;

print_answer ( eval [] e );;






