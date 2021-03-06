open Syntax;;

type env = (string  * value) list 
and value =  | Closure of var  * expr * env  | Constant of constant  * value list ;;

type answer = Error  | Value of value;;

let  val_int  u =
	Value (Constant ({name = Int  u; arity = 0; constr = true}, []));; 

let  val_bool u =
	Value (Constant ({name = Bool u; arity = 0; constr = true}, []));; 

let delta c  l =
	match c.name, l with
	| Name "+", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_int (first + second)
	| Name "-", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_int (first - second)
	| Name "*", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_int (first * second)
	| Name "/", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_int (first / second)
	| Name ">", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_bool (first > second)
	| Name "<", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_bool (first < second)
	| Name "==", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_bool (first == second)
	| Name "!=", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_bool (first != second)
	| Name "branch", [ Constant ({name=Int third}, []); Constant ({name=Int second}, []); Constant ({name=Bool first}, [])] ->
		let res =
			if first then second else third in
			val_int (res)
	| Name "branch", [ Constant ({name=Bool third}, []); Constant ({name=Bool second}, []); Constant ({name=Bool first}, [])] ->
		let res =
			if first then second else third in
			val_bool (res)
	| _ ->
		Error;;


let get x  env =
	try Value (List.assoc x  env) with Not_found  -> Error;;

let rec eval  env = function
	| Var x -> get  x env
	| Const c -> Value  (Constant (c, []))
	| Fun (x, a) -> Value (Closure (x, a, env))
	| Let (x, a1, a2) ->	
		begin match eval  env a1 with
		| Value v1 -> eval  ((x, v1)::env) a2  
		| Error -> Error  
		end  
	| App (a1, a2) ->  
		begin match eval  env a1 with  
		| Value v1 ->  
			begin match v1, eval env a2  with  
			| Constant  (c, l), Value  v2 ->  
				let  k = List.length  l + 1 in  
				if  c.arity  < k then Error  
				else  if c.arity  > k then Value (Constant (c, v2::l))  
				else  if c.constr  then Value (Constant  (c, v2::l))  
				else  delta c (v2::l)  
			| Closure (x, e, env0), Value v2  ->  
				eval ((x, v2) :: env0) e  
			| _, Error -> Error  
			end  
		|Error -> Error  
		end  
	| _ -> Error ;;

