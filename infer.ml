(* infer.ml *)
exception Undefined_constant of string 
let type_of_const c  =  
	let int3 = tarrow  tint (tarrow tint  tint) in  
	match c.name  with  
	| Int _ -> tint  
	| Name ("+" | "*") -> int3  
	| Name n -> raise  (Undefined_constant n);; 

exception Free_variable of  var 
let type_of_var tenv  x =  
	try List.assoc  x tenv  
	with Not_found -> raise  (Free_variable x) 
let extend tenv  (x, t) = (x, t)::tenv;;

let rec infer  tenv a t  =  
	match a with  
	| Const c -> funify  (type_of_const c) t  
	| Var x -> funify  (type_of_var tenv x) t  
	| Fun (x, a) ->  
		let  tv1 = tvar() and  tv2 = tvar() in  
		infer (extend tenv  (x, tv1)) a  tv2;  
		funify t (tarrow  tv1 tv2)  
	| App (a1, a2) ->  
		let  tv = tvar() in  
		infer tenv a1  (tarrow tv t);  
		infer tenv a2  tv  
	| Let (x, a1, a2) ->  
		let  tv = tvar() in  
		infer tenv a1  tv;  
		infer (extend tenv  (x, tv)) a2  t;; 

let type_of a  = let tv = tvar() in infer [] a  tv; tv;;

