open Syntax;;
open Unify;;
open Typescheme;;



exception Undefined_constant of string
let type_of_const c  =  
	let int3 = tarrow  tint (tarrow tint  tint) in  
	let bool3 = tarrow  tbool (tarrow tbool  tbool) in  
	match c.name  with  
	| Int _ -> [], tint
	| Bool _ -> [], tbool
	| Name ("+" | "-" | "*" | "/") -> [], int3
	| Name (">" | "<" | "==" | "!=") -> [], bool3
	| Name n -> raise  (Undefined_constant n);; 

let ftv_type t  =  
	let visited = marker() in  
	let rec visit  ftv t =  
		let t = repr  t in  
		if t.mark  = visited then ftv  
		else  
			begin  
				t.mark <- visited;  
				match  desc t with  
				| Tvar _ -> t::ftv  
				| Tcon (g, l) -> List.fold_left visit  ftv l  
			end in  
		visit [] t;;


let type_instance (q, t) =  
	acyclic t;  
	let copy t  = let t = repr  t in t, tvar() in  
	let copied = List.map copy  q in  
	let rec visit  t =  
		let t = repr  t in  
		try List.assq  t copied  with Not_found ->  
		begin match desc  t with  
		| Tvar _ 
		| Tcon  (_, []) -> t  
		| Tcon (g, l) -> texp (Tcon (g, List.map visit  l))  
		end in  
	visit t;;

exception Free_variable  of var 
let type_of_var tenv  x =  
	try List.assoc  x tenv  
	with Not_found -> raise  (Free_variable x) 
let extend tenv  (x, t) = (x, t)::tenv;;


let visit_type exclude  visited f t  = let rec visit  t =  
	let t = repr  t in  
	if t.mark  = exclude || t.mark  == visited then ()  
	else  
		begin  
			t.mark <- visited; f t;  
			match desc t  with  
			| Tvar _ -> ()  
			| Tcon (g, l) -> List.iter visit  l  
		end in 
	visit t;;


let generalizable tenv  t0 =  
	let inenv = marker() in  
	let mark m  t = (repr t).mark <- m  in  
	let visit_asumption (x, (q, t)) =  
		let bound = marker() in  
		List.iter (mark  bound) q; visit_type  bound inenv ignore  t in  
		List.iter visit_asumption  tenv;  
	let ftv = ref  [] in  
	let collect t  = 
		match desc t  with 
		Tvar _  -> ftv := t::!ftv  
		| _ -> 
			() in  
			let free = marker() in
			visit_type inenv free  collect t0;  !ftv;; 

let x = tvar();; 
generalizable [] (tarrow x  x);;


let rec infer  tenv a t  =  
	match a with  
	| Const c -> unify  (type_instance (type_of_const c)) t  
	| Var x -> unify  (type_instance (type_of_var tenv  x)) t  
	| Fun (x, a) ->  
		let  tv1 = tvar() and  tv2 = tvar() in  
		infer (extend tenv  (x, ([], tv1))) a  tv2;  
		unify t (tarrow  tv1 tv2)  
	| App (a1, a2) ->  
		let  tv = tvar() 
		in  infer tenv a1  (tarrow tv t);  
		infer tenv a2  tv  
	| Let (x, a1, a2) ->  
		let  tv = tvar() in  
		infer tenv a1  tv;  
		let  s = generalizable tenv  tv, tv in  
		infer (extend tenv  (List.hd x, s)) a2  t;; 

let type_of a  = let tv = tvar() in infer [] a  tv; tv;;

