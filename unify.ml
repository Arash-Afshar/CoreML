type type_symbol = Tarrow  | Tint | Tbool
type texp = { mutable  texp_node : node; mutable  mark : int } 
and node = Desc  of desc | Link  of texp 
and desc = Tvar  of int | Tcon  of type_symbol * texp  list;;

let count = ref  0 
let tvar() = incr  count; ref (Desc  (Tvar !count));;

let texp d  = { texp_node = Desc d; mark = 0 };; 
let count = ref  0 let tvar() = incr  count; texp (Tvar  !count);; 
let tint = texp  (Tcon (Tint, []))
let tbool = texp  (Tcon (Tbool, []))
let tarrow t1  t2 = texp (Tcon  (Tarrow, [t1; t2]));; 

let last_mark = ref  0 
let marker() = incr  last_mark; !last_mark;; 

let rec repr  t =  
	match t.texp_node  with  
	  Link u -> let  v = repr u  in t.texp_node  <- Link v; v  
	| Desc _ -> t

let desc t  =
	match (repr t).texp_node with
	  Link u -> assert  false
	| Desc d -> d;;

exception Unify of  texp * texp 
exception Arity of  texp * texp 

let link t1  t2 = (repr t1).texp_node <- Link  t2 
let rec unify  t1 t2 =  
	let t1 = repr  t1 and t2  = repr t2 in  
	if t1 == t2  then () else  
	match desc t1, desc t2 with  
	| Tvar _, _  ->  
		link t1 t2  
	| _, Tvar _  ->  
		link t2 t1  
	| Tcon (g1, l1), Tcon (g2, l2) when g1 = g2  ->  
		link t1 t2;  List.iter2 unify  l1 l2  
	| _, _ -> 
		raise  (Unify (t1,t2)) ;;


exception Cycle of  texp list;; 
let acyclic t  =  
	let visiting = marker() and visited = marker() in  
	let cycles = ref  [] in  
	let rec visit  t =  
		let t = repr  t in  
		if t.mark  > visiting then ()  
		else if t.mark = visiting  then cycles := t  :: !cycles  
		else  
			begin  
				t.mark <- visiting;  
				begin match desc  t with  
				| Tvar _ -> ()  
				| Tcon (g, l) -> List.iter visit  l  
				end;  
				t.mark <- visited;  
			end in  
		visit t;  
		if !cycles <> [] then raise  (Cycle !cycles);; 
let funify t1  t2 = unify t1  t2; acyclic t1;;

