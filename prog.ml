open Syntax;;

let e =  
 App(Fun("x", Var "x"), int 1)



(*
	App(Fun("x", Var "x"), int 1)
*)


(*	let plus_x n  = App (App (plus, Var "x"), n) in  
	App (Fun ("x", App (App (times, plus_x (int 1)), plus_x  (int (-1)))),  
		App (Fun ("x", App (App (plus, Var "x"), int 1)),  int 2));;
*)
