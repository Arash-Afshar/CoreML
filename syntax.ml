type name = Name  of string | Int  of int | Bool of bool;;
type constant = { name  : name; constr : bool; arity : int}
type var = string
type expr =
	| Var of var
	| Const of constant
	| Fun of var  * expr
	| App of expr * expr
	| Let of var  * expr * expr;;


let plus   = Const {name = Name "+";  arity = 2; constr = false}
let minus  = Const {name = Name "-";  arity = 2; constr = false}
let times  = Const {name = Name "*";  arity = 2; constr = false}
let div    = Const {name = Name "/";  arity = 2; constr = false}
let gt     = Const {name = Name ">";  arity = 2; constr = false}
let lt     = Const {name = Name "<";  arity = 2; constr = false}
let eq     = Const {name = Name "=="; arity = 2; constr = false}
let ne     = Const {name = Name "!="; arity = 2; constr = false}
let branch = Const {name = Name "branch"; arity = 3; constr = false}
let int n  = Const {name = Int n;    arity = 0; constr = true}
let bool b = Const {name = Bool b;   arity = 0; constr = true};;

