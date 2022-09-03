open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Author: Soham Choudhury *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

let not_helper v = match v with
  | Bool false -> Bool true
  | Bool true -> Bool false
  | _ -> raise (TypeError("Expected type bool"));;

let add_helper x = match x with
  | ((Int a), (Int b)) -> Int (a+b)
  | _ -> raise (TypeError("Expected type int"));;

let sub_helper x = match x with
  | ((Int a), (Int b)) -> Int (a-b)
  | _ -> raise (TypeError("Expected type int"));;

let mult_helper x = match x with
  | ((Int a), (Int b)) -> Int (a*b)
  | _ -> raise (TypeError("Expected type int"));;

let div_helper x = match x with
  | (Int a, Int 0) -> raise DivByZeroError
  | ((Int a), (Int b)) -> Int (a/b)
  | _ -> raise (TypeError("Expected type int"));;

let greater x = match x with
  | ((Int a), (Int b)) -> if a>b then Bool true else Bool false
  | _ -> raise (TypeError("Expected type int"));;

let less x = match x with
  | ((Int a), (Int b)) -> if a<b then Bool true else Bool false
  | _ -> raise (TypeError("Expected type int"));;

let greater_eq x = match x with
  | ((Int a), (Int b)) -> if a>=b then Bool true else Bool false
  | _ -> raise (TypeError("Expected type int"));;

let less_eq x = match x with
  | ((Int a), (Int b)) -> if a<=b then Bool true else Bool false
  | _ -> raise (TypeError("Expected type int"));;

let concat x = match x with
  | ((String a), (String b)) -> String (a^b)
  | _ -> raise (TypeError("Expected type String"));;

let eql_helper x = match x with
  | (Int a, Int b) -> if a=b then Bool true else Bool false
  | (Bool a, Bool b) -> if a=b then Bool true else Bool false
  | (String a, String b) -> if a=b then Bool true else Bool false
  | _ -> raise (TypeError("Can't compare types"));;

let not_eql_helper x = match x with
  | (Int a, Int b) -> if a=b then Bool false else Bool true
  | (Bool a, Bool b) -> if a=b then Bool false else Bool true
  | (String a, String b) -> if a=b then Bool false else Bool true
  | _ -> raise (TypeError("Can't compare types"));;

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
  | Value(x) -> x
  | ID x -> lookup env x
  | Not x -> let ret_val = eval_expr env x in not_helper ret_val
  | Binop(Add, x, y) -> add_helper (eval_expr env x,eval_expr env y)
  | Binop(Sub, x, y) -> sub_helper (eval_expr env x,eval_expr env y)
  | Binop(Mult, x, y) -> mult_helper (eval_expr env x,eval_expr env y)
  | Binop(Div, x, y) -> div_helper (eval_expr env x,eval_expr env y)
  | Binop(Greater, x,y) -> greater (eval_expr env x,eval_expr env y)
  | Binop(Less, x,y) -> less (eval_expr env x,eval_expr env y)
  | Binop(GreaterEqual, x,y) -> greater_eq (eval_expr env x,eval_expr env y)
  | Binop(LessEqual, x,y) -> less_eq (eval_expr env x,eval_expr env y)
  | Binop(Concat, x,y) -> concat (eval_expr env x,eval_expr env y)
  | Binop(Equal, x,y) -> let a=eval_expr env x in let b=eval_expr env y in eql_helper (a,b)
  | Binop(NotEqual, x,y) -> let a=eval_expr env x in let b=eval_expr env y in not_eql_helper (a,b)
  
  | Binop(Or, x,y) -> let a=eval_expr env x in let b=eval_expr env y in (match a with
    | Bool i -> (match b with
                | Bool j -> if i||j then Bool true else Bool false
                | _ -> raise (TypeError("Expected type bool")))
    | _ -> raise (TypeError("Expected type bool")))

  | Binop(And, x,y) -> let a=eval_expr env x in let b=eval_expr env y in (match a with
    | Bool i -> (match b with
                | Bool j -> if i&&j then Bool true else Bool false
                | _ -> raise (TypeError("Expected type bool")))
    | _ -> raise (TypeError("Expected type bool")))  

  | If(x,y,z) -> let a=eval_expr env x in (match a with
    | Bool true -> eval_expr env y
    | Bool false -> eval_expr env z
    | _ -> raise (TypeError("Expected type bool")))  

  | Let(x,false,init,body) -> let a=eval_expr env init in eval_expr (extend env x a) body

  | Fun(id,body) -> Closure (env, id, body)

  | FunctionCall(x,y) -> let a=eval_expr env x in (match a with
    | Closure (i,j,k) -> let b=eval_expr env y in let env2 = extend i j b in eval_expr env2 k
    | _ ->  raise (TypeError("Expected type Closure")))

  | Let(x,true,init,body) -> let env2 = extend_tmp env x in let v = eval_expr env2 init in (update env2 x v;
                              eval_expr env2 body)
;;



(* Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let rec get_elem env = match env with
  | [] -> []
  | (id,v)::t -> (id, {contents = v})::(get_elem t)
;;

let eval_mutop env m = match m with 
  | Def(x,y) -> let env2 = extend_tmp env x in let v = eval_expr env2 y in
                ([(x,{contents = v})], Some (v))
  | Expr(x) -> let v = eval_expr env x in (env, Some (v))
  | NoOp -> (env ,None);;