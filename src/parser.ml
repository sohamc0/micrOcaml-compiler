open MicroCamlTypes
open Utils
open TokenTypes

(* Parser - Author: Soham Choudhury *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Parsing expressions *)

let rec parse_exp toks = let j = lookahead toks in
  if j=Some Tok_Let then parse_let_expr toks
  else if j=Some Tok_If then parse_if_expr toks
  else if j=Some Tok_Fun then parse_fun_expr toks
  else parse_or_expr toks

and parse_let_expr toks =
  let lst = match_token toks Tok_Let in
  let (rec_bool, lst1) = parse_rec lst in
  let (tok_id_name, lst2) = parse_prim_expr lst1 in
  let lst3 = match_token lst2 Tok_Equal in
  let (exp1, lst4) = parse_exp lst3 in
  let lst5 = match_token lst4 Tok_In in
  let (exp2, ret_lst) = parse_exp lst5 in
  match tok_id_name with
  | ID x -> (Let (x, rec_bool, exp1, exp2), ret_lst)
  | _ -> raise (InvalidInputException("not in ID format"))
and parse_rec toks = let j = lookahead toks in match j with
  | Some Tok_Rec -> (true, match_token toks Tok_Rec)
  | _ -> (false, toks)

and parse_if_expr toks =
  let lst = match_token toks Tok_If in
  let (exp1, lst1) = parse_exp lst in
  let lst2 = match_token lst1 Tok_Then in
  let (exp2, lst3) = parse_exp lst2 in
  let lst4 = match_token lst3 Tok_Else in
  let (exp3, ret_lst) = parse_exp lst4 in
  (If (exp1,exp2,exp3), ret_lst)

and parse_fun_expr toks =
  let lst1 = match_token toks Tok_Fun in
  let (id_name, lst2) = parse_prim_expr lst1 in
  let lst3 = match_token lst2 Tok_Arrow in
  let (exp, ret_lst) = parse_exp lst3 in
  match id_name with
  | ID x -> (Fun (x, exp), ret_lst)
  | _ -> raise (InvalidInputException("not in ID format"))
  

and parse_or_expr toks =
  let (exp1, lst1) = parse_and_expr toks in
  let (ret_bool, exp2, lst2) = parse_or_helper lst1 in
  if ret_bool=false then (exp1, lst2)
  else (Binop (Or, exp1, exp2), lst2)
and parse_or_helper toks =
  if lookahead toks=Some Tok_Or then
    let lst1 = match_token toks Tok_Or in
    let (exp, ret_lst) = parse_or_expr lst1 in
    (true, exp, ret_lst)
  else (false, Value (Bool true), toks)

and parse_and_expr toks =
  let (exp1, lst1) = parse_eq_expr toks in
  let (ret_bool, exp2, lst2) = parse_and_helper lst1 in
  if ret_bool=false then (exp1, lst2)
  else (Binop (And, exp1, exp2), lst2)
and parse_and_helper toks =
  if lookahead toks=Some Tok_And then
    let lst1 = match_token toks Tok_And in
    let (exp, ret_lst) = parse_and_expr lst1 in
    (true, exp, ret_lst)
  else (false, Value (Bool true), toks)

and parse_eq_expr toks =
  let (exp1, lst1) = parse_rel_expr toks in
  let (ret_val, exp2, lst2) = parse_eq_helper lst1 in
  if ret_val=0 then (exp1, lst2)
  else if ret_val=1 then (Binop (Equal, exp1, exp2), lst2)
  else (Binop (NotEqual, exp1, exp2), lst2)
and parse_eq_helper toks = 
  if lookahead toks=Some Tok_Equal then
    let lst1 = match_token toks Tok_Equal in
    let (exp, ret_lst) = parse_eq_expr lst1 in
    (1, exp, ret_lst)
  else if lookahead toks=Some Tok_NotEqual then
    let lst1 = match_token toks Tok_NotEqual in
    let (exp, ret_lst) = parse_eq_expr lst1 in
    (2, exp, ret_lst)
  else (0, Value (Bool true), toks)

and parse_rel_expr toks =
  let (exp1, lst1) = parse_add_expr toks in
  let (ret_val, exp2, lst2) = parse_rel_helper lst1 in
  if ret_val=0 then (exp1, lst2)
  else if ret_val=1 then (Binop (Less, exp1, exp2), lst2)
  else if ret_val=2 then (Binop (Greater, exp1, exp2), lst2)
  else if ret_val=3 then (Binop (LessEqual, exp1, exp2), lst2)
  else (Binop (GreaterEqual, exp1, exp2), lst2)
and parse_rel_helper toks = 
  if lookahead toks=Some Tok_Less then
    let lst1 = match_token toks Tok_Less in
    let (exp, ret_lst) = parse_rel_expr lst1 in
    (1, exp, ret_lst)
  else if lookahead toks=Some Tok_Greater then
    let lst1 = match_token toks Tok_Greater in
    let (exp, ret_lst) = parse_rel_expr lst1 in
    (2, exp, ret_lst)
  else if lookahead toks=Some Tok_LessEqual then
    let lst1 = match_token toks Tok_LessEqual in
    let (exp, ret_lst) = parse_rel_expr lst1 in
    (3, exp, ret_lst)
  else if lookahead toks=Some Tok_GreaterEqual then
    let lst1 = match_token toks Tok_GreaterEqual in
    let (exp, ret_lst) = parse_rel_expr lst1 in
    (4, exp, ret_lst)
  else (0, Value (Bool true), toks)

and parse_add_expr toks =
  let (exp1, lst1) = parse_mul_expr toks in
  let (ret_val, exp2, lst2) = parse_add_helper lst1 in
  if ret_val=0 then (exp1, lst2)
  else if ret_val=1 then (Binop (Add, exp1, exp2), lst2)
  else (Binop (Sub, exp1, exp2), lst2)
and parse_add_helper toks = 
  if lookahead toks=Some Tok_Add then
    let lst1 = match_token toks Tok_Add in
    let (exp, ret_lst) = parse_add_expr lst1 in
    (1, exp, ret_lst)
  else if lookahead toks=Some Tok_Sub then
    let lst1 = match_token toks Tok_Sub in
    let (exp, ret_lst) = parse_add_expr lst1 in
    (2, exp, ret_lst)
  else (0, Value (Bool true), toks)

and parse_mul_expr toks =
  let (exp1, lst1) = parse_con_expr toks in
  let (ret_val, exp2, lst2) = parse_mul_helper lst1 in
  if ret_val=0 then (exp1, lst2)
  else if ret_val=1 then (Binop (Mult, exp1, exp2), lst2)
  else (Binop (Div, exp1, exp2), lst2)
and parse_mul_helper toks = 
  if lookahead toks=Some Tok_Mult then
    let lst1 = match_token toks Tok_Mult in
    let (exp, ret_lst) = parse_mul_expr lst1 in
    (1, exp, ret_lst)
  else if lookahead toks=Some Tok_Div then
    let lst1 = match_token toks Tok_Div in
    let (exp, ret_lst) = parse_mul_expr lst1 in
    (2, exp, ret_lst)
  else (0, Value (Bool true), toks)

and parse_con_expr toks =
  let (exp1, lst1) = parse_una_expr toks in
  let (ret_bool, exp2, lst2) = parse_con_helper lst1 in
  if ret_bool=false then (exp1, lst2)
  else (Binop (Concat, exp1, exp2), lst2)
and parse_con_helper toks =
  if lookahead toks=Some Tok_Concat then
    let lst1 = match_token toks Tok_Concat in
    let (exp, ret_lst) = parse_con_expr lst1 in
    (true, exp, ret_lst)
  else (false, Value (Bool true), toks)

and parse_una_expr toks = 
  match toks with
  | Tok_Not::t -> let lst2 = (match_token toks Tok_Not) in
    let (e2, lst3) = (parse_una_expr lst2) in (Not (e2), lst3)
  | _ -> (parse_fun_call toks)

and parse_fun_call toks =
  let (bool1, ret_val, lst1) = parse_fun_helper toks in
  if bool1=false then raise (InvalidInputException(Printf.sprintf "bad call to fun_call.. tok_list: %s" (string_of_list string_of_token lst1)))
  else let (ret_bool, exp, lst2) = parse_fun_helper lst1 in
  if ret_bool=false then (ret_val, lst2)
  else (FunctionCall (ret_val, exp), lst2)
and parse_fun_helper toks =
  match toks with
  | [] -> (false, Value (Bool true), toks)
  | h::t -> match h with
            | Tok_Int(x) -> let (exp, lst1) = parse_prim_expr toks in (true, exp, lst1)
            | Tok_Bool(true) -> let (exp, lst1) = parse_prim_expr toks in (true, exp, lst1)
            | Tok_Bool(false) -> let (exp, lst1) = parse_prim_expr toks in (true, exp, lst1)
            | Tok_String(x) -> let (exp, lst1) = parse_prim_expr toks in (true, exp, lst1)
            | Tok_ID(x) -> let (exp, lst1) = parse_prim_expr toks in (true, exp, lst1)
            | Tok_LParen -> let (exp, lst1) = parse_prim_expr toks in (true, exp, lst1)
            | _ -> (false, Value (Bool true), toks)

and parse_prim_expr toks =
  match toks with
  | [] -> raise (InvalidInputException("no more tokens"))
  | h::t -> match h with
            | Tok_Int(x) -> let lst2 = match_token toks (Tok_Int(x)) in (Value (Int x), lst2)
            | Tok_Bool(true) -> let lst2 = match_token toks (Tok_Bool(true)) in (Value (Bool true), lst2)
            | Tok_Bool(false) -> let lst2 = match_token toks (Tok_Bool(false)) in (Value (Bool false), lst2)
            | Tok_String(x) -> let lst2 = match_token toks (Tok_String(x)) in (Value (String x), lst2)
            | Tok_ID(x) -> let lst2 = match_token toks (Tok_ID(x)) in (ID x, lst2)
            | Tok_LParen -> let lst2 = match_token toks Tok_LParen in 
                            let (e2, lst3) = (parse_exp lst2) in
                            let j = lookahead lst3 in
                            if j=Some Tok_RParen then (e2, match_token lst3 Tok_RParen)
                            else raise (InvalidInputException("Right Paren not found"))
            | _ -> raise (InvalidInputException("invalid prim expr call"))
;;

let rec parse_expr toks = let (b,a) = parse_exp toks in (a,b);;

(* Parsing mutop *)

let rec parse_mutop toks = match toks with
  | Tok_Def::t -> let (final_val, lst) = (parse_def toks) in
                  if lookahead lst!=None then raise (InvalidInputException(Printf.sprintf "still left after parse_def
                    .. tok_list: %s" (string_of_list string_of_token lst)))
                  else
                    (lst, final_val)
  | Tok_DoubleSemi::t -> if lookahead t=None then ([], NoOp) else raise (InvalidInputException(Printf.sprintf "still left after DoubleSemi
                    .. tok_list: %s" (string_of_list string_of_token t)))
  | _ -> let (final_val, lst) = parse_e_mutop toks in
         if lookahead lst!=None then raise (InvalidInputException(Printf.sprintf "still left after parse_e_mutop
                    .. tok_list: %s" (string_of_list string_of_token lst)))
         else
           (lst, final_val)

and parse_def toks =
  let lst1 = match_token toks Tok_Def in
  match lst1 with
  | Tok_ID(x)::t -> let string_of_ID = x in
                    let lst2 = match_token t Tok_Equal in
                    let (lst3, expr) = parse_expr lst2 in
                    let lst4 = match_token lst3 Tok_DoubleSemi in
                    (Def(string_of_ID,expr), lst4)
  | _ -> raise (InvalidInputException(Printf.sprintf "failure at parse_def.. tok_list: %s" (string_of_list string_of_token lst1)))

and parse_e_mutop toks =
  let (lst1, expr) = parse_expr toks in
  let lst2 = match_token lst1 Tok_DoubleSemi in (Expr (expr), lst2)
;;


