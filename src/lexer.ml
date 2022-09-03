open TokenTypes
open Str

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)


let tokenize input = let rec tok pos s =
	if pos >= (String.length s) then []
	else
		if (string_match (regexp "(-[0-9]+)") s pos) then
			let token = matched_string s in
        	(Tok_Int (int_of_string (String.sub token 1 ((String.length token)-2))))::(tok (pos+(String.length token)) s)
        else if (string_match (regexp "[0-9]+") s pos) then
        	let token = matched_string s in
        	(Tok_Int (int_of_string token))::(tok (pos+(String.length token)) s)
        else if (string_match (regexp "true") s pos) then
			Tok_Bool(true)::(tok (pos+4) s)
		else if (string_match (regexp "false") s pos) then
			Tok_Bool(false)::(tok (pos+5) s)
		else if (string_match (regexp ")") s pos) then
        	Tok_RParen::(tok (pos+1) s)
		else if (string_match (regexp "(") s pos) then
        	Tok_LParen::(tok (pos+1) s)
      	else if (string_match (regexp "=") s pos) then
        	Tok_Equal::(tok (pos+1) s)
        else if (string_match (regexp "let") s pos) then
        	Tok_Let::(tok (pos+3) s)
        else if (string_match (regexp "def") s pos) then
        	Tok_Def::(tok (pos+3) s)
        else if (string_match (regexp "in") s pos) then
        	Tok_In::(tok (pos+2) s)
        else if (string_match (regexp "rec") s pos) then
        	Tok_Rec::(tok (pos+3) s)
        else if (string_match (regexp "fun") s pos) then
        	Tok_Fun::(tok (pos+3) s)
        else if (string_match (regexp ";;") s pos) then
        	Tok_DoubleSemi::(tok (pos+2) s)
        else if (string_match (regexp ">=") s pos) then
        	Tok_GreaterEqual::(tok (pos+2) s)
      	else if (string_match (regexp "<=") s pos) then
        	Tok_LessEqual::(tok (pos+2) s)
      	else if (string_match (regexp "<>") s pos) then
        	Tok_NotEqual::(tok (pos+2) s)
        else if (string_match (regexp ">") s pos) then
        	Tok_Greater::(tok (pos+1) s)
      	else if (string_match (regexp "<") s pos) then
        	Tok_Less::(tok (pos+1) s)
        else if (string_match (regexp "||") s pos) then
        	Tok_Or::(tok (pos+2) s)
      	else if (string_match (regexp "&&") s pos) then
        	Tok_And::(tok (pos+2) s)
        else if (string_match (regexp "not") s pos) then
        	Tok_Not::(tok (pos+3) s)
        else if (string_match (regexp "if") s pos) then
        	Tok_If::(tok (pos+2) s)
        else if (string_match (regexp "then") s pos) then
        	Tok_Then::(tok (pos+4) s)
        else if (string_match (regexp "else") s pos) then
        	Tok_Else::(tok (pos+4) s)
        else if (string_match (regexp "[a-zA-Z][a-zA-Z0-9]*") s pos) then
        	let token = matched_string s in
        	(Tok_ID token)::(tok (pos+(String.length token)) s)
        else if (string_match (regexp "+") s pos) then
        	Tok_Add::(tok (pos+1) s)
        else if (string_match (regexp "->") s pos) then
        	Tok_Arrow::(tok (pos+2) s)
        else if (string_match (regexp "-") s pos) then
        	Tok_Sub::(tok (pos+1) s)
        else if (string_match (regexp "*") s pos) then
        	Tok_Mult::(tok (pos+1) s)
        (* else if (string_match (regexp "\"[^\"]*\"") s pos) then
        	let token = matched_string s in
        	(Tok_String token)::(tok (pos+(String.length token)) s) *)
      	else if (string_match (regexp "/") s pos) then
        	Tok_Div::(tok (pos+1) s)
        else if (string_match (regexp "^") s pos) then
        	Tok_Concat::(tok (pos+1) s)
		else if (string_match (regexp "[ \t\n]") s pos) then 
        	(tok (pos+(String.length (matched_string s))) s)
		else
			raise (InvalidInputException "tokenization failure")
	in
	tok 0 input;;