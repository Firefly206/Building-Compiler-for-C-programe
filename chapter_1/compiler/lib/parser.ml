open Lexer

let expect (token_expect : Lexer.token_type)
    (token_list : Lexer.token_type list) : Lexer.token_type list =
  match token_list with
  | [] -> failwith "Empty token list"
  | h :: t ->
      begin if token_expect = h then t
      else
        failwith
          (Lexer.token_to_str token_expect
          ^ "is expected.But got " ^ Lexer.token_to_str h ^ "instead")
      end
