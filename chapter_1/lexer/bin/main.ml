let read_file (filename : string) : string =
  try
    (* Read file and display the first line *)
    let input_channel = open_in filename in
    let n = in_channel_length input_channel in
    let s = Bytes.create n in
    really_input input_channel s 0 n;
    close_in input_channel;
    Bytes.to_string s
  with e ->
    (* some unexpected exception occurs *)
    (* emergency closing *)
    raise e

let file_path : string =
  if Array.length Sys.argv >= 2 then Sys.argv.(1)
  else failwith "usage: ./main <file.c>"

let lexer_state : Lexer.lexer_state =
  { source = read_file file_path; position = 0 }

let token_to_str (token : Lexer.token_type) : string =
  match token with
  | TokenInt -> "TokenInt(int)"
  | TokenVoid -> "TokenVoid(void)"
  | TokenReturn -> "TokenReturn(return)"
  | TokenIf -> "TokenIf(if)"
  | TokenElse -> "TokenElse(else)"
  | TokenWhile -> "TokenWhile(while)"
  | TokenFor -> "TokenFor(for)"
  | TokenIdentifier_str s -> "TokenIdentifier(" ^ s ^ ")"
  | TokenConstant_int n -> "TokenConstant(" ^ string_of_int n ^ ")"
  | TokenLeftParenthesis -> "TokenLeftParenthesis[ ( ]"
  | TokenRightParenthesis -> "TokenRightParenthesis[ ) ]"
  | TokenLeftBrace -> "TokenLeftBrace({)"
  | TokenRightBrace -> "TokenRightBrace(})"
  | TokenSemiColon -> "TokenSemiColon(;)"
  | TokenComa -> "TokenComa(,)"
  | TokenPlus -> "TokenPlus(+)"
  | TokenMinus -> "TokenMinus(-)"
  | TokenStar -> "TokenStar(*)"
  | TokenSlash -> "TokenSlash(/)"
  | TokenNot -> "TokenNot(!)"
  | TokenAssign -> "TokenAssign(=)"
  | TokenEq -> "TokenEq(==)"
  | TokenNeq -> "TokenNeq(!=)"
  | TokenLt -> "TokenLt(<)"
  | TokenLeq -> "TokenLeq(<=)"
  | TokenGt -> "TokenGt(>)"
  | TokenGeq -> "TokenGeq(>=)"
  | TokenAnd -> "TokenAnd(&&)"
  | TokenOr -> "TokenOr(||)"
  | TokenEOF -> "TokenEOF"

let () =
  begin
    lexer_state |> Lexer.tokenize
    |> List.iter (fun t -> print_endline (token_to_str t))
  end
