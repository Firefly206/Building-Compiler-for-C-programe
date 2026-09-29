(* This is a token constructor *)
type token_type =
  | TokenInt
  | TokenVoid
  | TokenReturn
  | TokenIf
  | TokenElse
  | TokenWhile
  | TokenFor
  | TokenIdentifier_str of string
  | TokenConstant_int of int
  | TokenLeftParenthesis
  | TokenRightParenthesis
  | TokenLeftBrace
  | TokenRightBrace
  | TokenSemiColon
  | TokenComa
  | TokenPlus
  | TokenMinus
  | TokenStar
  | TokenSlash
  | TokenNot (* ! *)
  | TokenAssign (* = *)
  | TokenEq (* == *)
  | TokenNeq (* != *)
  | TokenLt (* < *)
  | TokenLeq (* <= *)
  | TokenGt (* > *)
  | TokenGeq (* >= *)
  | TokenAnd (* && *)
  | TokenOr (* || *)
  | TokenEOF

(* This is a record that holds the file(a big string) and a index(location) *)
type lexer_state = { source : string; mutable position : int }

(* This is function that peek/look at the CURRENT position of the string and return the character there.
   Option here to handle the case where we reach EOF (meaning nothing get return)*)
let peek (state : lexer_state) : char option =
  if state.position < String.length state.source then
    Some (String.get state.source state.position)
  else None

(* This is function that peek/look at the NEXT position of the string and return the character there.
   Option here to handle the case where we reach EOF (meaning nothing get return)*)
let peek_next (state : lexer_state) : char option =
  if state.position + 1 < String.length state.source then
    Some (String.get state.source (state.position + 1))
  else None

(* This is to increase the position/pointer at the string *)
let advance (state : lexer_state) : unit = state.position <- state.position + 1

(* This fun were create to consume chars for two cases: identifiers and constants/numbers. 
  The recursive loop takes no arguements, and return no value that's why it has type unit. We match (peek state) not peek and state,
  where the condition fun being met. Then return the string value. *)
let read_while (state : lexer_state) (condition : char -> bool) : string =
  let buff = Buffer.create 16 in
  let rec loop () =
    match peek state with
    | Some c when condition c -> begin
        Buffer.add_char buff c;
        advance state;
        loop ()
      end
    | _ -> ()
  in
  loop ();
  Buffer.contents buff

(* This function takes and process a string and return the token type. *)
let rec next_token (state : lexer_state) : token_type =
  let is_digit (c : char) : bool = c >= '0' && c <= '9' in
  let is_letter (c : char) : bool =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  in
  let is_identifier_char (c : char) : bool =
    c = '_' || is_letter c || is_digit c
  in
  let rec skip_block_comment (state : lexer_state) : unit =
    match (peek state, peek_next state) with
    | None, _ -> failwith "unterminated comment"
    | Some '*', Some '/' -> begin
        advance state;
        advance state
      end
    | _, _ -> begin
        advance state;
        skip_block_comment state
      end
  in
  match peek state with
  | None -> TokenEOF
  | Some c when c = ' ' || c = '\t' || c = '\n' || c = '\r' -> begin
      advance state;
      next_token state
    end
  | Some c when is_digit c -> begin
      let str = read_while state (fun c -> is_digit c) in
      TokenConstant_int (int_of_string str)
    end
  | Some c when is_letter c || c = '_' -> begin
      let str = read_while state (fun c -> is_identifier_char c) in
      match str with
      | "int" -> TokenInt
      | "void" -> TokenVoid
      | "return" -> TokenReturn
      | "if" -> TokenIf
      | "else" -> TokenElse
      | "while" -> TokenWhile
      | "for" -> TokenFor
      | _ -> TokenIdentifier_str str
    end
  | Some '(' -> begin
      advance state;
      TokenLeftParenthesis
    end
  | Some ')' -> begin
      advance state;
      TokenRightParenthesis
    end
  | Some '{' -> begin
      advance state;
      TokenLeftBrace
    end
  | Some '}' -> begin
      advance state;
      TokenRightBrace
    end
  | Some ';' -> begin
      advance state;
      TokenSemiColon
    end
  | Some ',' -> begin
      advance state;
      TokenComa
    end
  | Some '+' -> begin
      advance state;
      TokenPlus
    end
  | Some '-' -> begin
      advance state;
      TokenMinus
    end
  | Some '*' -> begin
      advance state;
      TokenStar
    end
  | Some '/' -> begin
      advance state;
      match peek state with
      | Some c when c = '/' -> begin
          ignore (read_while state (fun c -> c <> '\n'));
          next_token state
        end
      | Some c when c = '*' -> begin
          advance state;
          skip_block_comment state;
          next_token state
        end
      | _ -> TokenSlash
    end
  | Some '=' -> begin
      advance state;
      match peek state with
      | Some '=' -> begin
          advance state;
          TokenEq
        end
      | _ -> TokenAssign
    end
  | Some '!' -> begin
      advance state;
      match peek state with
      | Some '=' -> begin
          advance state;
          TokenNeq
        end
      | _ -> TokenNot
    end
  | Some '<' -> begin
      advance state;
      match peek state with
      | Some '=' -> begin
          advance state;
          TokenLeq
        end
      | _ -> TokenLt
    end
  | Some '>' -> begin
      advance state;
      match peek state with
      | Some '=' -> begin
          advance state;
          TokenGeq
        end
      | _ -> TokenGt
    end
  | Some '&' -> begin
      advance state;
      match peek state with
      | Some '&' -> begin
          advance state;
          TokenAnd
        end
      | _ -> failwith "unexpected character '&'"
    end
  | Some '|' -> begin
      advance state;
      match peek state with
      | Some '|' -> begin
          advance state;
          TokenOr
        end
      | _ -> failwith "unexpected character '|'"
    end
  | Some c -> failwith (Printf.sprintf "unexpected character '%c'" c)
