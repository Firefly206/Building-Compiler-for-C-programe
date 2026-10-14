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

let token_to_str (token : token_type) : string =
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
(* Produces exactly one token starting at the current position in [state].
   Handles: skipping whitespace and both comment styles (//, /* */),
   integer constants, identifiers/keywords, single-char punctuation,
   and operators that may be one or two characters (=, ==, <, <=, etc).
   Raises an error on any unrecognized character. Recurses on itself
   for whitespace/comments, since those don't produce a token directly. *)
let rec next_token (state : lexer_state) : token_type =
  let is_digit (c : char) : bool = c >= '0' && c <= '9' in
  let is_letter (c : char) : bool =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  in
  let is_identifier_char (c : char) : bool =
    c = '_' || is_letter c || is_digit c
  in
  (* Consumes a block comment's body and its closing */.
   Assumes the caller has already consumed the opening /* — this function
   only looks for the close. Three cases: hitting end of file before
   finding */ is an error (unterminated comment); finding * immediately
   followed by / consumes both and stops; anything else just advances
   past one character and keeps looking. *)
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
      match peek state with
      | Some c when is_identifier_char c ->
          failwith
            "Invalid_identifier: Names must begin with a letter or an \
             underscore (_). Found likely: a number directly followed by a \
             letter or underscore"
      | _ -> TokenConstant_int (int_of_string str)
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

(* Repeatedly calls next_token to collect every token in [state] into a
   list. Uses an accumulator (acc) so the recursion is tail-recursive —
   each token gets prepended (::) rather than appended, which is why the
   final accumulated list is in reverse order and needs List.rev at the
   end. TokenEOF itself is not included in the returned list. *)
let tokenize (state : lexer_state) : token_type list =
  begin
    let rec acc_tokenize (state : lexer_state) (acc : token_type list) :
        token_type list =
      match next_token state with
      | TokenEOF -> acc
      | tken -> acc_tokenize state (tken :: acc)
    in
    (* acc_tokenize state [] *)
    List.rev (acc_tokenize state [])
  end
