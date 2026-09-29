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

(* This is function that peek/look at the current position of the string and return the character there.
   Option here to handle the case where we reach EOF (meaning nothing get return)*)
let peek (state : lexer_state) : char option =
  if state.position < String.length state.source then
    Some (String.get state.source state.position)
  else None

(* This is to increase the position/pointer at the string *)
let advance (state : lexer_state) : unit = state.position <- state.position + 1

(* This is a fun that takes state and fun condition and return string or rather the word to tokenize.
 First we create a buffer so we add char to that buffer build up the string, instead of copy the string and add char after that,
 increase the pointer using advance, and then loop back. Loop takes no arguements and return nothing, that's why we use unit type.*)
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
