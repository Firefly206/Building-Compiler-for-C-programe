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

let read_while (state : lexer_state) (condition : bool ) : string =
    let buff = Buffer.create 16 in
    let rec loop () = 
        match peek state with
        | 



