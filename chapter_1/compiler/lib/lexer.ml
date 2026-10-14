(* lexer.ml  --  first stage of the compiler.
 
   The lexer reads the C source file as one big string and cuts it into a
   list of tokens. A token is the smallest piece, like a word/number, example
   the keyword return, the number 42, or the symbol ;.
 
   Spaces, newlines and comments are thrown away here, so the parser never
   sees them.
 
   Main entry point: [tokenize]. Everything above it is a helper. *)

(* This is a token constructor. One constructor per kind of token.
   Two of them carry data: an identifier carries its name and a constant
   carries its number. The rest are just a tag *)
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
(* marks the end of the input. [tokenize] stops at it and does not
     put it in the list. *)

(* Turns a token into text, for printing with --lex and for error messages
   in the parser. Every case is listed on purpose: if you add a new token
   and forget it here, the compiler warns you. *)
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

(* Where the lexer is in the file.
   [source] is the whole file as one string and never changes.
   [position] is the index of the next character to read.
 
   [position] is mutable on purpose. It is the one place in the compiler
   that uses mutable state: every helper below moves the same pointer
   forward, and passing a new state around for every single character
   would be a lot of noise. The parser does not do this. It passes the
   leftover tokens around instead. *)
type lexer_state = { source : string; mutable position : int }

(* This is function that peek/look at the CURRENT position of the string without moving and return the character there.
   Option here to handle the case where we reach EOF, returns None at the end of the file, so the caller has to handle
   that case.*)
let peek (state : lexer_state) : char option =
  if state.position < String.length state.source then
    Some (String.get state.source state.position)
  else None

(* Same as [peek] but looks one character ahead. Needed where one character
   is not enough to decide, like the * / that closes a block comment. *)
let peek_next (state : lexer_state) : char option =
  if state.position + 1 < String.length state.source then
    Some (String.get state.source (state.position + 1))
  else None

(* This is to increase or moves the position/pointer one character forward. It does not check the end of
   the file, so only call it after [peek] returned Some. *)
let advance (state : lexer_state) : unit = state.position <- state.position + 1

(* This fun were create to consume characters. 
  It reads characters for as long as [condition] is true and returns them as
   one string. Used for numbers (condition: is a digit) and for names
   (condition: is a letter, digit or underscore). It also skips comment text.
 
   It stops at the first character that fails the condition, or at the end
   of the file, and leaves the position on that character. Nothing is
   consumed past it.
 
   [loop] takes unit and returns unit because its only job is to move the
   position and fill the buffer. The result is read from the buffer after
   it finishes. *)
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

(* Reads exactly one token, starting at the current position, and moves the
   position to just after it.
 
   Whitespace and comments (both // and block comments) are skipped by
   calling [next_token] again, because they do not produce a token.
 
   Where it decides:
   - a digit starts a number. If a letter or underscore comes right after
     the digits (like 1foo) it fails, because that is not a valid number.
     A number too big for an OCaml int also fails, inside int_of_string.
   - a letter or underscore starts a name. The name is checked against the
     keyword list first, and anything else becomes an identifier.
   - one character tokens (like ; and +) are returned right away.
   - = ! < > can be one or two characters, so the next character is
     checked: = then = is TokenEq, but = alone is TokenAssign. A lone ! is
     TokenNot and != is TokenNeq, and the same idea works for < and >.
   - & and | only exist in pairs (&& and ||). A single one is an error
     because this compiler has no bitwise operators yet.
   - anything else is an unexpected character error.
 
   Returns TokenEOF at the end of the file. *)
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
  (* a slash can be division or the start of a comment, so look at the next
     character: another slash is a line comment (skip to the end of the
     line), a star is a block comment, anything else is plain division *)
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

(* Turns the whole source into a list of tokens, in the same order as they
   appear in the file. TokenEOF is not in the list.
 
   Calls [next_token] over and over until it returns TokenEOF. The helper
   uses an accumulator: each new token is put on the FRONT of the list,
   because adding to the front of an OCaml list is cheap and adding to the
   back is not. That leaves the list backwards, so List.rev flips it once
   at the end. The parser uses the same trick in read_block_item.
 
   Fails if the source has something [next_token] rejects, such as an
   unknown character or an unterminated block comment. *)
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
