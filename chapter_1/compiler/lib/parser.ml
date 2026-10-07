open Lexer
open Ast

let expect (token_expect : Lexer.token_type)
    (token_list : Lexer.token_type list) : Lexer.token_type list =
  match token_list with
  | [] -> failwith "Empty token list"
  | h :: t ->
      begin if token_expect = h then t
      else
        failwith
          (Lexer.token_to_str token_expect
          ^ " is expected. But got " ^ Lexer.token_to_str h ^ " instead")
      end

(* the binary operator for this token *)
let binop_of_token (token : Lexer.token_type) :
    (Ast.binary_operator * int) option =
  match token with
  | Lexer.TokenStar -> Some (Ast.Multiply, 50)
  | Lexer.TokenSlash -> Some (Ast.Divide, 50)
  | Lexer.TokenPlus -> Some (Ast.Add, 45)
  | Lexer.TokenMinus -> Some (Ast.Subtract, 45)
  | Lexer.TokenLt -> Some (Ast.LessThan, 35)
  | Lexer.TokenLeq -> Some (Ast.LessOrEqual, 35)
  | Lexer.TokenGt -> Some (Ast.GreaterThan, 35)
  | Lexer.TokenGeq -> Some (Ast.GreaterOrEqual, 35)
  | Lexer.TokenEq -> Some (Ast.Equal, 30)
  | Lexer.TokenNeq -> Some (Ast.NotEqual, 30)
  | Lexer.TokenAnd -> Some (Ast.And, 10)
  | Lexer.TokenOr -> Some (Ast.Or, 5)
  | _ -> None

(* *)
let rec parse_exp (token_list : Lexer.token_type list) (min_priority : int) :
    Ast.exp * Lexer.token_type list =
  (* This parse factor only handle small piece such as unary operator, a name, a number.
         It process first peace of expression. *)
  let rec parse_factor (token_list_smaller : Lexer.token_type list) :
      Ast.exp * Lexer.token_type list =
    match token_list_smaller with
    | [] -> failwith "Empty token list"
    | h :: rest ->
        begin match h with
        | Lexer.TokenConstant_int n -> (Ast.Constant n, rest)
        | Lexer.TokenMinus -> begin
            let inner, rest' = parse_factor rest in
            (Ast.Unary (Ast.Negate, inner), rest')
          end
        (* Drop the positive sign *)
        | Lexer.TokenPlus -> begin
            let inner, rest' = parse_factor rest in
            (inner, rest')
          end
        | Lexer.TokenNot -> begin
            let inner, rest' = parse_factor rest in
            (Ast.Unary (Ast.Not, inner), rest')
          end
        | Lexer.TokenIdentifier_str variable -> (Ast.Variable variable, rest)
        | Lexer.TokenLeftParenthesis -> begin
            let inner, rest' = parse_exp rest 0 in
            let rest'' = expect Lexer.TokenRightParenthesis rest' in
            (inner, rest'')
          end
        | _ -> failwith "Not in the list of known expression"
        end
  in

  let rec absorb_operators (left : Ast.exp) (tokens : Lexer.token_type list) :
      Ast.exp * Lexer.token_type list =
    match tokens with
    | [] -> (left, tokens)
    | h :: rest ->
        (* Assignment case *)
        begin match h with
        | Lexer.TokenAssign ->
            begin match left with
            | _ when 1 < min_priority -> (left, tokens)
            | Ast.Variable _ -> begin
                let right, rest' = parse_exp rest 1 in
                absorb_operators (Ast.Assignment (left, right)) rest'
              end
            | _ -> failwith "Left side for Assignment must be Variable"
            end
        | _ ->
            (* a weak operator makes the inner call stop and return what it has *)
            begin match binop_of_token h with
            | None -> (left, tokens)
            | Some (_, priority) when priority < min_priority -> (left, tokens)
            | Some (op, priority) -> begin
                let right, rest' = parse_exp rest (priority + 1) in
                absorb_operators (Ast.Binary (op, left, right)) rest'
              end
            end
        end
  in

  let left, rest = parse_factor token_list in
  absorb_operators left rest

(* declaration helper *)
let token_to_tree_type (token : Lexer.token_type) : Ast.var_type option =
  match token with
  | Lexer.TokenInt -> Some Ast.IntType
  | Lexer.TokenVoid -> Some Ast.VoidType
  | _ -> None

(* declaration *)
let parse_declaration (token_list : Lexer.token_type list) :
    Ast.declaration * Lexer.token_type list =
  match token_list with
  | [] -> failwith "Expect a type (int) here but the input ends abruptly"
  | h :: rest ->
      begin match token_to_tree_type h with
      | Some Ast.VoidType -> failwith "A variable can not be a void type"
      | None -> failwith "Fails because a type was expected"
      | Some Ast.IntType ->
          begin match rest with
          | Lexer.TokenIdentifier_str name :: rest'' ->
              begin match rest'' with
              | [] ->
                  failwith
                    "Expected = or ; after the variable name, but the input \
                     ended"
              | Lexer.TokenAssign :: after -> begin
                  let inner, after' = parse_exp after 0 in
                  ( { decl_type = Ast.IntType; name; init_val = Some inner },
                    expect Lexer.TokenSemiColon after' )
                end
              | Lexer.TokenSemiColon :: after ->
                  ({ decl_type = Ast.IntType; name; init_val = None }, after)
              | h :: _ ->
                  failwith
                    ("Expected = or ; after the variable name, but got "
                   ^ Lexer.token_to_str h ^ " instead")
              end
          | _ -> failwith "Expect a name for variable"
          end
      end

let rec parse_statement (token_list : Lexer.token_type list) :
    Ast.statement * Lexer.token_type list =
  match token_list with
  | [] -> failwith "Empty token for statement"
  | h :: rest ->
      begin match h with
      | Lexer.TokenReturn ->
          begin match rest with
          | [] -> failwith "Empty return body"
          | Lexer.TokenSemiColon :: _ ->
              failwith "No expression nor void. Example: <return ;>"
          | _ -> begin
              let inner, rest' = parse_exp rest 0 in
              (Ast.Return inner, expect Lexer.TokenSemiColon rest')
            end
          end
      | Lexer.TokenWhile -> begin
          let inner, rest' =
            parse_exp (expect Lexer.TokenLeftParenthesis rest) 0
          in
          let inner', rest'' =
            parse_statement (expect Lexer.TokenRightParenthesis rest')
          in
          (Ast.While (inner, inner'), rest'')
        end
      | Lexer.TokenLeftBrace -> begin
          let rec read_block_item (tokens : Lexer.token_type list)
              (collected_acc : Ast.statement list) :
              Ast.statement * Lexer.token_type list =
            match tokens with
            | [] -> failwith "Unclosed block, missing right braces"
            | Lexer.TokenRightBrace :: rest' ->
                (Ast.Block (List.rev collected_acc), rest')
            | Lexer.TokenInt :: _ -> begin
                let inner, after_decl = parse_declaration tokens in
                read_block_item after_decl
                  (Ast.Declaration inner :: collected_acc)
              end
            | _ -> begin
                let statement, rest' = parse_statement tokens in
                read_block_item rest' (statement :: collected_acc)
              end
          in

          read_block_item rest []
        end
      | Lexer.TokenIf -> begin
          let exp_inner, rest' =
            parse_exp (expect Lexer.TokenLeftParenthesis rest) 0
          in
          let statement_inner, rest'' =
            parse_statement (expect Lexer.TokenRightParenthesis rest')
          in
          match rest'' with
          | Lexer.TokenElse :: else_statement -> begin
              let else_inner, after = parse_statement else_statement in
              (Ast.If (exp_inner, statement_inner, Some else_inner), after)
            end
          | _ -> (Ast.If (exp_inner, statement_inner, None), rest'')
        end
      | Lexer.TokenSemiColon -> (Ast.Null, rest)
      | _ -> begin
          let expression, rest' = parse_exp token_list 0 in
          (Ast.Expression expression, expect Lexer.TokenSemiColon rest')
        end
      end
