(* parser.ml  --  second stage of the compiler.
 
   The parser takes the token list from the lexer and builds the AST (the
   tree defined in ast.ml). It also checks the grammar: if the tokens are
   in an order C does not allow, it fails with an error.
 
   Method: recursive descent. There is one function per grammar rule, and
   each function reads the tokens its rule needs from the FRONT of the list.
 
   Every parse_* function has the same shape:
     it takes   a token list
     it returns (the tree it built, the tokens that are LEFT OVER)
   The leftover tokens are handed to the next step. That is how the parser
   moves forward without any mutable state (the lexer needs a mutable
   position, the parser does not). In the code, names like rest, rest' and
   after always mean tokens that are left over.
 
   The grammar this file follows (the names match the functions below):
 
     <program>     ::= { <function> }
     <function>    ::= <type> <identifier> ( <param-list> ) <block>
     <param-list>  ::= void | <parameter> { , <parameter> }
     <parameter>   ::= int <identifier>
     <block>       ::= { { <block-item> } }
     <block-item>  ::= <statement> | <declaration>
     <declaration> ::= int <identifier> [ = <exp> ] ;
     <statement>   ::= return <exp> ;
                     | <exp> ;
                     | if ( <exp> ) <statement> [ else <statement> ]
                     | while ( <exp> ) <statement>
                     | for ( <for-init> [ <exp> ] ; [ <exp> ] ) <statement>
                     | <block>
                     | ;
 
   Not handled yet: function prototypes, function calls, and a bare
   return; with no value.
 
   Main entry point: [parse_program]. *)

(* Checks that the first token is the one we expect, and returns the tokens
   after it (so the matched token is used up). Fails with a message naming
   both tokens when they do not match, or when the list is empty.
   Used for things that must be there, like ; ( ) and so on. *)
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

(* For a token that is a binary operator, gives the operator and its
   priority. For any other token gives None.
   A bigger number binds tighter, so * and / (50) win over + and - (45),
   which win over the comparisons, then ==, then &&, then ||.
   The gaps between the numbers leave room for new operators later.
   Assignment is not in this table because it needs special handling
   (see parse_exp). *)
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

(* Parses an expression using precedence climbing.
 
   [min_priority] is the weakest operator this call is allowed to take.
   An operator weaker than that is NOT taken: the call stops and returns
   what it has, so the caller (which is working at a lower priority) can
   take that operator instead. That is what makes 1 + 2 * 3 come out as
   1 + (2 * 3) and not (1 + 2) * 3.
 
   Start a fresh expression with min_priority 0, so every operator is
   allowed.
 
   Two helpers live inside:
   - parse_factor reads one small piece: a number, a name, a unary
     operator with its operand, or a parenthesised expression.
   - absorb_operators keeps taking operators and right-hand sides after
     that piece, for as long as the operator is strong enough. *)
let rec parse_exp (token_list : Lexer.token_type list) (min_priority : int) :
    Ast.exp * Lexer.token_type list =
  (* This parse factor only handle small piece such as unary operator, a name, a number. *)
  (* Reads the first small piece of an expression: a number, a name, a unary
     operator applied to the next factor, or an expression in parentheses.
     A unary operator calls parse_factor again on what follows, so - - 5
     and !-x both work. *)
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

  (* [left] is the expression built so far. Look at the next token:
     - not an operator, or too weak for [min_priority]: stop and return.
     - an operator that is strong enough: parse its right side, wrap
       left and right into one node, and loop again with that as the new
       left. That loop is what makes 1 - 2 - 3 group as (1 - 2) - 3. *)
  let rec absorb_operators (left : Ast.exp) (tokens : Lexer.token_type list) :
      Ast.exp * Lexer.token_type list =
    match tokens with
    | [] -> (left, tokens)
    | h :: rest ->
        (* Assignment case *)
        (* It is handled apart from the table because it
        has two differences: the left side must be a plain variable, and
        it groups to the RIGHT, so a = b = c means a = (b = c). That is
        why the right side is parsed at priority 1 and not at 1 + 1.
        Assignment has the lowest priority, 1, so the weak check is the
        first guard. *)
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

(* Turns a type token into the AST type. Gives None for a token that is not
   a type, so the caller picks the error message for its own situation. *)
let token_to_tree_type (token : Lexer.token_type) : Ast.var_type option =
  match token with
  | Lexer.TokenInt -> Some Ast.IntType
  | Lexer.TokenVoid -> Some Ast.VoidType
  | _ -> None

(* declaration *)
(* Parses a variable declaration: int name; or int name = exp;
   Only int is allowed, a void variable is an error.
   Declarations are called from two places only: the block loop
   (read_block_item) and the init part of a for loop. *)
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

(* For helper *)

(* Parses an expression that may be missing, followed by [terminator].
   If the first token already is the terminator there is no expression, so
   the result is None and the terminator is consumed. Otherwise parse the
   expression, then require the terminator.
   Used for the three parts of a for loop, which can each be empty:
   the terminator is ; for the first two and ) for the last. *)
let parse_optional_exp (terminator : Lexer.token_type)
    (token_list : Lexer.token_type list) :
    Ast.exp option * Lexer.token_type list =
  begin match token_list with
  | h :: rest when h = terminator -> (None, rest)
  | _ -> begin
      let expression, rest' = parse_exp token_list 0 in
      (Some expression, expect terminator rest')
    end
  end

(* Parses the first part of a for loop, before the first ;
   It is either a declaration (for (int i = 0; ...) ) or an optional
   expression. *)
let parse_init_forloop (token_list : Lexer.token_type list) :
    Ast.for_init * Lexer.token_type list =
  begin match token_list with
  | Lexer.TokenInt :: _ -> begin
      let declare, after = parse_declaration token_list in
      (Ast.InitDeclare declare, after)
    end
  | _ -> begin
      let expression, after =
        parse_optional_exp Lexer.TokenSemiColon token_list
      in
      (Ast.InitExp expression, after)
    end
  end

(* Parses one statement. The first token tells which kind it is:
   return, while, {, if, for, ; or else it must be an expression
   statement. Each case reads its own pieces and returns the tokens left
   over. Declarations are NOT handled here, because C only allows them
   directly inside a block, and the block loop below takes care of that. *)
let rec parse_statement (token_list : Lexer.token_type list) :
    Ast.statement * Lexer.token_type list =
  match token_list with
  | [] -> failwith "Empty token for statement"
  | h :: rest ->
      begin match h with
      (* return <exp> ; a return with no value is not supported yet *)
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
          (* Reads block items until the closing brace. Each item is a
             declaration (it starts with int) or a statement. The items
             are collected in [collected_acc] in reverse order, because
             adding to the front of a list is cheap, so List.rev puts them
             back in file order at the end. *)
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
      | Lexer.TokenFor -> begin
          let init_for, after =
            parse_init_forloop (expect Lexer.TokenLeftParenthesis rest)
          in
          let cond_for, after' =
            parse_optional_exp Lexer.TokenSemiColon after
          in
          let post_for, after'' =
            parse_optional_exp Lexer.TokenRightParenthesis after'
          in
          let for_statement, after_for = parse_statement after'' in
          ( Ast.For
              ( { init = init_for; condition = cond_for; post = post_for },
                for_statement ),
            after_for )
        end
      | Lexer.TokenSemiColon -> (Ast.Null, rest)
      | _ -> begin
          let expression, rest' = parse_exp token_list 0 in
          (Ast.Expression expression, expect Lexer.TokenSemiColon rest')
        end
      end

let parse_parameter (token_list : Lexer.token_type list) :
    Ast.parameter * Lexer.token_type list =
  begin match token_list with
  | [] -> failwith "Expect a type (int) but input ends"
  | h :: rest ->
      begin match token_to_tree_type h with
      | Some Ast.VoidType -> failwith "A variable can not be a void type"
      | None -> failwith "A type was expected"
      | Some Ast.IntType ->
          begin match rest with
          | Lexer.TokenIdentifier_str name :: rest'' -> begin
              ({ parameter_type = IntType; parameter_name = name }, rest'')
            end
          | _ -> failwith "Expect a name for variable"
          end
      end
  end

(* Parses everything between the parentheses of a function. It is either
   the single word void (no parameters, the list is empty) or one parameter
   followed by any number of , parameter pairs. Stops BEFORE the closing
   parenthesis and leaves it for the caller. *)
let parse_param_list (token_list : Lexer.token_type list) :
    Ast.parameter list * Lexer.token_type list =
  begin
    let rec read_comma_params (tokens : Lexer.token_type list)
        (acc : Ast.parameter list) : Ast.parameter list * Lexer.token_type list
        =
      begin match tokens with
      | Lexer.TokenComa :: after -> begin
          let param, after' = parse_parameter after in
          read_comma_params after' (param :: acc)
        end
      | _ -> (List.rev acc, tokens)
      end
    in

    match token_list with
    | Lexer.TokenVoid :: after -> ([], after)
    | Lexer.TokenInt :: _ -> begin
        let param, after = parse_parameter token_list in
        read_comma_params after [ param ]
      end
    | _ -> failwith "Expect type VOID or type INT"
  end

(* Parses one whole function: return type, name, ( parameters ), body.
   The return type may be int or void. The body must be a block. The AST
   stores the statements inside it as a plain list, so the Block wrapper
   is taken off here. Anything else after the parentheses is an error. *)
let parse_function (token_list : Lexer.token_type list) :
    Ast.fun_def * Lexer.token_type list =
  begin match token_list with
  | [] -> failwith "Expect a type (Int) or (Void) but input ends"
  | h :: rest -> begin
      let function_type = token_to_tree_type h in
      match function_type with
      | None -> failwith "Fails because a type was expected"
      | Some vt ->
          begin match rest with
          | Lexer.TokenIdentifier_str name :: after -> begin
              let param_list, after' =
                parse_param_list (expect Lexer.TokenLeftParenthesis after)
              in
              let fun_statement, after'' =
                parse_statement (expect Lexer.TokenRightParenthesis after')
              in
              match fun_statement with
              | Ast.Block items ->
                  ( {
                      fun_type = vt;
                      fun_name = name;
                      fun_parameter = param_list;
                      fun_body = items;
                    },
                    after'' )
              | _ -> failwith "Function body must be a block"
            end
          | _ -> failwith "Expect a name for function"
          end
    end
  end

let parse_program (token_list : Lexer.token_type list) : Ast.program_def =
  begin
    let rec parse_program_rec (token_list : Lexer.token_type list)
        (acc : Ast.fun_def list) : Ast.program_def =
      begin match token_list with
      | [] -> { fun_defs = List.rev acc }
      | _ -> begin
          let func, after = parse_function token_list in
          parse_program_rec after (func :: acc)
        end
      end
    in
    parse_program_rec token_list []
  end
