type unary_operator = Negate | Not

let unary_op_to_str (unary_op : unary_operator) : string =
  match unary_op with Negate -> " Negate " | Not -> " Not "

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
  | And
  | Or

let bin_op_to_str (binop : binary_operator) : string =
  match binop with
  | Add -> " Add "
  | Subtract -> " Subtract "
  | Multiply -> " Multiply "
  | Divide -> " Divide "
  | Equal -> " Equal "
  | NotEqual -> " NotEqual "
  | LessThan -> " LessThan "
  | LessOrEqual -> " LessOrEqual "
  | GreaterThan -> " GreaterThan "
  | GreaterOrEqual -> " GreaterOrEqual "
  | And -> " And "
  | Or -> " Or "

type exp =
  | Constant of int
  | Variable of string
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
  | Assignment of exp * exp

let label (expression : exp) : string =
  match expression with
  | Constant n -> "Constant(" ^ string_of_int n ^ ")"
  | Variable str -> "Variable(" ^ str ^ ")"
  | Unary (op, _) -> "Unary(" ^ unary_op_to_str op ^ ")"
  | Binary (op, _, _) -> "Binary(" ^ bin_op_to_str op ^ ")"
  | Assignment (_, _) -> "Assignment"

let inner_exp_fun (expression : exp) : exp list =
  match expression with
  | Constant _ -> []
  | Variable _ -> []
  | Unary (_, inner) -> [ inner ]
  | Binary (_, left, right) -> [ left; right ]
  | Assignment (left, right) -> [ left; right ]

let rec printer (expression : exp) (num_spaces : int) : string =
  let current_line = String.make num_spaces ' ' ^ label expression in
  let inner_exps_str =
    List.map
      (fun (inner : exp) -> printer inner (num_spaces + 2))
      (inner_exp_fun expression)
  in
  String.concat "\n" (current_line :: inner_exps_str)

type var_type = IntType | VoidType

let var_type_to_str (vt : var_type) : string =
  begin match vt with IntType -> "int" | VoidType -> "void"
  end

type declaration = {
  decl_type : var_type;
  name : string;
  init_val : exp option;
}

type for_init = InitDeclare of declaration | InitExp of exp option
type for_loop = { init : for_init; condition : exp option; post : exp option }

type statement =
  | Return of exp
  | Expression of exp
  | If of exp * statement * statement option
  | While of exp * statement
  | Declaration of declaration
  | Block of statement list
  | For of for_loop * statement
  | Null

type parameter = { parameter_type : var_type; parameter_name : string }

type fun_def = {
  fun_type : var_type;
  fun_name : string;
  fun_parameter : parameter list;
  fun_body : statement list;
}

type program_def = { fun_defs : fun_def list }
