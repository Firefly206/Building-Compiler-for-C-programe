type unary_operator = Negate | Not

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

type exp =
  | Constant of int
  | Variable of string
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
  | Assignment of exp * exp

type var_type = IntType | VoidType

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
