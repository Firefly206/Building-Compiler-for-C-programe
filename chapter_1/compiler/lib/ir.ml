type value = Num of int | Name of string

type instruction =
  | Returni of value
  | Unaryi of Ast.unary_operator * value * value (* one step *)

let create_tmp_name (n : int) : value * int =
  begin
    (Name ("tmp." ^ string_of_int n), n + 1)
  end

let flattening_exp (expression : Ast.exp) (counter : int) :
    instruction list * value * int =
  begin match expression with
  | Ast.Constant n -> ([], Num n, counter)
  | Ast.Variable str -> ([], Name str, counter)
  | Ast.Unary (op, inner) -> begin end
  | _ -> failwith "not yet"
  end
