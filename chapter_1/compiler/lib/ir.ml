type value = Num of int | Name of string

(* step does one thing: compute a value and store it in a name, or return a value *)
type step = Returni of value | Unaryi of Ast.unary_operator * value * value
(* apply op to the value first value, and store the result in the second value *)

let create_tmp_name (n : int) : value * int =
  begin
    (Name ("tmp." ^ string_of_int n), n + 1)
  end

let rec flattening_exp (expression : Ast.exp) (counter : int) :
    step list * value * int =
  begin match expression with
  | Ast.Constant n -> ([], Num n, counter)
  | Ast.Variable str -> ([], Name str, counter)
  | Ast.Unary (op, inner) -> begin
      let lst, v, counter' = flattening_exp inner counter in
      let tmp_name_val, counter'' = create_tmp_name counter' in
      (lst @ [ Unaryi (op, v, tmp_name_val) ], tmp_name_val, counter'')
    end
  | _ -> failwith "not yet"
  end
