type value = Num of int | Name of string

(* step does one thing: compute a value and store it in a name, or return a value *)
type step =
  | Returni of value
  (* apply op to the value first value, and store the result in the second value *)
  | Unaryi of Ast.unary_operator * value * value
  (* Binary (operator, left_value, right_value, store/destination)*)
  | Binaryi of Ast.binary_operator * value * value * value

(* record type for one IR function with four fields: name, return type, parameters (a string list), and body (a step list) *)
type fun_def_in_IR = {
  fun_type_ir : Ast.var_type;
  fun_name_ir : string;
  fun_parameter_ir : string list;
  fun_body_ir : step list;
}

let create_tmp_name (n : int) : value * int =
  begin
    (Name ("tmp." ^ string_of_int n), n + 1)
  end

let value_to_str (v : value) : string =
  begin match v with Num n -> string_of_int n | Name name -> name
  end

let step_to_str (s : step) : string =
  begin match s with
  | Returni v ->
      begin match v with
      | Num n -> "Return " ^ string_of_int n
      | Name name -> "Return " ^ name
      end
  | Unaryi (op, first, second) -> begin
      value_to_str second ^ " =" ^ Ast.unary_op_to_str op ^ value_to_str first
    end
  | Binaryi (op, left, right, destination) -> begin
      value_to_str destination ^ " = " ^ value_to_str left
      ^ Ast.bin_op_to_str op ^ value_to_str right
    end
  end

let rec flattening_exp (expression : Ast.exp) (counter : int) :
    step list * value * int =
  begin match expression with
  | Ast.Constant n -> ([], Num n, counter)
  | Ast.Variable str -> ([], Name str, counter)
  | Ast.Unary (op, inner) -> begin
      let lst, v, counter' = flattening_exp inner counter in
      let tmp_name_val, counter'' = create_tmp_name counter' in
      (* [ Unaryi (op, value, value) ] is the outer shell, 
         and we want to proccess inner -> outer shell, that's why
         lst @ [ Unaryi () ] *)
      (lst @ [ Unaryi (op, v, tmp_name_val) ], tmp_name_val, counter'')
    end
  | Ast.Binary (Ast.And, _, _) -> failwith "TODO"
  | Ast.Binary (Ast.Or, _, _) -> failwith "TODO"
  | Ast.Binary (op, left, right) -> begin
      let left_lst, left_v, leftc' = flattening_exp left counter in
      let right_lst, right_v, rightc' = flattening_exp right leftc' in
      let destination_val, counter'' = create_tmp_name rightc' in
      ( left_lst @ right_lst @ [ Binaryi (op, left_v, right_v, destination_val) ],
        destination_val,
        counter'' )
    end
  | _ -> failwith "not yet"
  end

let flattening_statement (s : Ast.statement) (c : int) : step list * int =
  begin match s with
  | Ast.Return e -> begin
      let lst, v, c' = flattening_exp e c in
      (lst @ [ Returni v ], c')
    end
  | _ -> failwith "TODO"
  end

let rec flattening_list_statement (lst_s : Ast.statement list) (c : int) :
    step list * int =
  begin match lst_s with
  | [] -> ([], c)
  | h :: rest -> begin
      let first_lst_step, c' = flattening_statement h c in
      let rest_of_lst, c'' = flattening_list_statement rest c' in
      (first_lst_step @ rest_of_lst, c'')
    end
  end

let flattening_function (f : Ast.fun_def) : fun_def_in_IR =
  begin
    {
      fun_type_ir = f.fun_type;
      fun_name_ir = f.fun_name;
      fun_parameter_ir =
        List.map
          (fun (t : Ast.parameter) : string -> t.parameter_name)
          f.fun_parameter;
      fun_body_ir =
        begin
          let steps, _ = flattening_list_statement f.fun_body 0 in
          steps
        end;
    }
  end

let flattening_program (p : Ast.program_def) : fun_def_in_IR list =
  begin
    List.map flattening_function p.fun_defs
  end
