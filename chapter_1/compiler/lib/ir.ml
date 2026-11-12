type value = Num of int | Name of string

(* step does one thing: compute a value and store it in a name, or return a value *)
type step =
  | Returni of value
  (* apply op to the value first value, and store the result in the second value *)
  | Unaryi of Ast.unary_operator * value * value
  (* Binary (operator, left_value, right_value, store/destination)*)
  | Binaryi of Ast.binary_operator * value * value * value
  (* Copyi (source value, destination value) *)
  | Copyi of value * value
  (* jump to a label (string) *)
  | Jumpi of string
  (* jump if a value is zero. *)
  | JumpIfZeroi of value * string
  (* Same thing as JumpIfZeroi. Meaning in the name *)
  | JumpIfNotZeroi of value * string
  (* mark a label *)
  | Labeli of string

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

let create_label_name (prefix : string) (counter : int) : string * int =
  begin
    (prefix ^ "_" ^ string_of_int counter, counter + 1)
  end

let value_to_str (v : value) : string =
  begin match v with Num n -> string_of_int n | Name name -> name
  end

let step_to_str (s : step) : string =
  begin match s with
  | Returni v -> value_to_str v
  | Unaryi (op, first, second) -> begin
      value_to_str second ^ " = " ^ Ast.unary_op_to_str op ^ value_to_str first
    end
  | Binaryi (op, left, right, destination) -> begin
      value_to_str destination ^ " = " ^ value_to_str left
      ^ Ast.bin_op_to_str op ^ value_to_str right
    end
  | Copyi (src, dst) -> begin value_to_str dst ^ " = " ^ value_to_str src end
  | Jumpi end_label -> "jump " ^ end_label
  | JumpIfZeroi (v, end_label) ->
      "jump_if_zero " ^ value_to_str v ^ " " ^ end_label
  | JumpIfNotZeroi (v, end_label) ->
      "jump_if_not_zero " ^ value_to_str v ^ " " ^ end_label
  | Labeli end_label -> end_label ^ ":"
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
  | Ast.Binary (Ast.And, left, right) -> begin
      let left_lst, left_v, leftc' = flattening_exp left counter in
      let right_lst, right_v, rightc' = flattening_exp right leftc' in
      let false_label, counter'' = create_label_name "and_false" rightc' in
      let end_label, counter''' = create_label_name "and_end" counter'' in
      let result, after = create_tmp_name counter''' in
      ( left_lst
        @ [ JumpIfZeroi (left_v, false_label) ]
        @ right_lst
        @ [
            JumpIfZeroi (right_v, false_label);
            Copyi (Num 1, result);
            Jumpi end_label;
            Labeli false_label;
            Copyi (Num 0, result);
            Labeli end_label;
          ],
        result,
        after )
    end
  | Ast.Binary (Ast.Or, left, right) -> begin
      let left_lst, left_v, leftc' = flattening_exp left counter in
      let right_lst, right_v, rightc' = flattening_exp right leftc' in
      let true_label, counter'' = create_label_name "or_true" rightc' in
      let end_label, counter''' = create_label_name "or_end" counter'' in
      let result, after = create_tmp_name counter''' in
      ( left_lst
        @ [ JumpIfNotZeroi (left_v, true_label) ]
        @ right_lst
        @ [
            JumpIfNotZeroi (right_v, true_label);
            Copyi (Num 0, result);
            Jumpi end_label;
            Labeli true_label;
            Copyi (Num 1, result);
            Labeli end_label;
          ],
        result,
        after )
    end
  | Ast.Binary (op, left, right) -> begin
      let left_lst, left_v, leftc' = flattening_exp left counter in
      let right_lst, right_v, rightc' = flattening_exp right leftc' in
      let destination_val, counter'' = create_tmp_name rightc' in
      ( left_lst @ right_lst @ [ Binaryi (op, left_v, right_v, destination_val) ],
        destination_val,
        counter'' )
    end
  | Ast.Assignment (Ast.Variable name, right) -> begin
      let right_lst, right_v, rightc' = flattening_exp right counter in
      (right_lst @ [ Copyi (right_v, Name name) ], Name name, rightc')
    end
  | Ast.Assignment (_, _) ->
      failwith
        "flattening_exp: assignment to something that is not a variable; the \
         parser should have rejected this"
  end

let rec flattening_statement (s : Ast.statement) (c : int) : step list * int =
  begin match s with
  | Ast.Return e -> begin
      let lst, v, c' = flattening_exp e c in
      (lst @ [ Returni v ], c')
    end
  | Ast.Null -> ([], c)
  | Ast.Expression e -> begin
      let lst, _, c' = flattening_exp e c in
      (lst, c')
    end
  | Ast.Declaration d -> begin
      begin match d.init_val with
      | Some e -> begin
          let right_lst, right_v, rightc' = flattening_exp e c in
          (right_lst @ [ Copyi (right_v, Name d.name) ], rightc')
        end
      | None -> ([], c)
      end
    end
  | Ast.Block lst -> flattening_list_statement lst c
  | Ast.If (cond, then_s, None) -> begin
      let lst_cond, v, c' = flattening_exp cond c in
      let end_label, c'' = create_label_name "if_end" c' in
      let lst_statement, c''' = flattening_statement then_s c'' in
      ( lst_cond
        @ [ JumpIfZeroi (v, end_label) ]
        @ lst_statement @ [ Labeli end_label ],
        c''' )
    end
  | Ast.If (cond, then_s, Some else_s) -> begin
      let lst_cond, v, c' = flattening_exp cond c in
      let then_steps, c'' = flattening_statement then_s c' in
      let else_label, after = create_label_name "if_else" c'' in
      let else_steps, after' = flattening_statement else_s after in
      let end_label, after'' = create_label_name "if_end" after' in
      ( lst_cond
        @ [ JumpIfZeroi (v, else_label) ]
        @ then_steps @ [ Jumpi end_label ] @ [ Labeli else_label ] @ else_steps
        @ [ Labeli end_label ],
        after'' )
    end
  | _ -> failwith "TODO"
  end

(* "and" here is not logical and: it means "defined together with the
   function above". flattening_statement calls flattening_list_statement
   (for a Block) and flattening_list_statement calls flattening_statement
   (for each statement), so each must be able to see the other. *)
and flattening_list_statement (lst_s : Ast.statement list) (c : int) :
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

let fun_ir_to_str (f : fun_def_in_IR) : string =
  begin
    let header =
      Ast.var_type_to_str f.fun_type_ir
      ^ " " ^ f.fun_name_ir ^ " ("
      ^ String.concat ", " f.fun_parameter_ir
      ^ ") :\n"
    in
    let body =
      List.map (fun (s : step) : string -> "  " ^ step_to_str s) f.fun_body_ir
    in
    header ^ String.concat "\n" body
  end
