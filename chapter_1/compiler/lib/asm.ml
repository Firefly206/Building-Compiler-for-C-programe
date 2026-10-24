type reg = EAX

type operand =
  | Number of int
  | Reg of reg
  | Pseudo of string
  (* stack carry offset rbp *)
  | Stack of int

type instruction =
  (* source to destination *)
  | Mov of operand * operand
  | Neg of operand
  | Ret

let operand_of_value (v : Ir.value) : operand =
  begin match v with Ir.Num n -> Number n | Ir.Name s -> Pseudo s
  end

let instructions_of_step (step : Ir.step) : instruction list =
  begin match step with
  | Ir.Returni v -> [ Mov (operand_of_value v, Reg EAX); Ret ]
  | Ir.Unaryi (op, first, second) ->
      begin match op with
      | Ast.Negate ->
          [
            Mov (operand_of_value first, operand_of_value second);
            Neg (operand_of_value second);
          ]
      | Ast.Not -> failwith "TODO"
      end
  | Ir.Binaryi (op, left_value, right_value, dst) -> failwith "TODO"
  end

type asm_fun = { fun_name_asm : string; fun_body_asm : instruction list }

let fun_ir_to_asm (f : Ir.fun_def_in_IR) : asm_fun =
  begin
    let body = List.concat (List.map instructions_of_step f.fun_body_ir) in
    { fun_name_asm = f.fun_name_ir; fun_body_asm = body }
  end

let asm_of_program (p : Ir.fun_def_in_IR list) : asm_fun list =
  begin
    List.map fun_ir_to_asm p
  end

let operand_to_str (o : operand) : string =
  begin match o with
  | Number n -> string_of_int n
  | Reg r ->
      begin match r with EAX -> "eax"
      end
  (* dword is 4 bytes, size of int *)
  | Stack b -> "dword [rbp" ^ string_of_int b ^ "]"
  | Pseudo s ->
      failwith "this should never happen: pseudoregister reached printer"
  end

let instruction_to_str (i : instruction) : string =
  begin match i with
  (* NASM writes the destination first *)
  | Mov (src, dst) ->
      "    mov " ^ operand_to_str dst ^ ", " ^ operand_to_str src
  | Neg o -> "    neg " ^ operand_to_str o
  | Ret -> "    ret"
  end

let asm_fun_to_str (f : asm_fun) : string =
  begin
    "global " ^ f.fun_name_asm ^ "\n" ^ f.fun_name_asm ^ ":\n"
    ^ String.concat "\n" (List.map instruction_to_str f.fun_body_asm)
    ^ "\n"
  end

let asm_program_to_str (p : asm_fun list) : string =
  begin
    "section .text\n" ^ String.concat "\n" (List.map asm_fun_to_str p) ^ "\n"
  end
