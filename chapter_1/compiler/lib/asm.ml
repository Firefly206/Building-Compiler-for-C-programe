type reg = EAX | RBP | RSP

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
  | Push of operand
  | Pop of operand
  | Sub of operand * operand
  | Ret

let operand_of_value (v : Ir.value) : operand =
  begin match v with Ir.Num n -> Number n | Ir.Name s -> Pseudo s
  end

let instructions_of_step (step : Ir.step) : instruction list =
  begin match step with
  | Ir.Returni v ->
      [
        Mov (operand_of_value v, Reg EAX);
        Mov (Reg RBP, Reg RSP);
        Pop (Reg RBP);
        Ret;
      ]
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

let replace_operand (o : operand) (seen : (string * int) list) :
    operand * (string * int) list =
  begin match o with
  | Number _ -> (o, seen)
  | Reg _ -> (o, seen)
  | Stack b -> (o, seen)
  | Pseudo s ->
      begin match List.assoc_opt s seen with
      | Some offset -> (Stack offset, seen)
      | None -> begin
          let offset = (List.length seen + 1) * -4 in
          (Stack offset, (s, offset) :: seen)
        end
      end
  end

let replace_instruction (i : instruction) (seen : (string * int) list) :
    instruction * (string * int) list =
  begin match i with
  | Mov (src, dst) -> begin
      let new_src, seen' = replace_operand src seen in
      let new_dst, seen'' = replace_operand dst seen' in
      (Mov (new_src, new_dst), seen'')
    end
  | Neg op -> begin
      let new_op, seen' = replace_operand op seen in
      (Neg new_op, seen')
    end
  | Push op -> begin
      let new_op, seen' = replace_operand op seen in
      (Push new_op, seen')
    end
  | Pop op -> begin
      let new_op, seen' = replace_operand op seen in
      (Pop new_op, seen')
    end
  | Sub (src, dst) -> begin
      let new_src, seen' = replace_operand src seen in
      let new_dst, seen'' = replace_operand dst seen' in
      (Sub (new_src, new_dst), seen'')
    end
  | Ret -> (i, seen)
  end

let rec replace_body (instructions : instruction list)
    (seen : (string * int) list) : instruction list * (string * int) list =
  begin match instructions with
  | [] -> (instructions, seen)
  | h :: rest -> begin
      let i, seen' = replace_instruction h seen in
      let rest', seen'' = replace_body rest seen' in
      (* A' :: B' :: C' :: [] *)
      (i :: rest', seen'')
    end
  end

let fun_ir_to_asm (f : Ir.fun_def_in_IR) : asm_fun =
  begin
    let body = List.concat (List.map instructions_of_step f.fun_body_ir) in
    let new_body, seen = replace_body body [] in
    let bytes = List.length seen * 4 in
    {
      fun_name_asm = f.fun_name_ir;
      fun_body_asm =
        [ Push (Reg RBP); Mov (Reg RSP, Reg RBP); Sub (Number bytes, Reg RSP) ]
        @ new_body;
    }
  end

let asm_of_program (p : Ir.fun_def_in_IR list) : asm_fun list =
  begin
    List.map fun_ir_to_asm p
  end

let operand_to_str (o : operand) : string =
  begin match o with
  | Number n -> string_of_int n
  | Reg r ->
      begin match r with EAX -> "eax" | RBP -> "rbp" | RSP -> "rsp"
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
  | Push o -> "    push " ^ operand_to_str o
  | Pop o -> "    pop " ^ operand_to_str o
  | Sub (src, dst) ->
      "    sub " ^ operand_to_str dst ^ ", " ^ operand_to_str src
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
