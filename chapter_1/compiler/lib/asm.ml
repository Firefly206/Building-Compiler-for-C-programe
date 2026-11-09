type reg = EAX | RBP | RSP | R10 | R11 | DX

type operand =
  | Number of int
  | Reg of reg
  | Pseudo of string
  (* stack carry offset rbp *)
  | Stack of int

type operand_size = Byte | Dword

(* condition for SetCC (and later JmpCC for if/while).
   The printer turns each one into the suffix of the real instruction:
   E -> sete, NE -> setne, L -> setl, LE -> setle, G -> setg, GE -> setge *)
type comparison_cond = E | NE | L | LE | G | GE

type instruction =
  (* source to destination *)
  | Mov of operand * operand
  | Neg of operand
  | Push of operand
  | Pop of operand
  | Sub of operand * operand
  | Add of operand * operand
  | Mul of operand * operand
  | Cdq
  | Idiv of operand
  (* source first, like Sub. Cmp (right, left) prints "cmp left, right"
     and computes left - right, then sets the flags *)
  | Cmp of operand * operand
  (* writes 0 or 1 into ONE byte of the operand. Our values are 4 bytes,
     so we do "mov dst, 0" first. The printer must use the 1-byte name:
     al / r10b for registers, "byte [rbp-4]" for a stack slot *)
  | SetCC of (comparison_cond * operand)
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
  | Ir.Binaryi (op, left_value, right_value, dst) ->
      begin match op with
      | Ast.Add ->
          [
            Mov (operand_of_value left_value, operand_of_value dst);
            Add (operand_of_value right_value, operand_of_value dst);
          ]
      | Ast.Subtract ->
          [
            Mov (operand_of_value left_value, operand_of_value dst);
            Sub (operand_of_value right_value, operand_of_value dst);
          ]
      | Ast.Multiply ->
          [
            Mov (operand_of_value left_value, operand_of_value dst);
            Mul (operand_of_value right_value, operand_of_value dst);
          ]
      | Ast.Divide ->
          [
            Mov (operand_of_value left_value, Reg EAX);
            Cdq;
            Idiv (operand_of_value right_value);
            Mov (Reg EAX, operand_of_value dst);
          ]
      | _ -> failwith "TODO"
      end
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
  | Add (src, dst) -> begin
      let new_src, seen' = replace_operand src seen in
      let new_dst, seen'' = replace_operand dst seen' in
      (Add (new_src, new_dst), seen'')
    end
  | Mul (src, dst) -> begin
      let new_src, seen' = replace_operand src seen in
      let new_dst, seen'' = replace_operand dst seen' in
      (Mul (new_src, new_dst), seen'')
    end
  | Idiv op -> begin
      let new_op, seen' = replace_operand op seen in
      (Idiv new_op, seen')
    end
  | Cdq -> (i, seen)
  | Cmp (src, dst) -> begin
      let new_src, seen' = replace_operand src seen in
      let new_dst, seen'' = replace_operand dst seen' in
      (Cmp (new_src, new_dst), seen'')
    end
  | SetCC (cond, dst) -> begin
      let new_dst, seen' = replace_operand dst seen in
      (SetCC (cond, new_dst), seen')
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

let fix_instruction (i : instruction) : instruction list =
  begin match i with
  | Mov (Stack s, Stack d) -> begin
      [ Mov (Stack s, Reg R10); Mov (Reg R10, Stack d) ]
    end
  | Sub (Stack s, Stack d) -> [ Mov (Stack s, Reg R10); Sub (Reg R10, Stack d) ]
  | Add (Stack s, Stack d) -> [ Mov (Stack s, Reg R10); Add (Reg R10, Stack d) ]
  | Mul (s, Stack d) ->
      [ Mov (Stack d, Reg R11); Mul (s, Reg R11); Mov (Reg R11, Stack d) ]
  | Idiv (Number n) -> [ Mov (Number n, Reg R10); Idiv (Reg R10) ]
  | _ -> [ i ]
  end

let fun_ir_to_asm (f : Ir.fun_def_in_IR) : asm_fun =
  begin
    let body = List.concat (List.map instructions_of_step f.fun_body_ir) in
    let new_body, seen = replace_body body [] in
    let new_body' = List.concat (List.map fix_instruction new_body) in
    let bytes = List.length seen * 4 in
    {
      fun_name_asm = f.fun_name_ir;
      fun_body_asm =
        [ Push (Reg RBP); Mov (Reg RSP, Reg RBP); Sub (Number bytes, Reg RSP) ]
        @ new_body';
    }
  end

let asm_of_program (p : Ir.fun_def_in_IR list) : asm_fun list =
  begin
    List.map fun_ir_to_asm p
  end

let operand_to_str (size : operand_size) (o : operand) : string =
  begin match o with
  | Number n -> string_of_int n
  | Reg r ->
      begin match (r, size) with
      | EAX, Dword -> "eax"
      | EAX, Byte -> "al"
      | R10, Dword -> "r10d"
      | R10, Byte -> "r10b"
      | R11, Dword -> "r11d"
      | R11, Byte -> "r11b"
      | DX, Dword -> "edx"
      | DX, Byte -> "dl"
      | RBP, Dword -> "rbp"
      | RSP, Dword -> "rsp"
      | RBP, Byte ->
          failwith
            "printing a 1-byte rbp/rsp: an earlier pass built an invalid \
             operand (rbp and rsp are only used as full 64-bit registers for \
             the stack frame)"
      | RSP, Byte ->
          failwith
            "printing a 1-byte rbp/rsp: an earlier pass built an invalid \
             operand (rbp and rsp are only used as full 64-bit registers for \
             the stack frame)"
      end
  (* dword is 4 bytes, size of int *)
  | Stack b ->
      begin match size with
      | Dword -> "dword [rbp" ^ string_of_int b ^ "]"
      | Byte -> "byte [rbp" ^ string_of_int b ^ "]"
      end
  | Pseudo s ->
      failwith "this should never happen: pseudoregister reached printer"
  end

let compare_cond_to_str (cond : comparison_cond) : string =
  begin match cond with
  | E -> "e"
  | NE -> "ne"
  | L -> "l"
  | LE -> "le"
  | G -> "g"
  | GE -> "ge"
  end

let instruction_to_str (i : instruction) : string =
  let op = operand_to_str Dword in
  begin match i with
  (* NASM writes the destination first *)
  | Mov (src, dst) -> "    mov " ^ op dst ^ ", " ^ op src
  | Neg o -> "    neg " ^ op o
  | Push o -> "    push " ^ op o
  | Pop o -> "    pop " ^ op o
  | Sub (src, dst) -> "    sub " ^ op dst ^ ", " ^ op src
  | Add (src, dst) -> "    add " ^ op dst ^ ", " ^ op src
  | Mul (src, dst) -> "    imul " ^ op dst ^ ", " ^ op src
  | Idiv o -> "    idiv " ^ op o
  | Cdq -> "    cdq"
  | Cmp (src, dst) -> "    cmp " ^ op dst ^ ", " ^ op src
  | SetCC (cond, dst) ->
      "    set" ^ compare_cond_to_str cond ^ " " ^ operand_to_str Byte dst
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
