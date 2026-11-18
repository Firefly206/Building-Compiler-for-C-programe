type var_info = { unique_name : string; from_current_block : bool }
type var_map = (string * var_info) list

let make_unique_name (name : string) (counter : int) : string * int =
  (name ^ "." ^ string_of_int counter, counter + 1)

let rec resolve_exp (e : Ast.exp) (map : var_map) : Ast.exp =
  begin match e with
  | Ast.Constant _ -> e
  | Ast.Variable str ->
      begin match List.assoc_opt str map with
      | Some info -> Ast.Variable info.unique_name
      | None -> failwith ("the variable ( " ^ str ^ " ) was never declared")
      end
  | Ast.Unary (op, exp) -> Ast.Unary (op, resolve_exp exp map)
  | Ast.Binary (op, left, right) ->
      Ast.Binary (op, resolve_exp left map, resolve_exp right map)
  | Ast.Assignment (left, right) ->
      Ast.Assignment (resolve_exp left map, resolve_exp right map)
  end

let resolve_declaration (d : Ast.declaration) (map : var_map) (counter : int) :
    Ast.declaration * var_map * int =
  begin match List.assoc_opt d.name map with
  | Some info when info.from_current_block ->
      failwith
        ("the variable < " ^ d.name
       ^ " > already declared. Redeclaration error. Compilation failed")
  | _ -> begin
      let uname, c = make_unique_name d.name counter in
      let new_var_info = { unique_name = uname; from_current_block = true } in
      let new_map = (d.name, new_var_info) :: map in
      let new_init =
        begin match d.init_val with
        | Some e -> Some (resolve_exp e new_map)
        | None -> None
        end
      in
      ({ d with Ast.name = uname; init_val = new_init }, new_map, c)
    end
  end

let rec resolve_statement (s : Ast.statement) (map : var_map) (counter : int) :
    Ast.statement * var_map * int =
  begin match s with
  | Ast.Return e -> (Ast.Return (resolve_exp e map), map, counter)
  | Ast.Null -> (Ast.Null, map, counter)
  | Ast.Expression e -> (Ast.Expression (resolve_exp e map), map, counter)
  | Ast.Declaration d -> begin
      let new_d, new_map, c = resolve_declaration d map counter in
      (Ast.Declaration new_d, new_map, c)
    end
  | Ast.If (cond, then_branch, else_branch) -> begin
      let new_cond = resolve_exp cond map in
      let new_then_branch, _, c' = resolve_statement then_branch map counter in
      let new_else, c'' =
        begin match else_branch with
        | Some el -> begin
            let el', _, after = resolve_statement el map c' in
            (Some el', after)
          end
        | None -> (None, c')
        end
      in
      (Ast.If (new_cond, new_then_branch, new_else), map, c'')
    end
  | _ -> failwith "TODO"
  end
