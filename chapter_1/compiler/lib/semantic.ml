type var_info = { unique_name : string; from_current_block : bool }
type var_map = (string * var_info) list

let make_unique_name (name : string) (counter : int) : string * int =
  (name ^ "." ^ string_of_int counter, counter + 1)

let current_block_lst_zeros_out (map : var_map) : var_map =
  begin
    List.map
      (fun (s, info) : (string * var_info) ->
        (s, { info with from_current_block = false }))
      map
  end

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

let resolve_optional_exp (op_e : Ast.exp option) (map : var_map) :
    Ast.exp option =
  begin
    begin match op_e with Some e -> Some (resolve_exp e map) | None -> None
    end
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
  | Ast.While (cond, body) -> begin
      let new_body, _, c' = resolve_statement body map counter in
      (Ast.While (resolve_exp cond map, new_body), map, c')
    end
  | Ast.Block statement_lst -> begin
      let inner_map = current_block_lst_zeros_out map in
      let rec block_helper (statements : Ast.statement list) (inner : var_map)
          (c : int) (acc_lst : Ast.statement list) : Ast.statement list * int =
        begin match statements with
        | [] -> (List.rev acc_lst, c)
        | head :: rest -> begin
            let resolved_statement, inner', c' =
              resolve_statement head inner c
            in
            block_helper rest inner' c' (resolved_statement :: acc_lst)
          end
        end
      in
      let lst, c' = block_helper statement_lst inner_map counter [] in
      (Ast.Block lst, map, c')
    end
  | Ast.For (header, body) -> begin
      let inner_map = current_block_lst_zeros_out map in
      let init, map', c' =
        begin match header.init with
        | Ast.InitDeclare d -> begin
            let decl, new_map, new_c =
              resolve_declaration d inner_map counter
            in
            (Ast.InitDeclare decl, new_map, new_c)
          end
        | Ast.InitExp op_exp -> begin
            let init_exp = resolve_optional_exp op_exp inner_map in
            (Ast.InitExp init_exp, inner_map, counter)
          end
        end
      in
      let cond = resolve_optional_exp header.condition map' in
      let post = resolve_optional_exp header.post map' in
      let body', _, c'' = resolve_statement body map' c' in
      (Ast.For ({ init; condition = cond; post }, body'), map, c'')
    end
  end

let resolve_function (fun_def : Ast.fun_def) (counter : int) : Ast.fun_def * int
    =
  begin
    let rec resolve_lst_param (params : Ast.parameter list) (m : var_map)
        (c : int) (acc : Ast.parameter list) :
        Ast.parameter list * var_map * int =
      begin match params with
      | [] -> (List.rev acc, m, c)
      | { Ast.parameter_type = Ast.VoidType; _ } :: _ -> (List.rev acc, m, c)
      | first :: rest ->
          begin match List.assoc_opt first.parameter_name m with
          | Some info when info.from_current_block ->
              failwith
                ("the variable < " ^ first.parameter_name
               ^ " > already declared. Redeclaration error. Compilation failed"
                )
          | _ -> begin
              let uname, c' = make_unique_name first.parameter_name c in
              let new_var_info =
                { unique_name = uname; from_current_block = true }
              in
              let new_map = (first.parameter_name, new_var_info) :: m in
              let acc_list = { first with Ast.parameter_name = uname } :: acc in
              resolve_lst_param rest new_map c' acc_list
            end
          end
      end
    in
    let params, map, c' =
      resolve_lst_param fun_def.fun_parameter [] counter []
    in
    let rec resolve_lst_statement (statements : Ast.statement list)
        (m : var_map) (c : int) (acc : Ast.statement list) :
        Ast.statement list * var_map * int =
      begin match statements with
      | [] -> (List.rev acc, m, c)
      | first :: rest -> begin
          let statement, m', c' = resolve_statement first m c in
          let acc' = statement :: acc in
          resolve_lst_statement rest m' c' acc'
        end
      end
    in
    let body, _, c'' = resolve_lst_statement fun_def.fun_body map c' [] in
    ({ fun_def with Ast.fun_parameter = params; Ast.fun_body = body }, c'')
  end

let resolve_program (program : Ast.program_def) (counter : int) :
    Ast.program_def * int =
  begin
    let rec resolve_lst_function (lst_fun : Ast.fun_def list) (c : int)
        (acc : Ast.fun_def list) : Ast.fun_def list * int =
      begin match lst_fun with
      | [] -> (List.rev acc, c)
      | first :: rest -> begin
          let fun_def, c' = resolve_function first c in
          let acc' = fun_def :: acc in
          resolve_lst_function rest c' acc'
        end
      end
    in
    let fun_defs, c = resolve_lst_function program.fun_defs counter [] in
    ({ Ast.fun_defs }, c)
  end
