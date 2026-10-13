type node = { label : string; children : node list }

let rec renderer (tree_node : node) (indent : int) : string =
  begin
    let current_line : string = String.make indent ' ' ^ tree_node.label in
    let children_lines =
      List.map
        (fun (n : node) : string -> renderer n (indent + 2))
        tree_node.children
    in
    String.concat "\n" (current_line :: children_lines)
  end

let rec node_of_exp (exp : Ast.exp) : node =
  begin
    {
      label = Ast.label exp;
      children =
        List.map
          (fun (e : Ast.exp) : node -> node_of_exp e)
          (Ast.inner_exp_fun exp);
    }
  end

let node_of_declaration (declaration : Ast.declaration) : node =
  begin
    {
      label =
        "Declaration " ^ declaration.name ^ " : "
        ^ Ast.var_type_to_str declaration.decl_type;
      children =
        begin match declaration.init_val with
        | None -> []
        | Some e -> [ node_of_exp e ]
        end;
    }
  end

let labeled (label_node : string) (lst_node : node list) : node =
  begin
    { label = label_node; children = lst_node }
  end

(* Helper for "for loop" statement *)

let rec node_of_statement (statement : Ast.statement) : node =
  begin match statement with
  | Ast.Return expression -> labeled "Return" [ node_of_exp expression ]
  | Ast.Expression expression -> labeled "Expression" [ node_of_exp expression ]
  | Ast.Declaration declare -> node_of_declaration declare
  | Ast.While (condition, body) ->
      labeled "While" [ node_of_exp condition; node_of_statement body ]
  | Ast.Block items -> labeled "Block" (List.map node_of_statement items)
  | Ast.If (condition, then_branch, else_branch) ->
      begin match else_branch with
      | None ->
          labeled "If" [ node_of_exp condition; node_of_statement then_branch ]
      | Some s ->
          labeled "If"
            [
              node_of_exp condition;
              node_of_statement then_branch;
              node_of_statement s;
            ]
      end
  | Ast.Null -> labeled "Null" []
  | Ast.For -> 
  | _ -> failwith "TODO"
  end
