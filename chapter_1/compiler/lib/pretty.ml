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

let node_of_statement
