open Compiler

let read_file (filename : string) : string =
  try
    (* Read file and display the first line *)
    let input_channel = open_in filename in
    let n = in_channel_length input_channel in
    let s = Bytes.create n in
    really_input input_channel s 0 n;
    close_in input_channel;
    Bytes.to_string s
  with e ->
    (* some unexpected exception occurs *)
    (* emergency closing *)
    raise e

let file_path : string =
  if Array.length Sys.argv >= 2 then Sys.argv.(1)
  else failwith "usage: ./main <file.c>"

let lexer_state : Lexer.lexer_state =
  { source = read_file file_path; position = 0 }

let () =
  begin
    let tokens : Lexer.token_type list = Lexer.tokenize lexer_state in
    print_endline "====Lexer state=====";
    tokens |> List.iter (fun t -> print_endline (Lexer.token_to_str t));
    print_endline "====Parser state====";
    let (tree, rest) = Parser.parse_exp tokens 0 in
    (* print_endline (Ast.printer tree 0) *)
    tree |> (fun t -> Ast.printer t 0) |> print_endline; 
    (* Debbug *)
    rest |> List.length |> string_of_int |> (fun str -> (print_endline ("Tokens left: " ^ str)))
  end
