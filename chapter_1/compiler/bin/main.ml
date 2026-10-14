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

let flag : string =
  begin if Array.length Sys.argv >= 2 then Sys.argv.(1)
  else failwith "usage: ./main --stage-name"
  end

let file_path : string =
  begin if Array.length Sys.argv >= 3 then Sys.argv.(2)
  else failwith "usage: ./main --lex|--parse <file.c>"
  end

let lexer_state : Lexer.lexer_state =
  { source = read_file file_path; position = 0 }

let () =
  begin
    let tokens : Lexer.token_type list = Lexer.tokenize lexer_state in
    match flag with
    | "--lex" -> begin
        print_endline "====Lexer state=====";
        tokens |> List.iter (fun t -> print_endline (Lexer.token_to_str t))
      end
    | "--parse" -> begin
        print_endline "====Parser state====";
        Parser.parse_program tokens
        |> Pretty.node_of_program
        |> (fun (n : Pretty.node) : string -> Pretty.renderer n 0)
        |> print_endline;
        print_endline "Parse OK"
      end
    | _ -> failwith "fails with the usage message"
  end
