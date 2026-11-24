open Compiler

let run_command (cmd : string) : unit =
  begin
    let code = Sys.command cmd in
    if code <> 0 then
      failwith ("command failed (exit code " ^ string_of_int code ^ "): " ^ cmd)
  end

let write_file (filename : string) (content : string) : unit =
  begin
    let out = open_out filename in
    output_string out content;
    close_out out
  end

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

let asm_pipeline (source : string) : string =
  begin
    let lexer_state : Lexer.lexer_state = { source; position = 0 } in
    let tokens : Lexer.token_type list = Lexer.tokenize lexer_state in
    Parser.parse_program tokens
    |> (fun (p : Ast.program_def) : (Ast.program_def * int) ->
    Semantic.resolve_program p 0)
    |> fst |> Ir.flattening_program |> Asm.asm_of_program
    |> Asm.asm_program_to_str
  end

let compile_to_executable (c_path : string) : unit =
  begin
    let base = Filename.remove_extension c_path in
    let i_path = base ^ ".i" in
    let asm_path = base ^ ".asm" in
    let obj_path = base ^ ".o" in
    Fun.protect
      ~finally:(fun () ->
        List.iter
          (fun p -> if Sys.file_exists p then Sys.remove p)
          [ i_path; asm_path; obj_path ])
      (fun () ->
        run_command
          (Printf.sprintf "gcc -E -P %s -o %s" (Filename.quote c_path)
             (Filename.quote i_path));
        write_file asm_path (asm_pipeline (read_file i_path));
        run_command
          (Printf.sprintf "nasm -f elf64 %s -o %s" (Filename.quote asm_path)
             (Filename.quote obj_path));
        run_command
          (Printf.sprintf "gcc %s -o %s" (Filename.quote obj_path)
             (Filename.quote base)))
  end

let run_flag_mode (flag : string) (file_path : string) : unit =
  begin
    let lexer_state : Lexer.lexer_state =
      { source = read_file file_path; position = 0 }
    in
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
    | "--sem" -> begin
        print_endline "====Semantic state====";
        Parser.parse_program tokens
        |> (fun (p : Ast.program_def) : (Ast.program_def * int) ->
        Semantic.resolve_program p 0)
        |> fst |> Pretty.node_of_program
        |> (fun (n : Pretty.node) : string -> Pretty.renderer n 0)
        |> print_endline;
        print_endline "Semantic OK"
      end
    | "--ir" -> begin
        print_endline "====Intermediate representation====";
        Parser.parse_program tokens
        |> (fun (p : Ast.program_def) : (Ast.program_def * int) ->
        Semantic.resolve_program p 0)
        |> fst
           (* fst is a function from OCaml's standard library. fst takes a pair (a 2-tuple) and returns the first element *)
        |> Ir.flattening_program |> List.map Ir.fun_ir_to_str
        |> String.concat "\n\n" |> print_endline;
        print_endline "IR OK"
      end
    | "--asm" -> begin asm_pipeline (read_file file_path) |> print_string end
    | _ -> failwith "fails with the usage message"
  end

let () =
  match Array.length Sys.argv with
  | 2 when not (String.starts_with ~prefix:"--" Sys.argv.(1)) ->
      compile_to_executable Sys.argv.(1)
  | 3 -> run_flag_mode Sys.argv.(1) Sys.argv.(2)
  | _ -> failwith "usage: main.exe <file.c>  or  main.exe --flag <file.c>"
