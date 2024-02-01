let main () =
  (* ファイルを開く *)
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin in
      let lexbuf = Lexing.from_channel cin in
        (* 生成コード用ファイルtmp.sをオープン *)
  try
        let file = open_out "tmp.s" in                           
            (* コード生成 *)
            let code = Emitter.trans_prog (Parser.prog Lexer.lexer lexbuf) in
              (* 生成コードの書出しとファイルのクローズ *)
              output_string file code; close_out file;     
              (* アセンブラとリンカの呼出し *)
              let _ = Unix.system "gcc tmp.s" in ()    
  with
    | Parsing.Parse_error -> print_string (Printf.sprintf "Syntax error at line %d, before \"%s\"\n" !Lexer.line_num (Lexing.lexeme lexbuf))
    | Table.No_such_symbol x -> print_string ("no such symbol: \""^x^"\"\n")
    | Semant.TypeErr s -> print_string (s^"\n")
    | Semant.Err s -> print_string (s^"\n")
    | Table.SymErr s -> print_string (s^"\n")            

let _ = main()