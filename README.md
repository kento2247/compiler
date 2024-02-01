# コンパイラ構成論 第二回演習

<div style="text-align: right;">
学籍番号: 62112607<br>
氏名: 戸倉健登
</div>

## 0. 回答状況

### Frontend

- [x] 1. 単行コメントアウト
- [x] 2. 構文エラー時に行番号と直後の字句を印字
- [ ] 3. error トークンの挿入

### Backend

- [x] 1. MOD の実装
- [ ] 2. 変数宣言と初期化を同時に行う
- [x] 3. ^の実装
- [ ] 4. ++の実装
- [ ] 5. +=の実装
- [ ] 6. do while の実装
- [ ] 7. for の実装
- [ ] 8. return 時の型検査の実装

## 1. Frontend

1.  //で単行コメントアウト  
    以下のコードを lexer.mll に追加

```ocaml
| "//"[^'\n']*            { lexer lexbuf }
```

2.  構文エラー時に行番号と直後の字句を印字

    1.  未定義の token(#など)が出現した場合: lexer.mll を編集
        - 行数を管理する以下のコードを追加
          ```ocaml
          let line_num = ref 1
          ```
        - 改行時の処理を以下のように変更。改行を検出したら行数をインクリメントするように変更
          ```ocaml
          | '\n'                    { incr line_num; lexer lexbuf; }
          | [' ' '\t']              { lexer lexbuf } (* eat up whitespace *)
          ```
        - No_such_symbol 定義を変更。文字列を引数に取れるように変更
          ```ocaml
           exception No_such_symbol of string
          ```
        - \_処理を以下のように変更
          ```ocaml
          | _                       {
                                    let lexeme = Lexing.lexeme lexbuf in
                                    let message = Printf.sprintf "at line %d, before '%s'\n" !line_num lexeme in
                                    raise (No_such_symbol message)
                                  }
          ```
    2.  構文エラーが発生した場合
        - print_ast.ml の main()、および呼び出しを以下に変更
          ```ocaml
          let main () =
            (* The open of a file *)
            let cin = if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
                    else stdin in
                        let lexbuf = Lexing.from_channel cin in
                (* The start of the entire program *)
                        try
                            print_string (ast_stmt (Parser.prog Lexer.lexer lexbuf));
                            print_string "\n"
                        with
                                | Parsing.Parse_error -> print_string (Printf.sprintf "Syntax error at line %d, before \"%s\"\n" !Lexer.line_num (Lexing.lexeme lexbuf)
          let \_ = main ()
          ```
          - try with で lexbuf が必要なので、main()全体に try with をせず、ast_stmt()の呼び出しに try with をした

3.  error トークンの挿入

## 2. Backend

1. MOD の実装

   - emitter.ml に以下のコードを追加
     ```ocaml
     (* %のコード *)
     | CallFunc ("%", [left; right]) ->
                            trans_exp left nest env
                            ^ trans_exp right nest env
                            ^ "\tpopq %rbx\n"
                            ^ "\tpopq %rax\n"
                            ^ "\tcqto\n"
                            ^ "\tidivq %rbx\n"
                            ^ "\tpushq %rdx\n"
     ```
   - lexer.mll に以下のコードを追加
     ```ocaml
     | "%"                     { MOD }
     ```
   - parser.mly のに以下のコードを追加
     ```ocaml
     %token MOD
     ...
     | expr MOD expr { CallFunc ("%", [$1; $3]) }
     ```
   - semant.ml の type_exp に以下のコードを追加
     ```ocaml
     | CallFunc ("%", [left; right]) ->
               (check_int (type_exp left env); check_int(type_exp right env); INT)
     ```

2. 変数宣言と初期化を同時に行う

3. ^の実装
   - emitter.ml に以下のコードを追加
     ```ocaml
     (* %のコード *)
     | CallFunc ("^", [left; right]) ->
                                            trans_exp left nest env
                                          ^ trans_exp right nest env
                                          ^ "\tpopq %rbx\n"
                                          ^ "\tpopq %rax\n"
                                          ^ "\tmovq $1, %rdx\n"
                                          ^ "factorial_loop" ^ string_of_int nest ^ ":\n"
                                          ^ "\tcmpq $0, %rbx\n"
                                          ^ "\tje factorial_end" ^ string_of_int nest ^ "\n"
                                          ^ "\timulq %rax, %rdx\n"
                                          ^ "\tdecq %rbx\n"
                                          ^ "\tjmp factorial_loop" ^ string_of_int nest ^ "\n"
                                          ^ "factorial_end" ^ string_of_int nest ^ ":\n"
                                          ^ "\tpushq %rdx\n"
     ```
   - lexer.mll に以下のコードを追加
     ```ocaml
     | "^"                     { POW }
     ```
   - parser,mly に以下のコードを追加
     ```ocaml
     %token POW
     ...
     | expr POW expr { CallFunc ("^", [$1; $3]) }
     ```
   - semant.ml の type_exp に以下のコードを追加
     ```ocaml
     | CallFunc ("^", [left; right]) ->
               (check_int (type_exp left env); check_int(type_exp right env); INT)
     ```
4. ++の実装

   - emitter.ml に以下のコードを追加

     ```ocaml
     (* ++のコード *)

     ```

   - lexer.mll に以下のコードを追加
     ```ocaml
     | "++"                     { INC }
     ```
   - parser.mly のに以下のコードを追加
     ```ocaml
     %token INC
     ...
     | expr INC { CallFunc ("++", [$1]) }
     ```
   - semant.ml の type_exp に以下のコードを追加
     ```ocaml
     | CallFunc ("++", [arg]) ->
               (check_int (type_exp arg env); INT)
     ```

5. +=の実装

   - emitter.ml に以下のコードを追加

     ```ocaml
     (* +=のコード *)

     ```

   - lexer.mll に以下のコードを追加
     ```ocaml
     | "+="                     { ADD_EQ }
     ```
   - parser.mly のに以下のコードを追加
     ```ocaml
     %token ADD_EQ
     ...
     | expr ADD_EQ { CallFunc ("+=", [$1; $3]) }
     ```
   - semant.ml の type_exp に以下のコードを追加
     ```ocaml
     | CallFunc ("+=", [left; right]) ->
               (check_int (type_exp left env); check_int(type_exp right env); INT)
     ```

6. do while の実装

7. for の実装

8. return 時の型検査の実装
