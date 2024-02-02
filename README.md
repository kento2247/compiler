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
- [x] 4. ++の実装
- [x] 5. +=の実装
- [x] 6. do while の実装
- [x] 7. for の実装
- [ ] 8. return 時の型検査の実装

## 1. Frontend

1.  //で単行コメントアウト

    - 以下のコードを lexer.mll に追加

      ```ocaml
      | "//"[^'\n']*            { lexer lexbuf }
      ```

    - 実行結果

      ```
      {
        int x, rlt;
        // int y = 0;

        x = 1;
      }
      ```

      ```bash
      [ub465982@logex01 front]$ ./print_ast ../sample.spl
      Block([VarDec(IntTyp,"x"); VarDec(IntTyp,"rlt")],[Assign(Var "x",IntExp(1))])
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
        - 実行結果

          ```
          {
            int x, rlt;
            // int y = 0;

            %
            x = 1;
          }
          ```

          ```bash
          [ub465982@logex01 front]$ ./print_ast ../sample.spl
          Fatal error: exception Lexer.No_such_symbol("at line 5, before \"%\"\n")
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
          - 実行結果

            ```
            {
              int x, rlt;
              // int y = 0;

              x = 1
            }
            ```

            ```bash
            [ub465982@logex01 front]$ ./print_ast ../sample.spl
            Syntax error at line 6, before "}"
            ```

3.  error トークンの挿入  
    未着手

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
   - lexer.mll の rule に以下のコードを追加
     ```ocaml
     | "%"                     { MOD }
     ```
   - parser.mly に以下のコードを追加
     ```ocaml
     %token MOD
     ...
     stmt: ...
     | expr MOD expr { CallFunc ("%", [$1; $3]) }
     ```
   - semant.ml の type_exp に以下のコードを追加
     ```ocaml
     | CallFunc ("%", [left; right]) ->
               (check_int (type_exp left env); check_int(type_exp right env); INT)
     ```
   - 実行結果

     ```
     {
       int rlt;

       //MOD
       rlt = 5%2;
       iprint(rlt);
       sprint("\n");
     }
     ```

     ```bash
     [ub465982@logex01 backend]$ ./run ../sample.spl
     1
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
   - lexer.mll の rule に以下のコードを追加
     ```ocaml
     | "^"                     { POW }
     ```
   - parser.mly に以下のコードを追加
     ```ocaml
     %token POW
     ...
     stmt: ...
     | expr POW expr { CallFunc ("^", [$1; $3]) }
     ```
   - semant.ml の type_exp に以下のコードを追加
     ```ocaml
     | CallFunc ("^", [left; right]) ->
               (check_int (type_exp left env); check_int(type_exp right env); INT)
     ```
   - 実行結果

     ```
     {
       int rlt;

       //POW
       rlt = 2^3;
       iprint(rlt);
       sprint("\n");
     }
     ```

     ```bash
      [ub465982@logex01 backend]$ ./run ../sample.spl
      8
     ```

4. ++の実装

   - emitter.ml の trans_stmt に以下のコードを追加

     ```ocaml
     (* ++のコード *)
                  | Incr v ->
                        trans_var v nest env
                        ^ "\tmovq (%rax), %rbx\n"
                        ^ "\taddq $1, (%rax)\n"
                        ^ "\tpushq %rbx\n"

     ```

   - lexer.mll の rule に以下のコードを追加
     ```ocaml
     | "++"                    { INC }
     ```
   - parser.mly に以下のコードを追加
     ```ocaml
     %token INC
     ...
     stmt: ...
     | ID INC { Incr (Var $1) }
     ```
   - semant.ml の type_stmt に以下のコードを追加
     ```ocaml
     | Incr v ->
               check_int (type_var v env);
     ```
   - ast.ml の stmt に以下のコードを追加
     ```ocaml
     | Incr of var
     ```
   - 実行結果

     ```
     {
       int rlt;

       //++
       rlt = 2;
       rlt++;
       iprint(rlt);
       sprint("\n");
     }
     ```

     ```bash
     [ub465982@logex01 backend]$ ./run ../sample.spl
     3
     ```

5. +=の実装

   - emitter.ml の trans_stmt に以下のコードを追加

     ```ocaml
     (* +=のコード *)
                  | AddEq (v, e) ->
                        trans_exp e nest env
                        ^ trans_var v nest env
                        ^ "\tpopq %rbx\n"
                        ^ "\taddq %rbx, (%rax)\n"

     ```

   - lexer.mll の rule に以下のコードを追加
     ```ocaml
     | "+="                     { ADD_EQ }
     ```
   - parser.mly に以下のコードを追加
     ```ocaml
     %token ADD_EQ
     ...
     stmt: ...
     | expr ADD_EQ { CallFunc ("+=", [$1; $3]) }
     ```
   - semant.ml の type_stmt に以下のコードを追加
     ```ocaml
     | AddEq (v, e) ->
               if (type_var v env) != INT then raise (TypeErr "type error 4");
               if (type_exp e env) != INT then raise (TypeErr "type error 4");
     ```
   - ast.ml の stmt に以下のコードを追加
     ```ocaml
     | AddEq of var * exp
     ```
   - 実行結果

     ```
     {
       int rlt;

       //+=
       rlt = 2;
       rlt += 3;
       iprint(rlt);
       sprint("\n");
     }
     ```

     ```bash
      [ub465982@logex01 backend]$ ./run ../sample.spl
      5
     ```

6. do while の実装

   - emitter.ml に以下のコードを追加

     ```ocaml
     (* do while文のコード *)
                  | DoWhile (e,s) -> let (condCode, l1) = trans_cond e nest env in
                                     let l2 = incLabel() in
                                         sprintf "L%d:\n" l2
                                       ^ trans_stmt s nest tenv env
                                       ^ condCode
                                       ^ sprintf "\tjmp L%d\n" l2
                                       ^ sprintf "L%d:\n" l1
     ```

   - lexer.mll の rule に以下のコードを追加
     ```ocaml
     | "do"                    { DO }
     ```
   - parser.mly に以下のコードを追加
     ```ocaml
     %token DO
     ...
     stmt: ...
     | DO stmt WHILE LP cond RP { DoWhile ($5, $2) }
     ```
   - semant.ml の type_stmt に以下のコードを追加
     ```ocaml
     | DoWhile (e,_) -> type_cond e env
     ```
   - ast.ml の stmt に以下のコードを追加
     ```ocaml
     | DoWhile of exp * stmt
     ```
   - 実行結果

     ```
     {
       int rlt;

       //do while
       rlt = 0;
       do {
         rlt++;
         iprint(rlt);
         sprint("\n");
       } while (rlt < 3);
     }
     ```

     ```bash
      [ub465982@logex01 backend]$ ./run ../sample.spl
      1
      2
      3
     ```

7. for の実装

   - emitter.ml に以下のコードを追加

     ```ocaml
     (* For文のコード *)
                  | For (v, e1, e2, s) ->
                                    let l1 = incLabel() in
                                    let l2 = incLabel() in
                                    trans_exp e1 nest env
                                    ^ trans_var v nest env
                                    ^ "\tpopq (%rax)\n"
                                    ^ sprintf "L%d:\n" l1
                                    ^ trans_exp e2 nest env
                                    ^ "\tpopq %rbx\n"
                                    ^ trans_var v nest env
                                    ^ "\tcmpq (%rax), %rbx\n"
                                    ^ sprintf "\tje L%d\n" l2
                                    ^ trans_stmt s nest tenv env
                                    ^ trans_var v nest env
                                    ^ "\taddq $1, (%rax)\n"
                                    ^ sprintf "\tjmp L%d\n" l1
                                    ^ sprintf "L%d:\n" l2
     ```

     - (%rax)にアクセスする前に、trans_var で rax に変数のアドレスを入れ直す方が安全であると判断
       ```ocaml
       ^ trans_var v nest env
       ^ "\tcmpq (%rax), %rbx\n"
       ```

   - lexer.mll の rule に以下のコードを追加

     ```ocaml
     | "for"                   { FOR }
     ...
     | ".."                    { TO }
     ```

     > ".."の宣言位置が FOR の直後だと、なぜか実行時に segmentation fault した。そのため SEMI の直後に移動した。

   - parser.mly に以下のコードを追加
     ```ocaml
     %token FOR
     %token TO
     ...
     stmt: ...
     | FOR LP ID ASSIGN expr TO expr RP stmt { For ((Var $3), $5, $7, $9) }
     ```
   - semant.ml の type_stmt に以下のコードを追加
     ```ocaml
     | For (v, e1, e2, s) ->
               if (type_var v env) != INT then raise (TypeErr "type error 4");
               if (type_exp e1 env) != INT then raise (TypeErr "type error 4");
               if (type_exp e2 env) != INT then raise (TypeErr "type error 4");
               type_stmt s env
     ```
   - ast.ml の stmt に以下のコードを追加
     ```ocaml
     | For of var * exp * exp * stmt
     ```
   - 実行結果

     ```
     {
       int rlt;

       //for
       for (rlt = 0; rlt < 3; rlt++) {
         iprint(rlt);
         sprint("\n");
       }
     }
     ```

     ```bash
     [ub465982@logex01 backend]$ ./run ../sample.spl
     0
     1
     2
     ```

8. return 時の型検査の実装  
   未着手
