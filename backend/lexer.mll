(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol of string
 let line_num = ref 1
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule lexer = parse
| digit+ as num  { NUM (int_of_string num) }
| "if"                    { IF }
| "else"                  { ELSE }
| "while"                 { WHILE }
| "scan"                  { SCAN }
| "sprint"                { SPRINT }
| "iprint"                { IPRINT }
| "int"                   { INT }
| "return"                { RETURN }
| "type"                  { TYPE }
| "void"                  { VOID }
| id as text              { ID text }
| '\"'[^'\"']*'\"' as str { STR str }
| "//"[^'\n']*            { lexer lexbuf; }
| '='                     { ASSIGN }
| "=="                    { EQ }
| "!="                    { NEQ }
| '>'                     { GT }
| '<'                     { LT }
| ">="                    { GE }
| "<="                    { LE }
| '+'                     { PLUS }
| '-'                     { MINUS }
| '*'                     { TIMES }
| '/'                     { DIV }
| '%'                     { MOD }
| '{'                     { LB  }
| '}'                     { RB  }
| '['                     { LS }
| ']'                     { RS }
| '('                     { LP  }
| ')'                     { RP  }
| ','                     { COMMA }
| ';'                     { SEMI }
| '\n'                    { incr line_num; lexer lexbuf; }
| [' ' '\t']              { lexer lexbuf } (* eat up whitespace *)
| eof                     { raise End_of_file }
| _                       {
                            let lexeme = Lexing.lexeme lexbuf in
                            let message = Printf.sprintf "at line %d, before \"%s\"\n" !line_num lexeme in
                            raise (No_such_symbol message)
                          }