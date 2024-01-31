type token =
  | NUM of (int)
  | STR of (string)
  | ID of (string)
  | INT
  | IF
  | WHILE
  | SPRINT
  | IPRINT
  | SCAN
  | EQ
  | NEQ
  | GT
  | LT
  | GE
  | LE
  | ELSE
  | RETURN
  | NEW
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LB
  | RB
  | LS
  | RS
  | LP
  | RP
  | ASSIGN
  | SEMI
  | COMMA
  | TYPE
  | VOID
  | MOD
  | POW
  | INC
  | ADD_EQ
  | DO
  | FOR

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Printf
open Ast
# 48 "parser.ml"
let yytransl_const = [|
  260 (* INT *);
  261 (* IF *);
  262 (* WHILE *);
  263 (* SPRINT *);
  264 (* IPRINT *);
  265 (* SCAN *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* GT *);
  269 (* LT *);
  270 (* GE *);
  271 (* LE *);
  272 (* ELSE *);
  273 (* RETURN *);
  274 (* NEW *);
  275 (* PLUS *);
  276 (* MINUS *);
  277 (* TIMES *);
  278 (* DIV *);
  279 (* LB *);
  280 (* RB *);
  281 (* LS *);
  282 (* RS *);
  283 (* LP *);
  284 (* RP *);
  285 (* ASSIGN *);
  286 (* SEMI *);
  287 (* COMMA *);
  288 (* TYPE *);
  289 (* VOID *);
  290 (* MOD *);
  291 (* POW *);
  292 (* INC *);
  293 (* ADD_EQ *);
  294 (* DO *);
  295 (* FOR *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* STR *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\004\000\004\000\005\000\005\000\
\005\000\005\000\006\000\006\000\007\000\007\000\009\000\009\000\
\010\000\010\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\013\000\
\013\000\014\000\014\000\008\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\012\000\012\000\012\000\012\000\012\000\012\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\002\000\000\000\003\000\005\000\
\006\000\006\000\003\000\001\000\000\000\001\000\004\000\002\000\
\002\000\001\000\004\000\007\000\005\000\007\000\005\000\005\000\
\005\000\005\000\005\000\005\000\003\000\001\000\001\000\000\000\
\001\000\003\000\001\000\004\000\001\000\001\000\004\000\004\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\003\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\031\000\057\000\001\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\037\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\000\000\000\000\047\000\000\000\000\000\000\000\
\000\000\000\000\000\000\018\000\000\000\005\000\000\000\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\000\017\000\
\000\000\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\024\000\025\000\026\000\040\000\039\000\
\027\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\000\000\003\000\004\000\000\000\000\000\000\000\000\000\000\000\
\011\000\020\000\022\000\008\000\016\000\000\000\000\000\000\000\
\010\000\000\000\009\000\015\000"

let yydgoto = "\002\000\
\013\000\014\000\125\000\030\000\062\000\094\000\126\000\015\000\
\127\000\063\000\032\000\037\000\033\000\034\000"

let yysindex = "\003\000\
\173\255\000\000\236\254\004\255\014\255\015\255\016\255\018\255\
\005\255\025\255\000\000\000\000\000\000\000\000\000\000\005\255\
\005\255\005\255\005\255\005\255\021\255\005\255\052\255\000\000\
\245\254\005\255\005\255\115\000\054\255\074\255\134\000\211\000\
\031\255\027\255\142\000\126\255\032\255\033\255\034\255\161\000\
\035\255\005\255\005\255\001\255\165\000\005\255\005\255\005\255\
\005\255\000\000\005\255\005\255\000\000\005\255\036\255\236\254\
\040\255\063\255\064\255\000\000\073\255\000\000\147\255\055\255\
\039\255\005\255\000\000\005\255\005\255\005\255\005\255\005\255\
\005\255\173\255\173\255\056\255\059\255\060\255\184\000\067\255\
\000\000\066\255\066\255\001\255\001\255\001\255\211\000\211\000\
\068\255\093\255\085\255\069\255\088\255\252\254\000\000\000\000\
\005\255\000\000\211\000\211\000\211\000\211\000\211\000\211\000\
\211\000\100\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\095\255\030\255\030\255\030\255\000\000\119\255\192\000\
\173\255\000\000\000\000\097\255\120\255\102\255\094\255\103\255\
\000\000\000\000\000\000\000\000\000\000\121\255\030\255\121\255\
\000\000\140\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\114\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\098\255\000\000\000\000\000\000\000\000\000\000\000\000\238\254\
\000\000\123\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\114\255\187\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\146\255\
\000\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\072\000\094\000\212\255\022\000\047\000\241\255\116\000\
\000\000\000\000\000\000\000\000\009\255\000\000\000\000\000\000\
\000\000\000\000\240\254\129\255\130\255\131\255\138\255\141\255\
\144\255\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\145\255\145\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\155\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\228\255\226\255\000\000\000\000\000\000\050\000\139\255\
\000\000\000\000\002\000\148\000\131\000\000\000"

let yytablesize = 504
let yytable = "\061\000\
\021\000\060\000\002\000\001\000\016\000\024\000\017\000\025\000\
\018\000\035\000\028\000\034\000\035\000\042\000\034\000\043\000\
\137\000\031\000\139\000\035\000\036\000\036\000\039\000\040\000\
\026\000\118\000\119\000\044\000\045\000\002\000\019\000\027\000\
\123\000\057\000\096\000\052\000\053\000\054\000\012\000\012\000\
\020\000\021\000\022\000\079\000\023\000\106\000\107\000\082\000\
\083\000\084\000\085\000\029\000\086\000\087\000\041\000\088\000\
\055\000\066\000\065\000\074\000\075\000\076\000\078\000\089\000\
\090\000\091\000\092\000\099\000\098\000\100\000\101\000\102\000\
\103\000\104\000\105\000\093\000\056\000\057\000\004\000\005\000\
\006\000\007\000\008\000\097\000\124\000\108\000\048\000\049\000\
\109\000\110\000\009\000\010\000\131\000\114\000\112\000\116\000\
\011\000\113\000\120\000\051\000\052\000\053\000\054\000\012\000\
\138\000\058\000\059\000\038\000\038\000\038\000\038\000\038\000\
\038\000\115\000\117\000\121\000\038\000\038\000\038\000\038\000\
\122\000\129\000\133\000\038\000\135\000\038\000\132\000\038\000\
\038\000\134\000\136\000\038\000\038\000\038\000\038\000\068\000\
\069\000\070\000\071\000\072\000\073\000\032\000\140\000\011\000\
\046\000\047\000\048\000\049\000\004\000\003\000\033\000\004\000\
\005\000\006\000\007\000\008\000\051\000\052\000\053\000\051\000\
\052\000\053\000\054\000\009\000\010\000\054\000\128\000\038\000\
\055\000\011\000\095\000\056\000\013\000\080\000\000\000\003\000\
\012\000\004\000\005\000\006\000\007\000\008\000\014\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\010\000\000\000\
\000\000\000\000\000\000\011\000\049\000\049\000\049\000\049\000\
\049\000\049\000\012\000\000\000\000\000\049\000\049\000\049\000\
\049\000\000\000\000\000\000\000\049\000\000\000\049\000\000\000\
\049\000\049\000\000\000\000\000\049\000\043\000\043\000\043\000\
\043\000\043\000\043\000\000\000\000\000\000\000\043\000\043\000\
\043\000\043\000\000\000\000\000\000\000\043\000\000\000\043\000\
\000\000\043\000\043\000\000\000\000\000\043\000\000\000\000\000\
\000\000\000\000\046\000\046\000\046\000\046\000\046\000\046\000\
\000\000\000\000\000\000\021\000\000\000\021\000\021\000\021\000\
\021\000\021\000\046\000\000\000\046\000\000\000\046\000\046\000\
\000\000\021\000\021\000\000\000\000\000\000\000\000\000\021\000\
\021\000\000\000\000\000\000\000\000\000\000\000\021\000\044\000\
\044\000\044\000\044\000\044\000\044\000\000\000\000\000\000\000\
\044\000\044\000\044\000\044\000\000\000\000\000\000\000\044\000\
\000\000\044\000\000\000\044\000\044\000\000\000\000\000\044\000\
\045\000\045\000\045\000\045\000\045\000\045\000\000\000\000\000\
\000\000\045\000\045\000\045\000\045\000\000\000\000\000\000\000\
\045\000\000\000\045\000\000\000\045\000\045\000\000\000\000\000\
\045\000\041\000\041\000\041\000\041\000\041\000\041\000\000\000\
\000\000\000\000\041\000\041\000\000\000\000\000\000\000\000\000\
\000\000\041\000\000\000\041\000\000\000\041\000\041\000\042\000\
\042\000\042\000\042\000\042\000\042\000\000\000\000\000\000\000\
\042\000\042\000\000\000\000\000\000\000\000\000\000\000\042\000\
\000\000\042\000\000\000\042\000\042\000\048\000\048\000\048\000\
\048\000\048\000\048\000\000\000\000\000\046\000\047\000\048\000\
\049\000\000\000\000\000\000\000\000\000\048\000\000\000\048\000\
\050\000\048\000\048\000\000\000\051\000\052\000\053\000\054\000\
\046\000\047\000\048\000\049\000\000\000\000\000\000\000\064\000\
\046\000\047\000\048\000\049\000\000\000\000\000\000\000\051\000\
\052\000\053\000\054\000\067\000\000\000\000\000\000\000\051\000\
\052\000\053\000\054\000\046\000\047\000\048\000\049\000\046\000\
\047\000\048\000\049\000\000\000\077\000\000\000\000\000\000\000\
\081\000\000\000\051\000\052\000\053\000\054\000\051\000\052\000\
\053\000\054\000\046\000\047\000\048\000\049\000\000\000\000\000\
\000\000\111\000\046\000\047\000\048\000\049\000\000\000\000\000\
\000\000\051\000\052\000\053\000\054\000\130\000\000\000\000\000\
\000\000\051\000\052\000\053\000\054\000\046\000\047\000\048\000\
\049\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\051\000\052\000\053\000\054\000"

let yycheck = "\030\000\
\000\000\030\000\003\001\001\000\025\001\001\001\027\001\003\001\
\029\001\028\001\009\000\028\001\031\001\025\001\031\001\027\001\
\134\000\016\000\136\000\018\000\019\000\020\000\002\001\022\000\
\020\001\030\001\031\001\026\000\027\000\030\001\027\001\027\001\
\003\001\004\001\063\000\035\001\036\001\037\001\030\001\031\001\
\027\001\027\001\027\001\042\000\027\001\074\000\075\000\046\000\
\047\000\048\000\049\000\027\001\051\000\052\000\003\001\054\000\
\003\001\031\001\028\001\028\001\028\001\028\001\028\001\028\001\
\025\001\003\001\003\001\066\000\030\001\068\000\069\000\070\000\
\071\000\072\000\073\000\003\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\029\001\115\000\030\001\021\001\022\001\
\030\001\030\001\017\001\018\001\121\000\001\001\028\001\027\001\
\023\001\030\001\097\000\034\001\035\001\036\001\037\001\030\001\
\135\000\032\001\033\001\010\001\011\001\012\001\013\001\014\001\
\015\001\029\001\027\001\016\001\019\001\020\001\021\001\022\001\
\026\001\003\001\003\001\026\001\031\001\028\001\030\001\030\001\
\031\001\028\001\028\001\034\001\035\001\036\001\037\001\010\001\
\011\001\012\001\013\001\014\001\015\001\028\001\003\001\023\001\
\019\001\020\001\021\001\022\001\003\001\003\001\028\001\005\001\
\006\001\007\001\008\001\009\001\028\001\028\001\028\001\034\001\
\035\001\036\001\037\001\017\001\018\001\028\001\117\000\020\000\
\028\001\023\001\024\001\028\001\028\001\043\000\255\255\003\001\
\030\001\005\001\006\001\007\001\008\001\009\001\028\001\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\255\255\
\255\255\255\255\255\255\023\001\010\001\011\001\012\001\013\001\
\014\001\015\001\030\001\255\255\255\255\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\026\001\255\255\028\001\255\255\
\030\001\031\001\255\255\255\255\034\001\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\255\255\255\255\255\255\026\001\255\255\028\001\
\255\255\030\001\031\001\255\255\255\255\034\001\255\255\255\255\
\255\255\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\026\001\255\255\028\001\255\255\030\001\031\001\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\255\255\255\255\255\255\255\255\255\255\030\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\026\001\
\255\255\028\001\255\255\030\001\031\001\255\255\255\255\034\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\026\001\255\255\028\001\255\255\030\001\031\001\255\255\255\255\
\034\001\010\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\019\001\020\001\255\255\255\255\255\255\255\255\
\255\255\026\001\255\255\028\001\255\255\030\001\031\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\019\001\020\001\255\255\255\255\255\255\255\255\255\255\026\001\
\255\255\028\001\255\255\030\001\031\001\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\255\255\026\001\255\255\028\001\
\030\001\030\001\031\001\255\255\034\001\035\001\036\001\037\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\026\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\034\001\
\035\001\036\001\037\001\030\001\255\255\255\255\255\255\034\001\
\035\001\036\001\037\001\019\001\020\001\021\001\022\001\019\001\
\020\001\021\001\022\001\255\255\028\001\255\255\255\255\255\255\
\028\001\255\255\034\001\035\001\036\001\037\001\034\001\035\001\
\036\001\037\001\019\001\020\001\021\001\022\001\255\255\255\255\
\255\255\026\001\019\001\020\001\021\001\022\001\255\255\255\255\
\255\255\034\001\035\001\036\001\037\001\030\001\255\255\255\255\
\255\255\034\001\035\001\036\001\037\001\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\034\001\035\001\036\001\037\001"

let yynames_const = "\
  INT\000\
  IF\000\
  WHILE\000\
  SPRINT\000\
  IPRINT\000\
  SCAN\000\
  EQ\000\
  NEQ\000\
  GT\000\
  LT\000\
  GE\000\
  LE\000\
  ELSE\000\
  RETURN\000\
  NEW\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LB\000\
  RB\000\
  LS\000\
  RS\000\
  LP\000\
  RP\000\
  ASSIGN\000\
  SEMI\000\
  COMMA\000\
  TYPE\000\
  VOID\000\
  MOD\000\
  POW\000\
  INC\000\
  ADD_EQ\000\
  DO\000\
  FOR\000\
  "

let yynames_block = "\
  NUM\000\
  STR\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 31 "parser.mly"
             (  _1  )
# 365 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
           ( IntTyp )
# 371 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 35 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 378 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "parser.mly"
               ( NameTyp _1 )
# 385 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 39 "parser.mly"
                ( _1@_2 )
# 393 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                ( [] )
# 399 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 43 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 407 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 44 "parser.mly"
                              ( [TypeDec (_2,_4)] )
# 415 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 425 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 46 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 434 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
                       ( _1@[_3] )
# 442 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                       ( [_1]  )
# 449 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                        ( [] )
# 455 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 54 "parser.mly"
                        ( _1 )
# 462 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                             ( _1@[(_3,_4)] )
# 471 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                             ( [(_1,_2)] )
# 479 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 61 "parser.mly"
                   ( _1@[_2] )
# 487 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                   ( [_1] )
# 494 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                              ( Assign (Var _1, _3) )
# 502 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 511 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser.mly"
                              ( If (_3, _5, None) )
# 519 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 528 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                              ( While (_3, _5) )
# 536 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 71 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 543 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 550 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 73 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 557 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 74 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 564 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 75 "parser.mly"
                                ( CallProc (_1, _3) )
# 572 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 579 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 77 "parser.mly"
             ( _1 )
# 586 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
            ( NilStmt )
# 592 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                           ( [] )
# 598 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 82 "parser.mly"
                           ( _1 )
# 605 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                          ( _1@[_3] )
# 613 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                           ( [_1] )
# 620 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 89 "parser.mly"
                         ( Block (_2, _3) )
# 628 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 92 "parser.mly"
           ( IntExp _1  )
# 635 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
          ( VarExp (Var _1) )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 94 "parser.mly"
                          ( CallFunc (_1, _3) )
# 650 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 674 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 682 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 690 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( CallFunc ("%", [_1; _3]) )
# 698 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( CallFunc ("^", [_1; _3]) )
# 706 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                ( CallFunc ("++", [_1]) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                        ( CallFunc ("+=", [_1; _3]) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 728 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                   ( _2 )
# 735 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 743 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 751 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 759 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 767 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 775 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 783 "parser.ml"
               : 'cond))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stmt)
;;
