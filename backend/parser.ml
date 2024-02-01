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
  | TO

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Printf
open Ast
# 49 "parser.ml"
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
  296 (* TO *);
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
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\013\000\013\000\014\000\014\000\008\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\012\000\012\000\012\000\012\000\
\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\002\000\000\000\003\000\005\000\
\006\000\006\000\003\000\001\000\000\000\001\000\004\000\002\000\
\002\000\001\000\004\000\007\000\005\000\007\000\005\000\006\000\
\009\000\005\000\005\000\005\000\005\000\005\000\003\000\001\000\
\001\000\004\000\002\000\000\000\001\000\003\000\001\000\004\000\
\001\000\001\000\004\000\004\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\033\000\000\000\000\000\059\000\001\000\
\032\000\000\000\000\000\000\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\000\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\034\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\017\000\000\000\
\000\000\000\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\026\000\027\000\028\000\044\000\
\043\000\029\000\000\000\000\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\003\000\004\000\000\000\000\000\
\000\000\000\000\000\000\011\000\024\000\000\000\020\000\022\000\
\008\000\016\000\000\000\000\000\000\000\000\000\010\000\000\000\
\009\000\000\000\015\000\025\000"

let yydgoto = "\002\000\
\015\000\016\000\136\000\034\000\067\000\101\000\137\000\017\000\
\138\000\068\000\043\000\044\000\039\000\040\000"

let yysindex = "\004\000\
\145\255\000\000\193\000\244\254\252\254\014\255\080\255\081\255\
\005\255\086\255\000\000\000\000\145\255\091\255\000\000\000\000\
\000\000\005\255\005\255\005\255\000\000\005\255\005\255\005\255\
\017\255\005\255\057\255\000\000\015\255\005\255\005\255\248\254\
\070\255\082\255\103\255\114\255\046\255\192\000\095\255\098\255\
\076\255\237\255\024\255\102\255\104\255\105\255\129\000\106\255\
\005\255\005\255\096\255\150\000\005\255\005\255\005\255\005\255\
\000\000\005\255\005\255\107\255\193\000\113\255\136\255\152\255\
\000\000\153\255\000\000\119\255\134\255\135\255\137\255\146\255\
\005\255\000\000\000\000\005\255\005\255\005\255\005\255\005\255\
\005\255\145\255\145\255\147\255\156\255\158\255\154\000\139\255\
\000\000\028\255\028\255\096\255\096\255\096\255\192\000\161\255\
\181\255\163\255\168\255\169\255\235\254\000\000\000\000\005\255\
\005\255\005\255\000\000\192\000\192\000\192\000\192\000\192\000\
\192\000\192\000\182\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\171\255\090\255\090\255\090\255\000\000\203\255\
\179\255\125\255\171\000\145\255\000\000\000\000\178\255\210\255\
\186\255\184\255\189\255\000\000\000\000\005\255\000\000\000\000\
\000\000\000\000\196\255\090\255\196\255\175\000\000\000\219\255\
\000\000\145\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\195\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\159\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\255\000\000\197\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\195\255\190\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\223\255\001\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\084\000\106\000\221\255\022\000\053\000\128\000\000\000\
\000\000\000\000\000\000\071\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\255\199\255\200\255\201\255\209\255\
\211\255\216\255\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\217\255\217\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\218\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\243\255\224\255\000\000\000\000\000\000\112\000\191\255\
\000\000\000\000\254\255\235\255\198\000\000\000"

let yytablesize = 486
let yytable = "\035\000\
\021\000\066\000\045\000\002\000\001\000\028\000\032\000\029\000\
\127\000\128\000\053\000\054\000\055\000\056\000\023\000\037\000\
\038\000\041\000\046\000\042\000\065\000\057\000\024\000\047\000\
\030\000\058\000\059\000\051\000\052\000\039\000\002\000\031\000\
\039\000\076\000\077\000\078\000\079\000\080\000\081\000\049\000\
\025\000\050\000\053\000\054\000\055\000\056\000\087\000\038\000\
\055\000\056\000\090\000\091\000\092\000\093\000\103\000\094\000\
\095\000\058\000\059\000\048\000\038\000\058\000\059\000\038\000\
\053\000\054\000\055\000\056\000\115\000\116\000\108\000\071\000\
\060\000\109\000\110\000\111\000\112\000\113\000\114\000\058\000\
\059\000\151\000\129\000\153\000\061\000\062\000\004\000\005\000\
\006\000\007\000\008\000\135\000\134\000\062\000\053\000\054\000\
\055\000\056\000\009\000\010\000\012\000\012\000\130\000\131\000\
\011\000\074\000\026\000\027\000\069\000\058\000\059\000\012\000\
\033\000\063\000\064\000\152\000\070\000\036\000\144\000\013\000\
\014\000\003\000\072\000\004\000\005\000\006\000\007\000\008\000\
\073\000\082\000\059\000\083\000\084\000\086\000\096\000\009\000\
\010\000\097\000\098\000\150\000\156\000\011\000\102\000\053\000\
\054\000\055\000\056\000\003\000\012\000\004\000\005\000\006\000\
\007\000\008\000\099\000\100\000\013\000\014\000\058\000\059\000\
\104\000\009\000\010\000\105\000\142\000\106\000\121\000\011\000\
\042\000\042\000\042\000\042\000\042\000\042\000\012\000\107\000\
\117\000\042\000\042\000\042\000\042\000\123\000\013\000\014\000\
\042\000\118\000\042\000\119\000\042\000\042\000\122\000\124\000\
\042\000\042\000\125\000\126\000\133\000\132\000\042\000\051\000\
\051\000\051\000\051\000\051\000\051\000\140\000\141\000\145\000\
\051\000\051\000\051\000\051\000\146\000\147\000\148\000\051\000\
\149\000\051\000\011\000\051\000\051\000\155\000\036\000\051\000\
\037\000\004\000\053\000\054\000\055\000\051\000\047\000\047\000\
\047\000\047\000\047\000\047\000\056\000\139\000\057\000\047\000\
\047\000\047\000\047\000\058\000\013\000\014\000\047\000\088\000\
\047\000\000\000\047\000\047\000\000\000\000\000\047\000\053\000\
\054\000\055\000\056\000\021\000\047\000\021\000\021\000\021\000\
\021\000\021\000\075\000\000\000\000\000\000\000\058\000\059\000\
\000\000\021\000\021\000\000\000\000\000\000\000\000\000\021\000\
\021\000\000\000\000\000\000\000\000\000\000\000\021\000\048\000\
\048\000\048\000\048\000\048\000\048\000\000\000\021\000\021\000\
\048\000\048\000\048\000\048\000\000\000\000\000\000\000\048\000\
\000\000\048\000\000\000\048\000\048\000\000\000\000\000\048\000\
\000\000\000\000\000\000\000\000\000\000\048\000\049\000\049\000\
\049\000\049\000\049\000\049\000\000\000\000\000\000\000\049\000\
\049\000\049\000\049\000\000\000\000\000\000\000\049\000\000\000\
\049\000\000\000\049\000\049\000\000\000\000\000\049\000\000\000\
\000\000\000\000\000\000\000\000\049\000\045\000\045\000\045\000\
\045\000\045\000\045\000\000\000\000\000\000\000\045\000\045\000\
\000\000\000\000\000\000\000\000\000\000\045\000\000\000\045\000\
\000\000\045\000\045\000\046\000\046\000\046\000\046\000\046\000\
\046\000\000\000\000\000\045\000\046\000\046\000\000\000\000\000\
\000\000\000\000\000\000\046\000\000\000\046\000\000\000\046\000\
\046\000\050\000\050\000\050\000\050\000\050\000\050\000\000\000\
\000\000\046\000\000\000\053\000\054\000\055\000\056\000\000\000\
\000\000\050\000\000\000\050\000\085\000\050\000\050\000\000\000\
\000\000\000\000\058\000\059\000\000\000\000\000\000\000\050\000\
\053\000\054\000\055\000\056\000\053\000\054\000\055\000\056\000\
\000\000\089\000\000\000\120\000\000\000\000\000\000\000\058\000\
\059\000\000\000\000\000\058\000\059\000\053\000\054\000\055\000\
\056\000\053\000\054\000\055\000\056\000\000\000\000\000\000\000\
\143\000\000\000\154\000\000\000\058\000\059\000\000\000\000\000\
\058\000\059\000\053\000\054\000\055\000\056\000\000\000\000\000\
\000\000\018\000\000\000\019\000\000\000\020\000\000\000\000\000\
\000\000\058\000\059\000\000\000\021\000\022\000"

let yycheck = "\013\000\
\000\000\034\000\024\000\003\001\001\000\001\001\009\000\003\001\
\030\001\031\001\019\001\020\001\021\001\022\001\027\001\018\000\
\019\000\020\000\002\001\022\000\034\000\030\001\027\001\026\000\
\020\001\034\001\035\001\030\000\031\000\028\001\030\001\027\001\
\031\001\010\001\011\001\012\001\013\001\014\001\015\001\025\001\
\027\001\027\001\019\001\020\001\021\001\022\001\049\000\050\000\
\021\001\022\001\053\000\054\000\055\000\056\000\068\000\058\000\
\059\000\034\001\035\001\003\001\028\001\034\001\035\001\031\001\
\019\001\020\001\021\001\022\001\082\000\083\000\073\000\026\001\
\003\001\076\000\077\000\078\000\079\000\080\000\081\000\034\001\
\035\001\147\000\104\000\149\000\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\124\000\003\001\004\001\019\001\020\001\
\021\001\022\001\017\001\018\001\030\001\031\001\105\000\106\000\
\023\001\030\001\027\001\027\001\006\001\034\001\035\001\030\001\
\027\001\032\001\033\001\148\000\003\001\027\001\132\000\038\001\
\039\001\003\001\028\001\005\001\006\001\007\001\008\001\009\001\
\031\001\028\001\035\001\028\001\028\001\028\001\028\001\017\001\
\018\001\025\001\003\001\142\000\154\000\023\001\024\001\019\001\
\020\001\021\001\022\001\003\001\030\001\005\001\006\001\007\001\
\008\001\009\001\003\001\003\001\038\001\039\001\034\001\035\001\
\027\001\017\001\018\001\029\001\040\001\029\001\028\001\023\001\
\010\001\011\001\012\001\013\001\014\001\015\001\030\001\030\001\
\030\001\019\001\020\001\021\001\022\001\001\001\038\001\039\001\
\026\001\030\001\028\001\030\001\030\001\031\001\030\001\029\001\
\034\001\035\001\027\001\027\001\026\001\016\001\040\001\010\001\
\011\001\012\001\013\001\014\001\015\001\003\001\028\001\030\001\
\019\001\020\001\021\001\022\001\003\001\028\001\031\001\026\001\
\028\001\028\001\023\001\030\001\031\001\003\001\028\001\034\001\
\028\001\003\001\028\001\028\001\028\001\040\001\010\001\011\001\
\012\001\013\001\014\001\015\001\028\001\126\000\028\001\019\001\
\020\001\021\001\022\001\028\001\028\001\028\001\026\001\050\000\
\028\001\255\255\030\001\031\001\255\255\255\255\034\001\019\001\
\020\001\021\001\022\001\003\001\040\001\005\001\006\001\007\001\
\008\001\009\001\030\001\255\255\255\255\255\255\034\001\035\001\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\255\255\255\255\255\255\255\255\255\255\030\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\038\001\039\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\026\001\
\255\255\028\001\255\255\030\001\031\001\255\255\255\255\034\001\
\255\255\255\255\255\255\255\255\255\255\040\001\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001\255\255\255\255\255\255\026\001\255\255\
\028\001\255\255\030\001\031\001\255\255\255\255\034\001\255\255\
\255\255\255\255\255\255\255\255\040\001\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\255\255\019\001\020\001\
\255\255\255\255\255\255\255\255\255\255\026\001\255\255\028\001\
\255\255\030\001\031\001\010\001\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\040\001\019\001\020\001\255\255\255\255\
\255\255\255\255\255\255\026\001\255\255\028\001\255\255\030\001\
\031\001\010\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\040\001\255\255\019\001\020\001\021\001\022\001\255\255\
\255\255\026\001\255\255\028\001\028\001\030\001\031\001\255\255\
\255\255\255\255\034\001\035\001\255\255\255\255\255\255\040\001\
\019\001\020\001\021\001\022\001\019\001\020\001\021\001\022\001\
\255\255\028\001\255\255\026\001\255\255\255\255\255\255\034\001\
\035\001\255\255\255\255\034\001\035\001\019\001\020\001\021\001\
\022\001\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\030\001\255\255\028\001\255\255\034\001\035\001\255\255\255\255\
\034\001\035\001\019\001\020\001\021\001\022\001\255\255\255\255\
\255\255\025\001\255\255\027\001\255\255\029\001\255\255\255\255\
\255\255\034\001\035\001\255\255\036\001\037\001"

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
  TO\000\
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
# 32 "parser.mly"
             (  _1  )
# 370 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
           ( IntTyp )
# 376 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 36 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 383 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parser.mly"
               ( NameTyp _1 )
# 390 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 40 "parser.mly"
                ( _1@_2 )
# 398 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                ( [] )
# 404 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 44 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 412 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 45 "parser.mly"
                              ( [TypeDec (_2,_4)] )
# 420 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 46 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 430 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 47 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 439 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                       ( _1@[_3] )
# 447 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
                       ( [_1]  )
# 454 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                        ( [] )
# 460 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 55 "parser.mly"
                        ( _1 )
# 467 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                             ( _1@[(_3,_4)] )
# 476 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
                             ( [(_1,_2)] )
# 484 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                   ( _1@[_2] )
# 492 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 63 "parser.mly"
                   ( [_1] )
# 499 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                              ( Assign (Var _1, _3) )
# 507 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 516 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 68 "parser.mly"
                              ( If (_3, _5, None) )
# 524 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 533 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 71 "parser.mly"
                              ( While (_3, _5) )
# 541 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'stmt) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'cond) in
    Obj.repr(
# 72 "parser.mly"
                                ( DoWhile (_5, _2) )
# 549 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 73 "parser.mly"
                                             ( For (Var _3, _5, _7, _9) )
# 559 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 74 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 566 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 573 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 76 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 580 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 77 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 587 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 78 "parser.mly"
                                ( CallProc (_1, _3) )
# 595 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 602 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 80 "parser.mly"
             ( _1 )
# 609 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
            ( NilStmt )
# 615 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                           ( AddEq ((Var _1), _3) )
# 623 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 83 "parser.mly"
              ( Incr (Var _1) )
# 630 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                           ( [] )
# 636 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 87 "parser.mly"
                           ( _1 )
# 643 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                          ( _1@[_3] )
# 651 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                           ( [_1] )
# 658 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 94 "parser.mly"
                         ( Block (_2, _3) )
# 666 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 97 "parser.mly"
           ( IntExp _1  )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
          ( VarExp (Var _1) )
# 680 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 99 "parser.mly"
                          ( CallFunc (_1, _3) )
# 688 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 696 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 704 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 712 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 720 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 728 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     ( CallFunc ("%", [_1; _3]) )
# 736 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( CallFunc ("^", [_1; _3]) )
# 744 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                   ( _2 )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 766 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 774 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 782 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 790 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 798 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 806 "parser.ml"
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
