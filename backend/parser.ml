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
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\013\000\013\000\014\000\014\000\008\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\002\000\000\000\003\000\005\000\
\006\000\006\000\003\000\001\000\000\000\001\000\004\000\002\000\
\002\000\001\000\004\000\007\000\005\000\007\000\005\000\006\000\
\005\000\005\000\005\000\005\000\005\000\003\000\001\000\001\000\
\004\000\000\000\001\000\003\000\001\000\004\000\001\000\001\000\
\004\000\004\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\032\000\000\000\058\000\001\000\031\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\019\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\051\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\017\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\025\000\026\000\027\000\042\000\041\000\028\000\000\000\000\000\
\000\000\000\000\007\000\000\000\000\000\000\000\000\000\003\000\
\004\000\000\000\000\000\000\000\000\000\000\000\011\000\024\000\
\020\000\022\000\008\000\016\000\000\000\000\000\000\000\010\000\
\000\000\009\000\015\000"

let yydgoto = "\002\000\
\014\000\015\000\131\000\032\000\065\000\098\000\132\000\016\000\
\133\000\066\000\040\000\041\000\036\000\037\000"

let yysindex = "\004\000\
\148\255\000\000\009\255\002\255\006\255\020\255\021\255\029\255\
\005\255\030\255\000\000\000\000\148\255\000\000\000\000\000\000\
\005\255\005\255\005\255\005\255\005\255\005\255\056\255\005\255\
\057\255\000\000\003\255\005\255\005\255\113\255\058\255\088\255\
\053\255\043\255\178\000\042\255\048\255\221\255\248\255\188\255\
\052\255\061\255\070\255\132\000\074\255\005\255\005\255\000\000\
\046\255\136\000\005\255\005\255\005\255\005\255\000\000\005\255\
\005\255\079\255\009\255\083\255\106\255\107\255\000\000\110\255\
\000\000\122\255\089\255\086\255\087\255\005\255\000\000\000\000\
\005\255\005\255\005\255\005\255\005\255\005\255\148\255\148\255\
\092\255\093\255\094\255\153\000\091\255\000\000\245\254\245\254\
\046\255\046\255\046\255\178\000\108\255\135\255\112\255\115\255\
\117\255\239\254\000\000\000\000\005\255\005\255\000\000\178\000\
\178\000\178\000\178\000\178\000\178\000\178\000\121\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\123\255\017\255\
\017\255\017\255\000\000\147\255\130\255\161\000\148\255\000\000\
\000\000\129\255\158\255\134\255\132\255\136\255\000\000\000\000\
\000\000\000\000\000\000\000\000\144\255\017\255\144\255\000\000\
\165\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\141\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\162\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\237\254\000\000\142\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\141\255\000\000\
\205\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\176\255\001\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\097\000\119\000\
\022\000\047\000\072\000\073\255\000\000\000\000\000\000\000\000\
\011\255\000\000\000\000\000\000\000\000\000\000\000\000\012\255\
\152\255\157\255\159\255\161\255\163\255\166\255\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\167\255\167\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\177\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\243\255\226\255\000\000\000\000\000\000\082\000\150\255\
\000\000\000\000\254\255\237\255\159\000\000\000"

let yytablesize = 469
let yytable = "\033\000\
\021\000\064\000\042\000\002\000\001\000\026\000\030\000\027\000\
\037\000\053\000\054\000\037\000\123\000\124\000\034\000\035\000\
\038\000\039\000\063\000\129\000\060\000\044\000\056\000\057\000\
\028\000\049\000\050\000\046\000\021\000\047\000\002\000\029\000\
\022\000\017\000\144\000\018\000\146\000\019\000\048\000\036\000\
\012\000\012\000\036\000\084\000\035\000\020\000\023\000\024\000\
\087\000\088\000\089\000\090\000\100\000\091\000\092\000\025\000\
\031\000\043\000\067\000\045\000\058\000\051\000\052\000\053\000\
\054\000\111\000\112\000\104\000\068\000\069\000\105\000\106\000\
\107\000\108\000\109\000\110\000\056\000\057\000\070\000\079\000\
\057\000\125\000\049\000\049\000\049\000\049\000\049\000\049\000\
\080\000\130\000\059\000\060\000\004\000\005\000\006\000\007\000\
\008\000\081\000\049\000\126\000\049\000\083\000\049\000\049\000\
\009\000\010\000\093\000\094\000\095\000\096\000\011\000\145\000\
\097\000\138\000\102\000\101\000\103\000\012\000\117\000\061\000\
\062\000\113\000\114\000\115\000\003\000\013\000\004\000\005\000\
\006\000\007\000\008\000\051\000\052\000\053\000\054\000\119\000\
\127\000\118\000\009\000\010\000\120\000\121\000\055\000\122\000\
\011\000\099\000\056\000\057\000\128\000\135\000\003\000\012\000\
\004\000\005\000\006\000\007\000\008\000\136\000\139\000\013\000\
\140\000\141\000\142\000\143\000\009\000\010\000\011\000\147\000\
\034\000\035\000\011\000\040\000\040\000\040\000\040\000\040\000\
\040\000\012\000\004\000\052\000\040\000\040\000\040\000\040\000\
\053\000\013\000\054\000\040\000\055\000\040\000\056\000\040\000\
\040\000\057\000\013\000\040\000\040\000\073\000\074\000\075\000\
\076\000\077\000\078\000\134\000\014\000\085\000\051\000\052\000\
\053\000\054\000\000\000\000\000\000\000\000\000\050\000\050\000\
\050\000\050\000\050\000\050\000\000\000\056\000\057\000\050\000\
\050\000\050\000\050\000\000\000\000\000\000\000\050\000\000\000\
\050\000\000\000\050\000\050\000\000\000\000\000\050\000\051\000\
\052\000\053\000\054\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\071\000\000\000\000\000\000\000\056\000\057\000\
\000\000\000\000\000\000\021\000\000\000\021\000\021\000\021\000\
\021\000\021\000\051\000\052\000\053\000\054\000\000\000\000\000\
\000\000\021\000\021\000\000\000\000\000\072\000\000\000\021\000\
\021\000\056\000\057\000\000\000\000\000\000\000\021\000\046\000\
\046\000\046\000\046\000\046\000\046\000\000\000\021\000\000\000\
\046\000\046\000\046\000\046\000\000\000\000\000\000\000\046\000\
\000\000\046\000\000\000\046\000\046\000\000\000\000\000\046\000\
\047\000\047\000\047\000\047\000\047\000\047\000\000\000\000\000\
\000\000\047\000\047\000\047\000\047\000\000\000\000\000\000\000\
\047\000\000\000\047\000\000\000\047\000\047\000\000\000\000\000\
\047\000\048\000\048\000\048\000\048\000\048\000\048\000\000\000\
\000\000\000\000\048\000\048\000\048\000\048\000\000\000\000\000\
\000\000\048\000\000\000\048\000\000\000\048\000\048\000\000\000\
\000\000\048\000\044\000\044\000\044\000\044\000\044\000\044\000\
\000\000\000\000\000\000\044\000\044\000\000\000\000\000\000\000\
\000\000\000\000\044\000\000\000\044\000\000\000\044\000\044\000\
\045\000\045\000\045\000\045\000\045\000\045\000\000\000\000\000\
\000\000\045\000\045\000\000\000\000\000\000\000\000\000\000\000\
\045\000\000\000\045\000\000\000\045\000\045\000\051\000\052\000\
\053\000\054\000\051\000\052\000\053\000\054\000\000\000\082\000\
\000\000\000\000\000\000\086\000\000\000\056\000\057\000\000\000\
\000\000\056\000\057\000\051\000\052\000\053\000\054\000\000\000\
\000\000\000\000\116\000\051\000\052\000\053\000\054\000\000\000\
\000\000\000\000\056\000\057\000\000\000\000\000\137\000\000\000\
\000\000\000\000\056\000\057\000\051\000\052\000\053\000\054\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\056\000\057\000"

let yycheck = "\013\000\
\000\000\032\000\022\000\003\001\001\000\001\001\009\000\003\001\
\028\001\021\001\022\001\031\001\030\001\031\001\017\000\018\000\
\019\000\020\000\032\000\003\001\004\001\024\000\034\001\035\001\
\020\001\028\000\029\000\025\001\027\001\027\001\030\001\027\001\
\027\001\025\001\141\000\027\001\143\000\029\001\036\001\028\001\
\030\001\031\001\031\001\046\000\047\000\037\001\027\001\027\001\
\051\000\052\000\053\000\054\000\066\000\056\000\057\000\027\001\
\027\001\002\001\006\001\003\001\003\001\019\001\020\001\021\001\
\022\001\079\000\080\000\070\000\026\001\028\001\073\000\074\000\
\075\000\076\000\077\000\078\000\034\001\035\001\031\001\028\001\
\035\001\101\000\010\001\011\001\012\001\013\001\014\001\015\001\
\028\001\120\000\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\028\001\026\001\102\000\028\001\028\001\030\001\031\001\
\017\001\018\001\028\001\025\001\003\001\003\001\023\001\142\000\
\003\001\127\000\029\001\027\001\030\001\030\001\028\001\032\001\
\033\001\030\001\030\001\030\001\003\001\038\001\005\001\006\001\
\007\001\008\001\009\001\019\001\020\001\021\001\022\001\001\001\
\016\001\030\001\017\001\018\001\029\001\027\001\030\001\027\001\
\023\001\024\001\034\001\035\001\026\001\003\001\003\001\030\001\
\005\001\006\001\007\001\008\001\009\001\028\001\030\001\038\001\
\003\001\028\001\031\001\028\001\017\001\018\001\023\001\003\001\
\028\001\028\001\023\001\010\001\011\001\012\001\013\001\014\001\
\015\001\030\001\003\001\028\001\019\001\020\001\021\001\022\001\
\028\001\038\001\028\001\026\001\028\001\028\001\028\001\030\001\
\031\001\028\001\028\001\034\001\035\001\010\001\011\001\012\001\
\013\001\014\001\015\001\122\000\028\001\047\000\019\001\020\001\
\021\001\022\001\255\255\255\255\255\255\255\255\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\034\001\035\001\019\001\
\020\001\021\001\022\001\255\255\255\255\255\255\026\001\255\255\
\028\001\255\255\030\001\031\001\255\255\255\255\034\001\019\001\
\020\001\021\001\022\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\030\001\255\255\255\255\255\255\034\001\035\001\
\255\255\255\255\255\255\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\019\001\020\001\021\001\022\001\255\255\255\255\
\255\255\017\001\018\001\255\255\255\255\030\001\255\255\023\001\
\024\001\034\001\035\001\255\255\255\255\255\255\030\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\038\001\255\255\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\026\001\
\255\255\028\001\255\255\030\001\031\001\255\255\255\255\034\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\026\001\255\255\028\001\255\255\030\001\031\001\255\255\255\255\
\034\001\010\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\019\001\020\001\021\001\022\001\255\255\255\255\
\255\255\026\001\255\255\028\001\255\255\030\001\031\001\255\255\
\255\255\034\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\019\001\020\001\255\255\255\255\255\255\
\255\255\255\255\026\001\255\255\028\001\255\255\030\001\031\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\255\255\255\255\255\255\
\026\001\255\255\028\001\255\255\030\001\031\001\019\001\020\001\
\021\001\022\001\019\001\020\001\021\001\022\001\255\255\028\001\
\255\255\255\255\255\255\028\001\255\255\034\001\035\001\255\255\
\255\255\034\001\035\001\019\001\020\001\021\001\022\001\255\255\
\255\255\255\255\026\001\019\001\020\001\021\001\022\001\255\255\
\255\255\255\255\034\001\035\001\255\255\255\255\030\001\255\255\
\255\255\255\255\034\001\035\001\019\001\020\001\021\001\022\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\034\001\035\001"

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
# 360 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
           ( IntTyp )
# 366 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 35 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 373 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "parser.mly"
               ( NameTyp _1 )
# 380 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 39 "parser.mly"
                ( _1@_2 )
# 388 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                ( [] )
# 394 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 43 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 402 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 44 "parser.mly"
                              ( [TypeDec (_2,_4)] )
# 410 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 420 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 46 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 429 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
                       ( _1@[_3] )
# 437 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                       ( [_1]  )
# 444 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                        ( [] )
# 450 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 54 "parser.mly"
                        ( _1 )
# 457 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                             ( _1@[(_3,_4)] )
# 466 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                             ( [(_1,_2)] )
# 474 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 61 "parser.mly"
                   ( _1@[_2] )
# 482 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                   ( [_1] )
# 489 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                              ( Assign (Var _1, _3) )
# 497 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 506 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser.mly"
                              ( If (_3, _5, None) )
# 514 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 523 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                              ( While (_3, _5) )
# 531 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'stmt) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'cond) in
    Obj.repr(
# 71 "parser.mly"
                                ( DoWhile (_5, _2) )
# 539 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 72 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 546 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 553 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 74 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 560 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 75 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 567 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 76 "parser.mly"
                                ( CallProc (_1, _3) )
# 575 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 582 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 78 "parser.mly"
             ( _1 )
# 589 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
            ( NilStmt )
# 595 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                           ( AddEq ((Var _1), _3) )
# 603 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                           ( [] )
# 609 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 84 "parser.mly"
                           ( _1 )
# 616 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                          ( _1@[_3] )
# 624 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                           ( [_1] )
# 631 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 91 "parser.mly"
                         ( Block (_2, _3) )
# 639 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "parser.mly"
           ( IntExp _1  )
# 646 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parser.mly"
          ( VarExp (Var _1) )
# 653 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 96 "parser.mly"
                          ( CallFunc (_1, _3) )
# 661 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 98 "parser.mly"
              ( Incr (Var _1) )
# 676 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 700 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 708 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( CallFunc ("%", [_1; _3]) )
# 716 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( CallFunc ("^", [_1; _3]) )
# 724 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 731 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                   ( _2 )
# 738 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 746 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 754 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 762 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 770 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 778 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 786 "parser.ml"
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
