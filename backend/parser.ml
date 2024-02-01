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
\002\000\002\000\013\000\013\000\014\000\014\000\008\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\002\000\000\000\003\000\005\000\
\006\000\006\000\003\000\001\000\000\000\001\000\004\000\002\000\
\002\000\001\000\004\000\007\000\005\000\007\000\005\000\006\000\
\005\000\005\000\005\000\005\000\005\000\003\000\001\000\001\000\
\004\000\002\000\000\000\001\000\003\000\001\000\004\000\001\000\
\001\000\004\000\004\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\032\000\000\000\058\000\001\000\031\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\019\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\051\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\000\017\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\025\000\026\000\027\000\043\000\042\000\028\000\000\000\000\000\
\000\000\000\000\007\000\000\000\000\000\000\000\000\000\003\000\
\004\000\000\000\000\000\000\000\000\000\000\000\011\000\024\000\
\020\000\022\000\008\000\016\000\000\000\000\000\000\000\010\000\
\000\000\009\000\015\000"

let yydgoto = "\002\000\
\014\000\015\000\131\000\033\000\065\000\098\000\132\000\016\000\
\133\000\066\000\041\000\042\000\037\000\038\000"

let yysindex = "\004\000\
\147\255\000\000\212\255\240\254\243\254\247\254\006\255\010\255\
\005\255\020\255\000\000\000\000\147\255\000\000\000\000\000\000\
\005\255\005\255\005\255\000\000\005\255\005\255\005\255\058\255\
\005\255\067\255\000\000\255\254\005\255\005\255\112\255\076\255\
\087\255\055\255\022\255\063\255\052\255\056\255\121\000\138\000\
\187\255\060\255\071\255\073\255\142\000\074\255\005\255\005\255\
\051\255\159\000\005\255\005\255\005\255\005\255\000\000\005\255\
\005\255\075\255\212\255\081\255\104\255\105\255\000\000\106\255\
\000\000\121\255\085\255\084\255\086\255\005\255\000\000\000\000\
\005\255\005\255\005\255\005\255\005\255\005\255\147\255\147\255\
\088\255\091\255\092\255\043\255\095\255\000\000\000\255\000\255\
\051\255\051\255\051\255\063\255\107\255\114\255\111\255\108\255\
\109\255\009\255\000\000\000\000\005\255\005\255\000\000\063\255\
\063\255\063\255\063\255\063\255\063\255\063\255\125\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\117\255\026\255\
\026\255\026\255\000\000\145\255\129\255\169\000\147\255\000\000\
\000\000\119\255\155\255\132\255\130\255\134\255\000\000\000\000\
\000\000\000\000\000\000\000\000\140\255\026\255\140\255\000\000\
\163\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\139\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\161\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\237\254\000\000\141\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\139\255\
\204\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\165\255\001\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\097\000\119\000\
\022\000\047\000\072\000\241\255\000\000\000\000\000\000\000\000\
\028\255\000\000\000\000\000\000\000\000\000\000\000\000\238\254\
\150\255\156\255\158\255\160\255\162\255\166\255\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\175\255\175\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\176\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\243\255\225\255\000\000\000\000\000\000\057\000\151\255\
\000\000\000\000\254\255\236\255\145\000\000\000"

let yytablesize = 460
let yytable = "\034\000\
\021\000\064\000\043\000\002\000\001\000\027\000\031\000\028\000\
\038\000\037\000\022\000\038\000\037\000\023\000\035\000\036\000\
\039\000\024\000\040\000\063\000\053\000\054\000\045\000\047\000\
\029\000\048\000\049\000\050\000\129\000\060\000\002\000\030\000\
\025\000\056\000\057\000\144\000\026\000\146\000\123\000\124\000\
\051\000\052\000\053\000\054\000\084\000\036\000\032\000\068\000\
\087\000\088\000\089\000\090\000\100\000\091\000\092\000\056\000\
\057\000\012\000\012\000\044\000\067\000\051\000\052\000\053\000\
\054\000\111\000\112\000\104\000\116\000\046\000\105\000\106\000\
\107\000\108\000\109\000\110\000\056\000\057\000\058\000\069\000\
\125\000\051\000\052\000\053\000\054\000\057\000\070\000\079\000\
\130\000\059\000\060\000\004\000\005\000\006\000\007\000\008\000\
\056\000\057\000\080\000\126\000\081\000\083\000\093\000\009\000\
\010\000\094\000\095\000\096\000\097\000\011\000\145\000\101\000\
\102\000\138\000\119\000\103\000\012\000\113\000\061\000\062\000\
\114\000\115\000\117\000\003\000\013\000\004\000\005\000\006\000\
\007\000\008\000\051\000\052\000\053\000\054\000\121\000\122\000\
\118\000\009\000\010\000\120\000\127\000\055\000\128\000\011\000\
\099\000\056\000\057\000\135\000\139\000\003\000\012\000\004\000\
\005\000\006\000\007\000\008\000\136\000\140\000\013\000\141\000\
\142\000\143\000\011\000\009\000\010\000\147\000\035\000\004\000\
\036\000\011\000\041\000\041\000\041\000\041\000\041\000\041\000\
\012\000\052\000\134\000\041\000\041\000\041\000\041\000\053\000\
\013\000\054\000\041\000\055\000\041\000\056\000\041\000\041\000\
\085\000\057\000\041\000\041\000\073\000\074\000\075\000\076\000\
\077\000\078\000\013\000\014\000\000\000\051\000\052\000\053\000\
\054\000\000\000\000\000\000\000\000\000\050\000\050\000\050\000\
\050\000\050\000\050\000\000\000\056\000\057\000\050\000\050\000\
\050\000\050\000\000\000\000\000\000\000\050\000\000\000\050\000\
\000\000\050\000\050\000\000\000\017\000\050\000\018\000\000\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\021\000\000\000\049\000\049\000\049\000\049\000\049\000\049\000\
\000\000\000\000\000\000\021\000\000\000\021\000\021\000\021\000\
\021\000\021\000\049\000\000\000\049\000\000\000\049\000\049\000\
\000\000\021\000\021\000\000\000\000\000\000\000\000\000\021\000\
\021\000\000\000\000\000\000\000\000\000\000\000\021\000\046\000\
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
\000\000\045\000\045\000\051\000\052\000\053\000\054\000\000\000\
\045\000\000\000\045\000\000\000\045\000\045\000\071\000\000\000\
\000\000\000\000\056\000\057\000\051\000\052\000\053\000\054\000\
\051\000\052\000\053\000\054\000\000\000\000\000\000\000\072\000\
\000\000\082\000\000\000\056\000\057\000\000\000\000\000\056\000\
\057\000\051\000\052\000\053\000\054\000\000\000\000\000\000\000\
\000\000\000\000\086\000\051\000\052\000\053\000\054\000\000\000\
\056\000\057\000\000\000\000\000\000\000\000\000\137\000\000\000\
\000\000\000\000\056\000\057\000"

let yycheck = "\013\000\
\000\000\033\000\023\000\003\001\001\000\001\001\009\000\003\001\
\028\001\028\001\027\001\031\001\031\001\027\001\017\000\018\000\
\019\000\027\001\021\000\033\000\021\001\022\001\025\000\025\001\
\020\001\027\001\029\000\030\000\003\001\004\001\030\001\027\001\
\027\001\034\001\035\001\141\000\027\001\143\000\030\001\031\001\
\019\001\020\001\021\001\022\001\047\000\048\000\027\001\026\001\
\051\000\052\000\053\000\054\000\066\000\056\000\057\000\034\001\
\035\001\030\001\031\001\002\001\006\001\019\001\020\001\021\001\
\022\001\079\000\080\000\070\000\026\001\003\001\073\000\074\000\
\075\000\076\000\077\000\078\000\034\001\035\001\003\001\028\001\
\101\000\019\001\020\001\021\001\022\001\035\001\031\001\028\001\
\120\000\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\034\001\035\001\028\001\102\000\028\001\028\001\028\001\017\001\
\018\001\025\001\003\001\003\001\003\001\023\001\142\000\027\001\
\029\001\127\000\001\001\030\001\030\001\030\001\032\001\033\001\
\030\001\030\001\028\001\003\001\038\001\005\001\006\001\007\001\
\008\001\009\001\019\001\020\001\021\001\022\001\027\001\027\001\
\030\001\017\001\018\001\029\001\016\001\030\001\026\001\023\001\
\024\001\034\001\035\001\003\001\030\001\003\001\030\001\005\001\
\006\001\007\001\008\001\009\001\028\001\003\001\038\001\028\001\
\031\001\028\001\023\001\017\001\018\001\003\001\028\001\003\001\
\028\001\023\001\010\001\011\001\012\001\013\001\014\001\015\001\
\030\001\028\001\122\000\019\001\020\001\021\001\022\001\028\001\
\038\001\028\001\026\001\028\001\028\001\028\001\030\001\031\001\
\048\000\028\001\034\001\035\001\010\001\011\001\012\001\013\001\
\014\001\015\001\028\001\028\001\255\255\019\001\020\001\021\001\
\022\001\255\255\255\255\255\255\255\255\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\034\001\035\001\019\001\020\001\
\021\001\022\001\255\255\255\255\255\255\026\001\255\255\028\001\
\255\255\030\001\031\001\255\255\025\001\034\001\027\001\255\255\
\029\001\255\255\255\255\255\255\255\255\255\255\255\255\036\001\
\037\001\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\026\001\255\255\028\001\255\255\030\001\031\001\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\255\255\255\255\255\255\255\255\255\255\030\001\010\001\
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
\255\255\019\001\020\001\019\001\020\001\021\001\022\001\255\255\
\026\001\255\255\028\001\255\255\030\001\031\001\030\001\255\255\
\255\255\255\255\034\001\035\001\019\001\020\001\021\001\022\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\030\001\
\255\255\028\001\255\255\034\001\035\001\255\255\255\255\034\001\
\035\001\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\255\255\255\255\028\001\019\001\020\001\021\001\022\001\255\255\
\034\001\035\001\255\255\255\255\255\255\255\255\030\001\255\255\
\255\255\255\255\034\001\035\001"

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
# 358 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
           ( IntTyp )
# 364 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 35 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 371 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "parser.mly"
               ( NameTyp _1 )
# 378 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 39 "parser.mly"
                ( _1@_2 )
# 386 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                ( [] )
# 392 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 43 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 400 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 44 "parser.mly"
                              ( [TypeDec (_2,_4)] )
# 408 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 418 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 46 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 427 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
                       ( _1@[_3] )
# 435 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                       ( [_1]  )
# 442 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                        ( [] )
# 448 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 54 "parser.mly"
                        ( _1 )
# 455 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                             ( _1@[(_3,_4)] )
# 464 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                             ( [(_1,_2)] )
# 472 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 61 "parser.mly"
                   ( _1@[_2] )
# 480 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                   ( [_1] )
# 487 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                              ( Assign (Var _1, _3) )
# 495 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 504 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 67 "parser.mly"
                              ( If (_3, _5, None) )
# 512 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 521 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                              ( While (_3, _5) )
# 529 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'stmt) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'cond) in
    Obj.repr(
# 71 "parser.mly"
                                ( DoWhile (_5, _2) )
# 537 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 72 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 544 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 551 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 74 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 558 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 75 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 565 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 76 "parser.mly"
                                ( CallProc (_1, _3) )
# 573 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 580 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 78 "parser.mly"
             ( _1 )
# 587 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
            ( NilStmt )
# 593 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                           ( AddEq ((Var _1), _3) )
# 601 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 81 "parser.mly"
              ( Incr (Var _1) )
# 608 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                           ( [] )
# 614 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 85 "parser.mly"
                           ( _1 )
# 621 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                          ( _1@[_3] )
# 629 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                           ( [_1] )
# 636 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 92 "parser.mly"
                         ( Block (_2, _3) )
# 644 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
           ( IntExp _1  )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
          ( VarExp (Var _1) )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 97 "parser.mly"
                          ( CallFunc (_1, _3) )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 674 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 682 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 690 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 698 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 706 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( CallFunc ("%", [_1; _3]) )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( CallFunc ("^", [_1; _3]) )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                   ( _2 )
# 736 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 744 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 752 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 760 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 768 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 776 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 784 "parser.ml"
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
