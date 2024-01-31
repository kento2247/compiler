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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Printf
open Ast
# 43 "parser.ml"
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
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\012\000\
\012\000\012\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\002\000\000\000\003\000\005\000\
\006\000\006\000\003\000\001\000\000\000\001\000\004\000\002\000\
\002\000\001\000\004\000\007\000\005\000\007\000\005\000\005\000\
\005\000\005\000\005\000\005\000\003\000\001\000\001\000\000\000\
\001\000\003\000\001\000\004\000\001\000\001\000\004\000\004\000\
\003\000\003\000\003\000\003\000\003\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\031\000\054\000\001\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\037\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\000\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\000\000\005\000\000\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\000\000\000\000\
\043\000\044\000\045\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\017\000\000\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\024\000\025\000\
\026\000\040\000\039\000\027\000\000\000\000\000\000\000\000\000\
\007\000\000\000\000\000\000\000\003\000\004\000\000\000\000\000\
\000\000\000\000\000\000\011\000\020\000\022\000\008\000\016\000\
\000\000\000\000\000\000\010\000\000\000\009\000\015\000"

let yydgoto = "\002\000\
\013\000\014\000\120\000\030\000\059\000\089\000\121\000\015\000\
\122\000\060\000\032\000\037\000\033\000\034\000"

let yysindex = "\028\000\
\126\255\000\000\243\254\006\255\013\255\028\255\033\255\034\255\
\004\255\043\255\000\000\000\000\000\000\000\000\000\000\004\255\
\004\255\004\255\004\255\004\255\036\255\004\255\076\255\000\000\
\235\254\004\255\004\255\063\255\078\255\069\255\037\255\015\255\
\061\255\059\255\091\255\165\255\066\255\067\255\068\255\213\255\
\070\255\004\255\004\255\000\000\217\255\004\255\004\255\004\255\
\004\255\000\000\004\255\086\255\243\254\079\255\112\255\113\255\
\000\000\116\255\000\000\100\255\093\255\090\255\004\255\000\000\
\004\255\004\255\004\255\004\255\004\255\004\255\126\255\126\255\
\096\255\097\255\098\255\233\255\108\255\000\000\244\254\244\254\
\000\000\000\000\000\000\107\255\137\255\110\255\114\255\115\255\
\253\254\000\000\000\000\004\255\000\000\015\255\015\255\015\255\
\015\255\015\255\015\255\015\255\124\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\119\255\048\255\048\255\048\255\
\000\000\143\255\249\255\126\255\000\000\000\000\117\255\145\255\
\129\255\127\255\135\255\000\000\000\000\000\000\000\000\000\000\
\141\255\048\255\141\255\000\000\162\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\139\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\140\255\000\000\000\000\000\000\000\000\000\000\000\000\248\254\
\000\000\144\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\139\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\166\255\000\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\178\255\200\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\255\
\000\000\000\000\000\000\000\000\000\000\011\255\153\255\154\255\
\155\255\167\255\168\255\172\255\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\173\255\173\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\174\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\228\255\226\255\000\000\000\000\000\000\061\000\138\255\
\000\000\000\000\255\255\174\000\160\000\000\000"

let yytablesize = 287
let yytable = "\058\000\
\021\000\057\000\002\000\042\000\024\000\043\000\025\000\028\000\
\048\000\049\000\132\000\016\000\134\000\017\000\031\000\018\000\
\035\000\036\000\036\000\035\000\040\000\051\000\035\000\026\000\
\044\000\045\000\113\000\114\000\001\000\002\000\027\000\091\000\
\019\000\046\000\047\000\048\000\049\000\039\000\034\000\020\000\
\076\000\034\000\101\000\102\000\079\000\080\000\081\000\082\000\
\051\000\083\000\118\000\054\000\012\000\012\000\021\000\046\000\
\047\000\048\000\049\000\022\000\023\000\094\000\061\000\095\000\
\096\000\097\000\098\000\099\000\100\000\029\000\051\000\053\000\
\054\000\004\000\005\000\006\000\007\000\008\000\041\000\119\000\
\052\000\046\000\047\000\048\000\049\000\009\000\010\000\126\000\
\062\000\063\000\115\000\011\000\050\000\071\000\072\000\073\000\
\051\000\075\000\012\000\133\000\055\000\056\000\003\000\085\000\
\004\000\005\000\006\000\007\000\008\000\046\000\047\000\048\000\
\049\000\084\000\086\000\087\000\009\000\010\000\088\000\093\000\
\064\000\092\000\011\000\090\000\051\000\103\000\104\000\105\000\
\003\000\012\000\004\000\005\000\006\000\007\000\008\000\107\000\
\108\000\109\000\110\000\116\000\111\000\112\000\009\000\010\000\
\117\000\124\000\127\000\128\000\011\000\038\000\038\000\038\000\
\038\000\038\000\038\000\012\000\129\000\130\000\038\000\038\000\
\038\000\038\000\131\000\011\000\135\000\038\000\032\000\038\000\
\004\000\038\000\038\000\033\000\123\000\038\000\065\000\066\000\
\067\000\068\000\069\000\070\000\048\000\049\000\050\000\046\000\
\047\000\048\000\049\000\041\000\041\000\041\000\041\000\041\000\
\041\000\038\000\051\000\052\000\041\000\041\000\051\000\053\000\
\013\000\014\000\077\000\041\000\000\000\041\000\000\000\041\000\
\041\000\042\000\042\000\042\000\042\000\042\000\042\000\000\000\
\000\000\000\000\042\000\042\000\000\000\000\000\000\000\000\000\
\000\000\042\000\000\000\042\000\000\000\042\000\042\000\046\000\
\047\000\048\000\049\000\046\000\047\000\048\000\049\000\000\000\
\074\000\000\000\000\000\000\000\078\000\000\000\051\000\000\000\
\000\000\000\000\051\000\046\000\047\000\048\000\049\000\000\000\
\000\000\000\000\106\000\021\000\000\000\021\000\021\000\021\000\
\021\000\021\000\051\000\046\000\047\000\048\000\049\000\000\000\
\000\000\021\000\021\000\000\000\000\000\000\000\125\000\021\000\
\021\000\000\000\051\000\000\000\000\000\000\000\021\000"

let yycheck = "\030\000\
\000\000\030\000\003\001\025\001\001\001\027\001\003\001\009\000\
\021\001\022\001\129\000\025\001\131\000\027\001\016\000\029\001\
\018\000\019\000\020\000\028\001\022\000\034\001\031\001\020\001\
\026\000\027\000\030\001\031\001\001\000\030\001\027\001\060\000\
\027\001\019\001\020\001\021\001\022\001\002\001\028\001\027\001\
\042\000\031\001\071\000\072\000\046\000\047\000\048\000\049\000\
\034\001\051\000\003\001\004\001\030\001\031\001\027\001\019\001\
\020\001\021\001\022\001\027\001\027\001\063\000\026\001\065\000\
\066\000\067\000\068\000\069\000\070\000\027\001\034\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\003\001\110\000\
\003\001\019\001\020\001\021\001\022\001\017\001\018\001\116\000\
\028\001\031\001\092\000\023\001\030\001\028\001\028\001\028\001\
\034\001\028\001\030\001\130\000\032\001\033\001\003\001\025\001\
\005\001\006\001\007\001\008\001\009\001\019\001\020\001\021\001\
\022\001\028\001\003\001\003\001\017\001\018\001\003\001\030\001\
\030\001\029\001\023\001\024\001\034\001\030\001\030\001\030\001\
\003\001\030\001\005\001\006\001\007\001\008\001\009\001\028\001\
\030\001\001\001\029\001\016\001\027\001\027\001\017\001\018\001\
\026\001\003\001\030\001\003\001\023\001\010\001\011\001\012\001\
\013\001\014\001\015\001\030\001\028\001\031\001\019\001\020\001\
\021\001\022\001\028\001\023\001\003\001\026\001\028\001\028\001\
\003\001\030\001\031\001\028\001\112\000\034\001\010\001\011\001\
\012\001\013\001\014\001\015\001\028\001\028\001\028\001\019\001\
\020\001\021\001\022\001\010\001\011\001\012\001\013\001\014\001\
\015\001\020\000\028\001\028\001\019\001\020\001\034\001\028\001\
\028\001\028\001\043\000\026\001\255\255\028\001\255\255\030\001\
\031\001\010\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\019\001\020\001\255\255\255\255\255\255\255\255\
\255\255\026\001\255\255\028\001\255\255\030\001\031\001\019\001\
\020\001\021\001\022\001\019\001\020\001\021\001\022\001\255\255\
\028\001\255\255\255\255\255\255\028\001\255\255\034\001\255\255\
\255\255\255\255\034\001\019\001\020\001\021\001\022\001\255\255\
\255\255\255\255\026\001\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\034\001\019\001\020\001\021\001\022\001\255\255\
\255\255\017\001\018\001\255\255\255\255\255\255\030\001\023\001\
\024\001\255\255\034\001\255\255\255\255\255\255\030\001"

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
# 26 "parser.mly"
             (  _1  )
# 291 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
           ( IntTyp )
# 297 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 30 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 304 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 31 "parser.mly"
               ( NameTyp _1 )
# 311 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 34 "parser.mly"
                ( _1@_2 )
# 319 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                ( [] )
# 325 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 38 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 333 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 39 "parser.mly"
                              ( [TypeDec (_2,_4)] )
# 341 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 40 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 351 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 41 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 360 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                       ( _1@[_3] )
# 368 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                       ( [_1]  )
# 375 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                        ( [] )
# 381 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 49 "parser.mly"
                        ( _1 )
# 388 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                             ( _1@[(_3,_4)] )
# 397 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                             ( [(_1,_2)] )
# 405 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 56 "parser.mly"
                   ( _1@[_2] )
# 413 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 57 "parser.mly"
                   ( [_1] )
# 420 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                              ( Assign (Var _1, _3) )
# 428 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 437 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                              ( If (_3, _5, None) )
# 445 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 64 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 454 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "parser.mly"
                              ( While (_3, _5) )
# 462 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 66 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 469 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 476 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 68 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 483 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 69 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 490 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 70 "parser.mly"
                                ( CallProc (_1, _3) )
# 498 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 505 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 72 "parser.mly"
             ( _1 )
# 512 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
            ( NilStmt )
# 518 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                           ( [] )
# 524 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 77 "parser.mly"
                           ( _1 )
# 531 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                          ( _1@[_3] )
# 539 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                           ( [_1] )
# 546 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 84 "parser.mly"
                         ( Block (_2, _3) )
# 554 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 87 "parser.mly"
           ( IntExp _1  )
# 561 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
          ( VarExp (Var _1) )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 89 "parser.mly"
                          ( CallFunc (_1, _3) )
# 576 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 584 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 592 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 600 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 608 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 616 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                     ( CallFunc ("%", [_1; _3]) )
# 624 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 631 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                   ( _2 )
# 638 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 646 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 654 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 662 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 670 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 678 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 686 "parser.ml"
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
