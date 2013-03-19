type token =
  | INT of (int)
  | FLOAT of (float)
  | NAME of (string)
  | OP_EQ
  | OP_MULT
  | OP_DIV
  | OP_PLUS
  | OP_MINUS
  | LPAREN
  | RPAREN
  | AMPERSAND
  | ARROW
  | COLON
  | COMMA
  | SEMICOLON
  | CT_COSTS
  | CT_STRUCTURE
  | EOF

open Parsing;;
# 11 "InspeqtorParser.mly"

(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open AbstractStructure

(****************************************************************)
(** Functions *)
(****************************************************************)

(* Convert a float to a Num.num *)
let num_of_float f =
	(* Split the float in integer and fractional part *)
	let (fractional, integer) = modf f in
	let integer = int_of_float integer in
	(* Case of an integer *)
	if fractional = 0.0 then Num.num_of_int integer
	else(
		let fractional = string_of_float fractional in
		(* Remove the "0." in front of the fractional part *)
		let fractional = String.sub fractional 2 (String.length fractional -2) in
		(* Find the denominator *)
		let denominator = int_of_string ("1" ^ (String.make (String.length fractional) '0')) in
		let fractional = int_of_string fractional in
		(* Create the fraction *)
		Num.div_num (Num.num_of_int (integer * denominator + fractional)) (Num.num_of_int (denominator))
	)

# 53 "InspeqtorParser.ml"
let yytransl_const = [|
  260 (* OP_EQ *);
  261 (* OP_MULT *);
  262 (* OP_DIV *);
  263 (* OP_PLUS *);
  264 (* OP_MINUS *);
  265 (* LPAREN *);
  266 (* RPAREN *);
  267 (* AMPERSAND *);
  268 (* ARROW *);
  269 (* COLON *);
  270 (* COMMA *);
  271 (* SEMICOLON *);
  272 (* CT_COSTS *);
  273 (* CT_STRUCTURE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\004\000\004\000\005\000\006\000\006\000\007\000\
\008\000\008\000\009\000\010\000\010\000\010\000\011\000\011\000\
\011\000\012\000\012\000\013\000\013\000\003\000\014\000\014\000\
\017\000\017\000\015\000\015\000\016\000\000\000"

let yylen = "\002\000\
\003\000\002\000\002\000\000\000\003\000\002\000\000\000\006\000\
\001\000\001\000\001\000\003\000\003\000\001\000\003\000\003\000\
\001\000\001\000\002\000\001\000\001\000\002\000\003\000\000\000\
\003\000\000\000\001\000\000\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\030\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\003\000\027\000\022\000\000\000\001\000\
\000\000\005\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\023\000\020\000\021\000\010\000\000\000\000\000\009\000\
\000\000\000\000\017\000\018\000\029\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\000\000\
\015\000\016\000\008\000"

let yydgoto = "\002\000\
\004\000\005\000\010\000\007\000\008\000\018\000\019\000\031\000\
\032\000\033\000\034\000\035\000\036\000\014\000\015\000\021\000\
\026\000"

let yysindex = "\255\255\
\249\254\000\000\009\255\000\000\255\254\008\255\000\000\009\255\
\019\255\027\000\020\255\000\000\000\000\000\000\028\255\000\000\
\024\255\000\000\020\255\030\255\025\255\006\255\000\000\016\255\
\028\255\000\000\000\000\000\000\000\000\003\255\027\255\000\000\
\012\255\017\255\000\000\000\000\000\000\025\255\000\000\032\255\
\016\255\016\255\016\255\016\255\000\000\023\255\017\255\017\255\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\026\255\000\000\000\000\000\000\000\000\026\255\
\002\000\000\000\000\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\255\000\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\001\000\000\000\000\000\000\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\011\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\032\000\000\000\022\000\000\000\000\000\
\019\000\000\000\240\255\241\255\014\000\000\000\000\000\020\000\
\008\000"

let yytablesize = 280
let yytable = "\001\000\
\014\000\024\000\007\000\027\000\028\000\012\000\027\000\028\000\
\029\000\003\000\013\000\006\000\011\000\030\000\009\000\007\000\
\027\000\028\000\041\000\042\000\011\000\043\000\044\000\030\000\
\047\000\048\000\016\000\049\000\050\000\013\000\020\000\017\000\
\022\000\024\000\046\000\025\000\040\000\051\000\026\000\012\000\
\023\000\004\000\037\000\039\000\038\000\045\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\000\000\014\000\
\014\000\000\000\014\000\014\000\012\000\012\000\000\000\012\000\
\012\000\013\000\013\000\000\000\013\000\013\000\011\000\011\000"

let yycheck = "\001\000\
\000\000\000\000\003\001\001\001\002\001\000\000\001\001\002\001\
\003\001\017\001\000\000\003\001\000\000\008\001\016\001\016\001\
\001\001\002\001\007\001\008\001\013\001\005\001\006\001\008\001\
\041\000\042\000\000\000\043\000\044\000\011\001\003\001\012\001\
\009\001\004\001\003\001\011\001\010\001\015\001\000\000\008\000\
\019\000\016\001\024\000\030\000\025\000\038\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\003\001\255\255\255\255\007\001\
\008\001\255\255\010\001\011\001\007\001\008\001\255\255\010\001\
\011\001\007\001\008\001\255\255\010\001\011\001\010\001\011\001"

let yynames_const = "\
  OP_EQ\000\
  OP_MULT\000\
  OP_DIV\000\
  OP_PLUS\000\
  OP_MINUS\000\
  LPAREN\000\
  RPAREN\000\
  AMPERSAND\000\
  ARROW\000\
  COLON\000\
  COMMA\000\
  SEMICOLON\000\
  CT_COSTS\000\
  CT_STRUCTURE\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'structure) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'costs) in
    Obj.repr(
# 65 "InspeqtorParser.mly"
 (
		make_abstract_program (_1, _2)
	)
# 236 "InspeqtorParser.ml"
               : AbstractStructure.abstract_program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_list) in
    Obj.repr(
# 77 "InspeqtorParser.mly"
                         (_2)
# 243 "InspeqtorParser.ml"
               : 'structure))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'state) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_list) in
    Obj.repr(
# 83 "InspeqtorParser.mly"
                  ( _1 :: _2 )
# 251 "InspeqtorParser.ml"
               : 'state_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "InspeqtorParser.mly"
   ( [] )
# 257 "InspeqtorParser.ml"
               : 'state_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'transitions) in
    Obj.repr(
# 90 "InspeqtorParser.mly"
                        (
		(_1, _3)
	)
# 267 "InspeqtorParser.ml"
               : 'state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'transition) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transitions) in
    Obj.repr(
# 98 "InspeqtorParser.mly"
                          ( _1 :: _2 )
# 275 "InspeqtorParser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "InspeqtorParser.mly"
   ( [] )
# 281 "InspeqtorParser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'cost) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 105 "InspeqtorParser.mly"
                                         ( (_3, _5) )
# 289 "InspeqtorParser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arithm_exp) in
    Obj.repr(
# 112 "InspeqtorParser.mly"
              ( TmpConstCost _1 )
# 296 "InspeqtorParser.ml"
               : 'cost))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "InspeqtorParser.mly"
        ( TmpParametricCost _1 )
# 303 "InspeqtorParser.ml"
               : 'cost))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_exp) in
    Obj.repr(
# 118 "InspeqtorParser.mly"
         (_1)
# 310 "InspeqtorParser.ml"
               : 'arithm_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 123 "InspeqtorParser.mly"
                            (Num.add_num _1 _3)
# 318 "InspeqtorParser.ml"
               : 'add_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 124 "InspeqtorParser.mly"
                             (Num.sub_num _1 _3)
# 326 "InspeqtorParser.ml"
               : 'add_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_exp) in
    Obj.repr(
# 125 "InspeqtorParser.mly"
            (_1)
# 333 "InspeqtorParser.ml"
               : 'add_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'number) in
    Obj.repr(
# 130 "InspeqtorParser.mly"
                           (Num.mult_num _1 _3)
# 341 "InspeqtorParser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'number) in
    Obj.repr(
# 131 "InspeqtorParser.mly"
                          (Num.div_num _1 _3)
# 349 "InspeqtorParser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'number) in
    Obj.repr(
# 132 "InspeqtorParser.mly"
          (_1)
# 356 "InspeqtorParser.ml"
               : 'mult_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pos_number) in
    Obj.repr(
# 138 "InspeqtorParser.mly"
              (_1)
# 363 "InspeqtorParser.ml"
               : 'number))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pos_number) in
    Obj.repr(
# 139 "InspeqtorParser.mly"
                       (Num.minus_num _2)
# 370 "InspeqtorParser.ml"
               : 'number))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 145 "InspeqtorParser.mly"
       ( Num.num_of_int _1 )
# 377 "InspeqtorParser.ml"
               : 'pos_number))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 147 "InspeqtorParser.mly"
         ( (num_of_float _1))
# 384 "InspeqtorParser.ml"
               : 'pos_number))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cost_list) in
    Obj.repr(
# 155 "InspeqtorParser.mly"
                    ( _2 )
# 391 "InspeqtorParser.ml"
               : 'costs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_opt) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cost_def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cost_list2) in
    Obj.repr(
# 160 "InspeqtorParser.mly"
                               (_2 :: _3)
# 400 "InspeqtorParser.ml"
               : 'cost_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "InspeqtorParser.mly"
   ( [] )
# 406 "InspeqtorParser.ml"
               : 'cost_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cost_def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cost_list2) in
    Obj.repr(
# 166 "InspeqtorParser.mly"
                                 (_2 :: _3)
# 414 "InspeqtorParser.ml"
               : 'cost_list2))
; (fun __caml_parser_env ->
    Obj.repr(
# 167 "InspeqtorParser.mly"
   ( [] )
# 420 "InspeqtorParser.ml"
               : 'cost_list2))
; (fun __caml_parser_env ->
    Obj.repr(
# 172 "InspeqtorParser.mly"
             ()
# 426 "InspeqtorParser.ml"
               : 'and_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 173 "InspeqtorParser.mly"
   ()
# 432 "InspeqtorParser.ml"
               : 'and_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithm_exp) in
    Obj.repr(
# 178 "InspeqtorParser.mly"
                       ( (_1, _3) )
# 440 "InspeqtorParser.ml"
               : 'cost_def))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : AbstractStructure.abstract_program)
