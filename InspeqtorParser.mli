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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbstractStructure.abstract_program
