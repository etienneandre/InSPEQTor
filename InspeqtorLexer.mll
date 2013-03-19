(***********************************************
   Laboratoire Specification et Verification

   Etienne ANDRE

   Created       : 2009/04/27
   Last modified : 2009/04/28
***********************************************)

{
open InspeqtorParser
}
rule token = parse
	[' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
	| "(*"([^'*']|'*'+[^'*'')'])*'*'+')' { token lexbuf }
	
	| "costs"          { CT_COSTS }
	| "structure"      { CT_STRUCTURE }

	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as lxm { NAME lxm }
	| ['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT(float_of_string lxm) }
	| ['0'-'9']+ as lxm { INT(int_of_string lxm) }

	| '='              { OP_EQ }
	
	| '*'              { OP_MULT }
	| '/'              { OP_DIV }
	| '+'              { OP_PLUS }
	| '-'              { OP_MINUS }
	

	| '('              { LPAREN }
	| ')'              { RPAREN }

	| '&'              { AMPERSAND }
	| "->"             { ARROW }
	| ':'              { COLON }
	| ','              { COMMA }
	| ';'              { SEMICOLON }

	| eof              { EOF}


