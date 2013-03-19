/***********************************************
   Laboratoire Specification et Verification

   Etienne ANDRE

   Created       : 2009/04/27
   Last modified : 2009/04/28
***********************************************/

%{

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

%}

%token <int> INT
%token <float> FLOAT
%token <string> NAME

%token OP_EQ
%token OP_MULT OP_DIV OP_PLUS OP_MINUS

%token LPAREN RPAREN
%token AMPERSAND ARROW COLON COMMA SEMICOLON

%token CT_COSTS CT_STRUCTURE

%token EOF

%start program             /* the entry point */
%type <AbstractStructure.abstract_program> program
%%



/**********************************************/
program:
	 structure costs EOF
	{
		make_abstract_program ($1, $2)
	}
;

/***********************************************
  STRUCTURE
***********************************************/

/**********************************************/

structure:
	CT_STRUCTURE state_list {$2}
;

/**********************************************/

state_list:
	state state_list { $1 :: $2 }
	| { [] }
;

/**********************************************/

state:
	NAME COLON transitions {
		($1, $3)
	}
;

/**********************************************/

transitions:
	| transition transitions { $1 :: $2 }
	| { [] }
;

/**********************************************/

transition:
	ARROW LPAREN cost RPAREN NAME SEMICOLON { ($3, $5) }
;


/**********************************************/

cost:
	| arithm_exp { TmpConstCost $1 }
	| NAME { TmpParametricCost $1 }
;

/**********************************************/
arithm_exp:
	add_exp {$1}
;

/**********************************************/
add_exp:
	| add_exp OP_PLUS mult_exp {Num.add_num $1 $3}
	| add_exp OP_MINUS mult_exp {Num.sub_num $1 $3}
	| mult_exp {$1}
;

/**********************************************/
mult_exp:
	| mult_exp OP_MULT number {Num.mult_num $1 $3}
	| mult_exp OP_DIV number {Num.div_num $1 $3}
	| number {$1}
;

/**********************************************/

number:
	| pos_number {$1}
	| OP_MINUS pos_number {Num.minus_num $2}
;

/**********************************************/

pos_number:
	| INT { Num.num_of_int $1 }
	
	| FLOAT { (num_of_float $1)}
;


/***********************************************
  COSTS
***********************************************/
costs:
	CT_COSTS cost_list { $2 }
;

/**********************************************/
cost_list:
	| and_opt cost_def cost_list2 {$2 :: $3}
	| { [] }
;

/**********************************************/
cost_list2:
	| AMPERSAND cost_def cost_list2 {$2 :: $3}
	| { [] }
;

/**********************************************/
and_opt:
	| AMPERSAND {}
	| {}
;

/**********************************************/
cost_def:
	NAME OP_EQ arithm_exp { ($1, $3) }
;
