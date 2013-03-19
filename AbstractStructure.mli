(*****************************************************************
 *
 *                     INSPEQTOR
 * 
 * Compiler from INSPEQTOR file to OCaml
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/04/27
 * Last modified: 2009/04/27
 *
 ****************************************************************)



(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InvalidProgram



(****************************************************************)
(** Variables and names *)
(****************************************************************)

(** Name of an identifier *)
type name = string

(* Index of a state *)
type state_index = int

(* Cost of a state : we allow both rational constants and parameters *)
type tmp_cost =
	| TmpConstCost of Num.num
	| TmpParametricCost of string

(* Cost of a state : we allow both rational constants and parameters *)
type cost =
	| ConstCost of Num.num
	| ParametricCost of int (* index of the cost parameter *)
	| Infinity

type instantiated_cost =
	| ConstInstCost of Num.num
	| InstInfinity


(****************************************************************)
(** Costs definition *)
(****************************************************************)
type costs_definition = (name * Num.num) list


(****************************************************************)
(** Parsing structure *)
(****************************************************************)
(* Parsing structure *)
type parsing_structure = (name * (tmp_cost * name ) list) list


(****************************************************************)
(** Abstract program *)
(****************************************************************)
type abstract_program = {
	(* Number of states *)
	nb_states : int;
	
	(* Number of cost parameters *)
	nb_cost_parameters : int;
	
	(* Array of states indexed by the state_index *)
	states : name array;
	
	(* Array of cost names indexed by the cost index *)
	cost_parameters : name array;
	
	(* Array of cost values indexed by the cost index *)
	cost_values : Num.num array;
	
	(* Matrix : state * state -> cost *)
	instantiated_costs_matrix : instantiated_cost array array;
	
	(* Matrix : state * state -> cost *)
	costs_matrix : cost array array;
}



(****************************************************************)
(** Creation function *)
(****************************************************************)
(* Convert the result of the parsing into a valid program ; raise VerificationFailure if invalid program*)
val make_abstract_program : parsing_structure * costs_definition -> abstract_program

