(*****************************************************************
 *
 *                     Floydator
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
(** Modules *)
(****************************************************************)
open Global
open Num


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
(** Functions to get the variable names from the parsing structure *)
(****************************************************************)

(* Compute the list of (possible identical) state names *)
let get_state_names parsing_structure =
	(* Get state name in a state*)
	let get_state_names state_names (state_name, _) =
		state_name :: state_names
	in
	(* Reverse list *)
	List.rev (List.fold_left get_state_names [] parsing_structure)


(* Compute the list of (all different) parametric cost variables *)
let get_cost_parameter_names parsing_structure =
	(* Get cost parameters in a state *)
	let get_cost_parameters list_of_names (_, list_of_transitions) =
		(* Get cost parameter in a transition *)
		let get_cost_parameter list_of_names (cost_name, _) =
			let extended_list_of_names =
			match cost_name with
				| TmpConstCost _ -> list_of_names
				| TmpParametricCost p -> if List.mem p list_of_names then list_of_names else p :: list_of_names
			in extended_list_of_names
		in
		List.fold_left get_cost_parameter list_of_names list_of_transitions
	in
	(* Reverse list to keep the right order *)
	List.rev (List.fold_left get_cost_parameters [] parsing_structure)



(****************************************************************)
(** Verification functions *)
(****************************************************************)
exception AlreadyFound

(* Check that all the elements of a list are different *)
let all_different l =
	let res =
	try(
	let _ = List.fold_left
		(fun list_of_elements elem ->
			if List.mem elem list_of_elements then raise AlreadyFound;
			elem :: list_of_elements)
		[]
		l
	in true
	) with AlreadyFound -> false
	in
	res


(* Check that all the state names used in the parsing structure have been defined ; if not, print error messages *)
let check_existence_of_state_names parsing_structure defined_state_names =
	(* Get all destination state names in a state *)
	let get_dest_states list_of_dest_states (_, list_of_transitions) =
		(* Get all destination state names in prob_and_name *)
		let get_dest_states list_of_dest_states (_, dest_state) =
			if List.mem dest_state list_of_dest_states then list_of_dest_states
			else dest_state :: list_of_dest_states
		in List.fold_left get_dest_states list_of_dest_states list_of_transitions
	in
	let dest_state_names = List.fold_left get_dest_states [] parsing_structure in
	(* Check that all states are defined *)
	let is_state_defined result state_name =
		if List.mem state_name defined_state_names then result
		else (print_debug_message mode_STANDARD mode_ERROR ("State name " ^ state_name ^ " is used but was not defined."); false)
	in List.fold_left is_state_defined true dest_state_names


(* Check that all parameters in the program are given a value (and warns if the cost parameteres in C0 are not all different) *)
let check_cost_parameters cost_parameter_names costs_definition =
	(* Check that all the defined cost parameters are different (if not, only warns) *)
	let distinct_names = List.fold_left
		(fun list_of_names (cost_name, _) ->
			if List.mem cost_name list_of_names then(
				print_warning ("The cost parameter " ^ cost_name ^ " is defined twice."); list_of_names
			) else cost_name :: list_of_names )
		[]
		costs_definition
	in
	(* Check that all the parameters of the program are given a value (if not, return false) *)
	List.fold_left
		(fun all_ok cost_name ->
			if not (List.mem cost_name distinct_names) then(
				print_debug_message mode_STANDARD mode_ERROR ("The cost parameter " ^ cost_name ^ " is used but is not given an instantiated value.");
				false
			) else all_ok
		)
		true
		cost_parameter_names


(* Check that all the defined cost parameters represent a parameter of the program (if not, only warns) *)
let warn_if_unused_cost_parameters list_of_cost_names costs_definition =
	List.iter
	(fun (cost_name, _) ->
		if not (List.mem cost_name list_of_cost_names)
		then print_warning ("The cost parameter " ^ cost_name ^ " does not appear in the program.")
	)
	costs_definition



(****************************************************************)
(** Creation functions *)
(****************************************************************)

(* Create the transition matrices and the costs array *)
let make_matrices parsing_structure index_of_states index_of_cost_parameters cost_values =
	(* Numbers *)
	let nb_states = Hashtbl.length index_of_states in
	
	(* Create the instantiated matrix instantiated with Infinity *)
	let instantiated_costs_matrix = Array.make_matrix nb_states nb_states InstInfinity in
	(* Create the parametric matrix instantiated with Infinity *)
	let costs_matrix = Array.make_matrix nb_states nb_states Infinity in
	
	(* Loop on states *)
	List.iter (fun (orig_state, list_of_transitions) ->
		(* Find the index of the orig_state *)
		let orig_state_index = Hashtbl.find index_of_states orig_state in
		(* Loop on transitions *)
		List.iter (fun (cost, dest_state) ->
			(* Find the index of the dest_state *)
			let dest_state_index = Hashtbl.find index_of_states dest_state in
			(* Consider the cost *)
			match cost with
				(* Constant cost *)
				| TmpConstCost c ->
					(* Update the two matrices *)
					instantiated_costs_matrix.(orig_state_index).(dest_state_index) <- ConstInstCost c;
					costs_matrix.(orig_state_index).(dest_state_index) <- ConstCost c;
				(* Parametric cost *)
				| TmpParametricCost cost_name ->
					(* Find its index *)
					let cost_index = Hashtbl.find index_of_cost_parameters cost_name in
					(* Find its instantiated value *)
					let cost_value = cost_values.(cost_index) in
					(* Update the two matrices *)
					instantiated_costs_matrix.(orig_state_index).(dest_state_index) <- ConstInstCost cost_value;
					costs_matrix.(orig_state_index).(dest_state_index) <- ParametricCost cost_index;
		) list_of_transitions;
	) parsing_structure;
	(* Return the structures *)
	(instantiated_costs_matrix, costs_matrix)



(****************************************************************)
(** Main conversion function *)
(****************************************************************)

(* Convert the result of the parsing into a valid program ; raise InvalidProgram if invalid program*)
let make_abstract_program (parsing_structure, costs_definition) =
	(* First check that the program is non empty *)
	if parsing_structure = [] then (
		print_debug_message mode_STANDARD mode_ERROR  ("The program seems to be empty.");
		raise InvalidProgram);
	
	(* Get the defined state names *)
	let list_of_state_names = get_state_names parsing_structure in
	(* Get all the (different) cost parameter names *)
	let list_of_cost_names = get_cost_parameter_names parsing_structure in
	
	(* Verify that all the defined state names are different *)
	let all_different = all_different list_of_state_names in
	(* Check that all destination state names are defined *)
	let all_states_defined = check_existence_of_state_names parsing_structure list_of_state_names in
	
	(* Check that all parameters in the program are given a value (and warns if the cost parameteres in C0 are not all different) *)
	let costs_ok = check_cost_parameters list_of_cost_names costs_definition in
	(* Warn if some parameters are not used *)
	warn_if_unused_cost_parameters list_of_cost_names costs_definition;
	
	(* Perform intersection and may raise exception *)
	if not (all_different && all_states_defined && costs_ok) then raise InvalidProgram;
	
	(* Numbers *)
	let nb_states = List.length list_of_state_names in
	let nb_cost_parameters = List.length list_of_cost_names in
	
	(* The array of state names ; index -> state name *)
	let states = Array.of_list list_of_state_names in
	(* A (constant) hash table state name -> index *)
	let index_of_states = Hashtbl.create nb_states in
	for i = 0 to nb_states - 1 do
		Hashtbl.add index_of_states states.(i) i;
	done;
	
	(* The array of all cost parameter names : index -> parameter name *)
	let cost_parameters = Array.of_list list_of_cost_names in
	(* A (constant) hash table parameter name -> cost_index *)
	let index_of_cost_parameters = Hashtbl.create nb_cost_parameters in
	for cost_index = 0 to nb_cost_parameters - 1 do
		Hashtbl.add index_of_cost_parameters cost_parameters.(cost_index) cost_index;
	done;
	
	(* The array of cost parameter values : index -> cost instantiated value *)
	let cost_values = Array.make nb_cost_parameters (Num.num_of_int 0) in
	List.iter (fun (cost_name, cost_value) ->
		(* Only consider the cost parameters used in the program *)
		if List.mem cost_name list_of_cost_names then(
			let cost_index = Hashtbl.find index_of_cost_parameters cost_name in
			cost_values.(cost_index) <- cost_value
		)
	) costs_definition;

	(* Create the matrices *)
	let instantiated_costs_matrix, costs_matrix
		= make_matrices parsing_structure index_of_states index_of_cost_parameters cost_values in

	(* Make and return the structure *)
	{
	(* Number of states *)
	nb_states = nb_states;
	
	(* Number of cost parameters *)
	nb_cost_parameters = nb_cost_parameters;
	
	(* Array of states indexed by the state_index *)
	states = states;
	
	(* Array of cost names indexed by the cost index *)
	cost_parameters = cost_parameters;
	
	(* Array of cost values indexed by the cost index *)
	cost_values = cost_values;
	
	(* Matrix : state * state -> cost *)
	instantiated_costs_matrix = instantiated_costs_matrix;
	
	(* Matrix : state * state -> cost *)
	costs_matrix = costs_matrix;
	}
