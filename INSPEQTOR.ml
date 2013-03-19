(*****************************************************************
 *
 *                     INSPEQTOR
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/04/27
 * Last modified: 2009/05/25
 *
 ****************************************************************)



(****************************************************************)
(** TO DO *)
(****************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open AbstractStructure
open Arg
open Num


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception BadInputGraph of string
exception NotFound


(****************************************************************)
(** Constants *)
(****************************************************************)
(* let debug_mode = mode_STANDARD *)
(* let debug_mode = mode_LOW_DEBUG *)
(* let debug_mode = mode_HIGH_DEBUG *)
let debug_mode = mode_TOTAL_DEBUG



(****************************************************************)
(** Useful functions *)
(****************************************************************)
(* Print a message in function of debug mode *)
let print_message message debug_level =
	print_debug_message debug_mode debug_level message


(* Convert a Num.num into a string, under the form of "exact value (approx value)" *)
let string_float_of_num num =
	(string_of_num num)
	^ (
		if Num.is_integer_num num then ""
		else (" ( = " ^ (string_of_float (Num.float_of_num num)) ^ ")")
	)


(* Get the first index of the array a satisfying predicate p, or raise NotFound if not found *)
let array_findi p a =
	(* Recursive function with index i *)
	let rec array_findi_rec p a i =
		(* Not found in an empty array *)
		if Array.length a = 0 then(
			(* Debug message *)
			print_message ("array_findi: No element found.") mode_TOTAL_DEBUG;
			raise NotFound;
		);
		(* Check the first element *)
		if p a.(0) then (
			(* Debug message *)
			print_message ("array_findi: The element of index " ^ (string_of_int i) ^ " verifies the predicate.") mode_TOTAL_DEBUG;
			(* Return the index *)
			i
		) else(
			(* Debug message *)
			print_message ("array_findi: The element of index " ^ (string_of_int i) ^ " does not verify the predicate.") mode_TOTAL_DEBUG;
			(* Recursive call *)
			array_findi_rec p (Array.sub a 1 (Array.length a - 1)) (i + 1)
		)
	(* Initial call *)
	in array_findi_rec p a 0
	


(****************************************************************)
(** Functions to work with infinity *)
(****************************************************************)
(* Compute the min of 2 instantiated_cost *)
(*
let min i j =
	match (i, j) with
	| InstInfinity, j -> j
	| i, InstInfinity -> i
	| ConstInstCost i, ConstInstCost j ->
		ConstInstCost (Num.min_num i j)
*)

(* Check if a instantiated_cost1 < instantiated_cost2 *)
let is_strictly_smaller c1 c2 =
	match (c1, c2) with
	| ConstInstCost _, InstInfinity -> true
	| ConstInstCost c1, ConstInstCost c2 -> c1 </ c2
	| _ -> false


(* Compute the sum of 2 instantiated_cost *)
let plus i j =
	match (i, j) with
	| InstInfinity, _ -> InstInfinity
	| _, InstInfinity -> InstInfinity
	| ConstInstCost i, ConstInstCost j ->
		ConstInstCost (Num.add_num i j)


(****************************************************************)
(** The Value Determiantion Function for the parametric Maxplus Algorithm *)
(****************************************************************)

(* Find a circuit and return a list of the (distinct) nodes involved in the cycle *)
let find_circuit a states =
	(* Number of states *)
	let nb_states = Array.length a in
	(* Function of successor *)
	let is_succ = function
		| ConstInstCost _ -> true
		| InstInfinity -> false
	in
	(* Find a successor for state_index, and raise exception BadInputGraph if not found *)
	let find_successor a state_index =
		try(
			array_findi is_succ a.(state_index)
		)with
		NotFound -> raise (BadInputGraph ("No successor to " ^ states.(state_index) ^ " could be found."));
	in
	(* Visited states : length max of nb_states *)
	let visited_states = Array.make nb_states (-1) in
	let nb_visited = ref 0 in
	let circuit = ref [] in
	(* Start arbitrarily with state 0 *)
	let current_state = ref 0 in
	(* Loop until a circuit is found (in nb_states iterations in the worst case) *)
	let circuit_found = ref false in
	print_message ("Looking for a circuit in a matrix of " ^ (string_of_int nb_states) ^ " states.") mode_HIGH_DEBUG;
	while not !circuit_found do
		(* Debug message *)
		print_message ("State considered at iteration " ^ (string_of_int !nb_visited)
			^ " : " ^ (string_of_int !current_state) ^ " ('" ^ (states.(!current_state)) ^ "')"
		) mode_HIGH_DEBUG;

		(* Try to find the current_state in the array of visited states *)
		try(
			(* Look for the previous index where the current state has been visited *)
			let previous_index = array_findi (fun i -> i = !current_state) visited_states in
			
			(* Keep only the sub list of the array *)
			circuit := Array.to_list (Array.sub visited_states previous_index (!nb_visited - previous_index));
			
			(* Debug message *)
			print_message ("The state '" ^ (states.(!current_state)) ^ "' has already been visited: circuit found.") mode_HIGH_DEBUG;
			
			(* Break *)
			circuit_found := true;
		)
		with NotFound -> (
			(* Update the array of visited states *)
			visited_states.(!nb_visited) <- !current_state;
			
			(* Update the number of visited states *)
			nb_visited := !nb_visited + 1;
			
			(* Find the successor and keep looking for the circuit *)
			current_state := find_successor a !current_state;
		);
	done;
	(* Return the real circuit as a list *)
	!circuit
	

let value_determination a states =
	(* Find a circuit *)
	let circuit = find_circuit a states in
	
	(* Debug message *)
	let string_of_circuit =
		(string_of_list_of_string (List.map (fun state_index -> states.(state_index) ^ " -> ") circuit))
		^ states.(List.nth circuit 0)
	in
	print_message ("Circuit considered: " ^ string_of_circuit) mode_MEDIUM_DEBUG;
	
	(* Compute the instantiated cost of a circuit (list of nodes) *)
	let first_element = List.hd circuit in
	let rec cost_of_circuit = function
		(* Single node in the circuit *)
		| last :: [] -> let cost = a.(last).(first_element) in(
			match cost with
			| ConstInstCost c -> c
			| InstInfinity -> raise (InternalError "value_determination : a cost can't be infinite in a circuit.")
			)
		(* At least 2 nodes in the circuit *)
		| first :: (second :: rest) -> let cost = a.(first).(second) in(
			match cost with
			| ConstInstCost c -> c +/ (cost_of_circuit (second :: rest))
			| InstInfinity -> raise (InternalError "value_determination : a cost can't be infinite in a circuit.")
			)
		(* This loop stops before the list becomes empty *)
		| [] -> raise (InternalError "value_determination : the list 'circuit' should never be empty")
	in
	let cost = cost_of_circuit circuit in
	(* Debug message *)
	print_message ("Cost of this circuit: " ^ (Num.string_of_num cost) ^ "") mode_HIGH_DEBUG;
	
	let eta = cost // (Num.num_of_int (List.length circuit)) in
	(* Debug message *)
	print_message ("eta = " ^ (Num.string_of_num eta) ^ "") mode_HIGH_DEBUG;



(****************************************************************)
(****************************************************************)
(** Hello world! *)
(****************************************************************)
(****************************************************************)
;;
print_message ("***********************************************") mode_STANDARD;
print_message ("*                 INSPEQTOR                   *") mode_STANDARD;
print_message ("*                              Etienne ANDRE  *") mode_STANDARD;
print_message ("*                                       2009  *") mode_STANDARD;
print_message ("*  Laboratoire Specification et Verification  *") mode_STANDARD;
print_message ("*               ENS de Cachan & CNRS, France  *") mode_STANDARD;
print_message ("***********************************************") mode_STANDARD;


(****************************************************************)
(** Get the arguments *)
(****************************************************************)
(* Name of the file *)
let file = ref "" in

(* Check if the name of the file is passed as an argument *)
let argument_found = ref false in

(* If true: maxplus algorithm; if false: Floyd-Warshall *)
let maxplus = ref false in

(* No options *)
let speclist = [("-maxplus", Set maxplus, "runs the maxplus algorithm instead of Floyd-Warshall")] in
(* A function on any argument *)
let anon_fun = (fun arg ->
	(* If more than one argument : warns *)
	if !argument_found then(
		print_warning ("The program argument '" ^ arg ^ "' will be ignored.");
	)else(
		print_message ("Considering file " ^ arg ^ ".") mode_LOW_DEBUG;
		argument_found := true;
		file := arg;
	)
) in
let usage_msg = "Usage: INSPEQTOR [file]" in

Arg.parse speclist anon_fun usage_msg;

(* If not argument is passed *)
if not !argument_found then(
	print_message ("Please give a file name.") mode_ERROR;
	Arg.usage speclist usage_msg;
	abort_program (); exit(0)
);

(* If maxplus : message *)
if !maxplus then(
	print_message ("Version of the algorithm: maxplus") mode_STANDARD;
)else(
	print_message ("Version of the algorithm: parametric Floyd-Warshall") mode_STANDARD;

);

(****************************************************************)
(** Parsing and printing *)
(****************************************************************)


(* Lexing *)
let lexbuf = try(
	Lexing.from_channel (open_in !file)
) with
	| Sys_error e -> print_message ("The file " ^ !file ^ " could not be opened.\n" ^ e) mode_ERROR; abort_program (); exit(0)
	| Failure f -> print_message ("Lexing error: " ^ f) mode_ERROR; abort_program (); exit(0)
in
(* Parsing *)
let abstract_program = try(
	InspeqtorParser.program InspeqtorLexer.token lexbuf
) with
	| Parsing.Parse_error -> print_message "Unknown parsing error. Check your program." mode_ERROR; abort_program (); exit(0)
	| ParsingError e -> print_message e mode_ERROR; print_message "Parsing error" mode_ERROR; abort_program (); exit(0)
	| InvalidProgram -> print_message "The input file contains errors. Please check it again." mode_ERROR; abort_program (); exit(0)
in


(****************************************************************)
(** Debug information *)
(****************************************************************)

(* Print parametric program *)
print_message "\nParametric program:" mode_MEDIUM_DEBUG;
print_message
	(InspeqtorPrinter.string_of_parametric_matrix
		abstract_program.states
		abstract_program.cost_parameters
		abstract_program.costs_matrix
	)
	mode_MEDIUM_DEBUG;


(* Print instantiated program *)
print_message "\nInstantiated program:" mode_MEDIUM_DEBUG;
print_message
	(InspeqtorPrinter.string_of_instantiated_matrix
		abstract_program.states
		abstract_program.instantiated_costs_matrix
	)
	mode_MEDIUM_DEBUG;


(****************************************************************)
(** Useful information *)
(****************************************************************)
let nb_states = abstract_program.nb_states in
let nb_cost_parameters = abstract_program.nb_cost_parameters in
let states = abstract_program.states in
let cost_parameters = abstract_program.cost_parameters in
let instantiated_costs_matrix = abstract_program.instantiated_costs_matrix in
let costs_matrix = abstract_program.costs_matrix  in




(****************************************************************)
(****************************************************************)
(** PARAMETRIC MAXPLUS *)
(****************************************************************)
(****************************************************************)
if !maxplus then(
	value_determination instantiated_costs_matrix states


(****************************************************************)
(****************************************************************)
(** PARAMETRIC FLOYD-WARSHALL *)
(****************************************************************)
(****************************************************************)
)else(

(****************************************************************)
(** Instantiations *)
(****************************************************************)

(* The instantiated shortest values from state i to state j : initially infinity *)
let shortest_matrix = Array.make_matrix nb_states nb_states InstInfinity in
(* Copy instantiated_costs_matrix *)
for i = 0 to nb_states-1 do
	for j = 0 to nb_states-1 do
		shortest_matrix.(i).(j) <- instantiated_costs_matrix.(i).(j);
	done;
done;
(* The path from i to i is 0 (case something else would have been defined in the program) *)
for i = 0 to nb_states-1 do
	shortest_matrix.(i).(i) <- ConstInstCost (Num.num_of_int 0);
done;


(* The member for every state * state *)
let members = Array.make_matrix nb_states nb_states (Array.make nb_cost_parameters (Num.num_of_int 0), Num.num_of_int 0) in
(* Initialized with the parametric cost *)
for i = 0 to nb_states-1 do
	for j = 0 to nb_states-1 do
		(* If i = j : 0 *)
		if i = j then(
		members.(i).(j) <- Array.make nb_cost_parameters (Num.num_of_int 0), Num.num_of_int 0;
		(* If i <> j *)
		) else (
		let coeff_array = Array.make nb_cost_parameters (Num.num_of_int 0) in
		let member =
			match costs_matrix.(i).(j) with
			(* Constant : return a constant member *)
			| ConstCost c -> coeff_array, c
			(* Parameter : return an updated member *)
			| ParametricCost cost_index ->
				coeff_array.(cost_index) <- Num.num_of_int 1;
				coeff_array, (Num.num_of_int 0)
			(* Infinity : return an empty member because it will never be used *)
			| Infinity -> coeff_array, (Num.num_of_int (-333))
		in
		(* Debug message *)
		print_message ("Initial member for (" ^ states.(i)
			^ ", "
			^ states.(j) ^ ") : "
			^ (Constraint.string_of_exact_member cost_parameters member)
			) mode_HIGH_DEBUG;
		(* Update the member *)
		members.(i).(j) <- member;
		)
	done;
done;

let the_constraint = ref [] in


(****************************************************************)
(** The Parametric Floyd-Warshall Algorithm *)
(****************************************************************)
for k = 0 to nb_states-1 do
	for i = 0 to nb_states-1 do
		for j = 0 to nb_states-1 do
			(* Strings for debug *)
			let si = (string_of_int (i+1)) in
			let sj = (string_of_int (j+1)) in
			let sk = (string_of_int (k+1)) in
		
			print_message ("Considering k = " ^ sk ^ ", "
				^ "i = " ^ (states.(i)) ^ ", "
				^ "j = " ^ (states.(j)) ^ ""
			) mode_HIGH_DEBUG;
			print_message ("(i.e., k = " ^ sk ^ ", "
				^ "i = " ^ si ^ ", "
				^ "j = " ^ sj ^ ")"
			) mode_TOTAL_DEBUG;
			
			let w_ij = shortest_matrix.(i).(j) in
			let w_ikkj = plus shortest_matrix.(i).(k) shortest_matrix.(k).(j) in
			(* Compare W(i,j) with W(i,k) + W(k,j) *)
			match w_ikkj, w_ij with
			
			(* INFINITY <> INFINITY : do nothing *)
			| InstInfinity, InstInfinity -> ()
			
			(* W(i,k) + W(k,j) = INFINITY > W(i,j) : do nothing *)
			| InstInfinity, ConstInstCost _ -> ()
			
			(* W(i,k) + W(k,j) < (W(i,j) = INFINITY) : update with no constraint *)
			| ConstInstCost v_ikkj, InstInfinity ->
				(* Compute the sum of members *)
				let sum = Constraint.add_members members.(i).(k) members.(k).(j) in
				(* Update the members *)
				members.(i).(j) <- sum;
				print_message ("...updating W(" ^ si ^ "," ^ sj ^ ") <- W(" ^ si ^ "," ^ sk ^ ") + W(" ^ sk ^ "," ^ sj ^ ");") mode_MEDIUM_DEBUG;
				print_message ("W(" ^ si ^ "," ^ sj ^ ") <- " ^ (Constraint.string_of_exact_member cost_parameters members.(i).(j)) ^ ";") mode_MEDIUM_DEBUG;
				
				(* Update the matrix *)
				shortest_matrix.(i).(j) <- w_ikkj;
				print_message ("V(" ^ si ^ "," ^ sj ^ ") <- " ^ (string_of_num v_ikkj) ^ ";") mode_MEDIUM_DEBUG;
				
			| ConstInstCost v_ikkj, ConstInstCost v_ij ->
				(* W(i,k) + W(k,j) < W(i,j) : update and add constraint *)
				if v_ikkj </ v_ij then(
					(* Compute the sum of members *)
					let sum = Constraint.add_members members.(i).(k) members.(k).(j) in
					(* Add the inequality *)
					let inequality = Constraint.make_inequality
						sum Constraint.Op_l members.(i).(j)
					in
					the_constraint := (Constraint.add_inequality inequality !the_constraint);
					print_message ("...Adding {W(" ^ si ^ "," ^ sk ^ ") + W(" ^ sk ^ "," ^ sj ^ ") <= W(" ^ si ^ "," ^ sj ^ ")} ;") mode_MEDIUM_DEBUG;
					print_message ("K_0 := K_0 ^ {" ^ (Constraint.string_of_exact_constraint cost_parameters [inequality]) ^ "} ;") mode_MEDIUM_DEBUG;
										
					(* Update the members *)
					members.(i).(j) <- sum;
					print_message ("W(" ^ si ^ "," ^ sj ^ ") <- " ^ (Constraint.string_of_exact_member cost_parameters members.(i).(j)) ^ ";") mode_MEDIUM_DEBUG;
					(* Update the matrix *)
					shortest_matrix.(i).(j) <- w_ikkj;
					print_message ("V(" ^ si ^ "," ^ sj ^ ") <- " ^ (string_of_num v_ikkj) ^ ";") mode_MEDIUM_DEBUG;
				)else(
				
				(* W(i,k) + W(k,j) > W(i,j) : only add constraint *)
				if v_ikkj >/ v_ij then(
					(* Compute the sum of members *)
					let sum = Constraint.add_members members.(i).(k) members.(k).(j) in
					(* Add the inequality *)
					let inequality = Constraint.make_inequality
						sum Constraint.Op_g members.(i).(j)
					in
					the_constraint := (Constraint.add_inequality inequality !the_constraint);
					print_message ("K_0 := K_0 ^ {" ^ (Constraint.string_of_exact_constraint cost_parameters [inequality]) ^ "} ;") mode_MEDIUM_DEBUG;
				)else(
				
				(* W(i,k) + W(k,j) = W(i,j) : only add constraint *)
				if v_ikkj =/ v_ij then(
					(* Compute the sum of members *)
					let sum = Constraint.add_members members.(i).(k) members.(k).(j) in
					(* Add the inequality : WARNING add 'leq' !! *)
					let inequality = Constraint.make_inequality
						members.(i).(j) Constraint.Op_le sum
					in
					the_constraint := (Constraint.add_inequality inequality !the_constraint);
					print_message ("K_0 := K_0 ^ {" ^ (Constraint.string_of_exact_constraint cost_parameters [inequality]) ^ "} ;") mode_MEDIUM_DEBUG;
				) ) )
		done;
	done;
done;


(****************************************************************)
(** The Result *)
(****************************************************************)
(** RAJOUT 13/09/2010 **)
print_message ("W = ") mode_STANDARD;
for i = 0 to nb_states-1 do
	for j = 0 to nb_states-1 do
		print_message ("i = " ^ (states.(i)) ^ ", "
				^ "j = " ^ (states.(j)) ^ ""
			) mode_STANDARD;
			
		print_message (Constraint.string_of_exact_member cost_parameters members.(i).(j)) mode_STANDARD;
	done;
done;


print_message "\nFinal costs between states:" mode_STANDARD;
print_message
	(InspeqtorPrinter.string_of_instantiated_matrix_with_infinity
		abstract_program.states
		shortest_matrix
	)
	mode_STANDARD;

print_message "\nFinal constraint:" mode_STANDARD;
print_message
	(Constraint.string_of_exact_constraint cost_parameters (List.rev !the_constraint))
	mode_STANDARD;


(****************************************************************)
(****************************************************************)
(** Goodbye, world, goodbye! *)
(****************************************************************)
(****************************************************************)
);


terminate_program ()

