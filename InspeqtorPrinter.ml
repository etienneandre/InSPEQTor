(*****************************************************************
 *
 *                     Inspeqtor
 * 
 * Compiler from Inspeqtor to OCaml
 *
 * This file prints an AbstractImperatorFile structure
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/04/27
 * Last modified: 2009/04/28
 *
 ****************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)
open AbstractStructure
open Global
open Num


(****************************************************************)
(** Print Mode *)
(****************************************************************)
type print_mode =
	| InstantiatedPrint
	| ParametricPrint


(****************************************************************)
(** String of structures *)
(****************************************************************)
(*
(* Convert to a string a transition from a orig_state_index to a dest_state_index *)
let string_of_transition orig_state_index dest_state_index abstract_program print_mode =
	match print_mode with
	(* Case : InstantiatedPrint *)
	| InstantiatedPrint ->
		let result =
		
		in result
	(* Case : InstantiatedPrint *)
	| ParametricPrint ->
		let result =

		in result
*)

(* Convert a program into a string *)
let string_of_instantiated_matrix_with_or_without_infinity with_infinity states instantiated_costs_matrix =
	let nb_states = Array.length states in
	(* The result *)
	let the_string = ref "" in
	(* For all origin state *)
	for orig_state_index = 0 to nb_states - 1 do
		(* Name of the state *)
		the_string := (!the_string) ^ "\n" ^ states.(orig_state_index) ^ ":";
		(* For all dest state *)
		for dest_state_index = 0 to nb_states - 1 do
			(* Transition (if any) *)
			the_string := !the_string ^ (
				match instantiated_costs_matrix.(orig_state_index).(dest_state_index) with
				(* Case : Constant cost *)
				| ConstInstCost const_value ->
					"\n\t->" 
					(* Cost *)
					^ " (" ^ (Num.string_of_num const_value) ^ ") "
					(* Dest state name *)
					^ states.(dest_state_index) ^ ";"
				(* Case infinity : no transition *)
				| InstInfinity -> 
					if with_infinity then (
					"\n\t->" 
					(* Cost *)
					^ " (infinity) "
					(* Dest state name *)
					^ states.(dest_state_index) ^ ";")
					else ""
			)
		done;
	done;
	(* Return our string *)
	!the_string

(* Convert a program into a string *)
let string_of_instantiated_matrix = string_of_instantiated_matrix_with_or_without_infinity false

(* Convert a program into a string with infinity *)
let string_of_instantiated_matrix_with_infinity = string_of_instantiated_matrix_with_or_without_infinity true
	

(* Convert a program into a string *)
let string_of_parametric_matrix states cost_parameters costs_matrix =
	let nb_states = Array.length states in
	(* The result *)
	let the_string = ref "" in
	(* For all origin state *)
	for orig_state_index = 0 to nb_states - 1 do
		(* Name of the state *)
		the_string := (!the_string) ^ "\n" ^ states.(orig_state_index) ^ ":";
		(* For all dest state *)
		for dest_state_index = 0 to nb_states - 1 do
			(* Transition (if any) *)
			the_string := !the_string ^ (
				match costs_matrix.(orig_state_index).(dest_state_index) with
				(* Case : Constant cost *)
				| ConstCost const_value ->
					"\n\t->" 
					(* Cost *)
					^ " (" ^ (Num.string_of_num const_value) ^ ") "
					(* Dest state name *)
					^ states.(dest_state_index) ^ ";"
				(* Case : Parametric cost *)
				| ParametricCost cost_index ->
					"\n\t->" 
					(* Cost *)
					^ " (" ^ cost_parameters.(cost_index) ^ ") "
					(* Dest state name *)
					^ states.(dest_state_index) ^ ";"
				(* Case infinity : no transition *)
				| Infinity -> ""
			)
		done;
	done;
	(* Return our string *)
	!the_string

