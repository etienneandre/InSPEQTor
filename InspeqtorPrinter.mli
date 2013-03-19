(*****************************************************************
 *
 *                     Inspeqtor
 * 
 * Compiler from Inspeqtor file to OCaml
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

(****************************************************************)
(** Functions *)
(****************************************************************)
(* Convert the instantiated program to a string*)
val string_of_instantiated_matrix : name array ->  instantiated_cost array array -> string

(* Convert the instantiated program to a string*)
val string_of_instantiated_matrix_with_infinity : name array ->  instantiated_cost array array -> string

(* Convert the parametric program to a string *)
val string_of_parametric_matrix : name array -> name array -> cost array array -> string
