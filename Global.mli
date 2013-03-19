(*****************************************************************
 *
 *                     INSPEQTOR
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       04/03/2009
 * Last modified: 2009/05/04
 *
 ****************************************************************)


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception ParsingError of string
exception InternalError of string


(****************************************************************)
(** Debug modes *)
(****************************************************************)
val mode_ERROR		: int
val mode_NODEBUG	: int
val mode_STANDARD	: int
val mode_LOW_DEBUG	: int
val mode_MEDIUM_DEBUG	: int
val mode_HIGH_DEBUG	: int
val mode_TOTAL_DEBUG	: int


(****************************************************************)
(** Global constants *)
(****************************************************************)
val default_beta : Num.num


(****************************************************************)
(** Useful functions *)
(****************************************************************)
(* Convert an array of string into a string *)
val string_of_array_of_string : string array -> string

(* Convert a list of string into a string *)
val string_of_list_of_string : string list -> string



(****************************************************************)
(** Messages *)
(****************************************************************)
(* Print a message in function of debug_level *)
val print_debug_message : int -> int -> string -> unit

(* Print a warning *)
val print_warning : string -> unit

(****************************************************************)
(** Terminating functions *)
(****************************************************************)
(* Abort program *)
val abort_program : unit -> unit

(* Terminate program *)
val terminate_program : unit -> unit
