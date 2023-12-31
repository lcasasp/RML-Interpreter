(** Implements the big-step environment model semantics. Important note:
    the the behavior of the interpreter is undefined on programs that do
    not pass the type-checker. You are free to evaluate malformed
    programs in any way you see fit. *)

open Ast

type value
(** [value] is the type of RML values *)

type env
(** [env] is an environment, which maps identifiers to values *)

type store
(** [store] is a store, which maps locations to values*)

val initial_env : env
(** [initial_env] is the environment in which evaluation begins. It must
    contain all the external functions defined in the writeup. *)

val initial_store : store
(**[initial_store] is the store in which evaluation begins. *)

val update_env : env -> id -> value -> env
(** [update_env env x v] is [env] updated with a mapping from [x] to
    [v]. *)

val prepend_env : env -> env -> env
(** [prepend_env env1 env2] is [env2] updated with all mappings from
    [env1]. *)

val take_env : int -> env -> env
(** [take_env n env] gets the first [n] bindings from [env], throws if
    [n] is bigger than the length of [env]. *)

val size_env : env -> int
(** [size_env env] gets the number of bindings in [env]. *)

val string_of_value : value -> string
(** [string_of_value v] is a string representing value [v].
    - If [v] is a unit, that string should be ["()"].
    - If [v] is a bool, that string should be [string_of_bool v].
    - If [v] is an int, that string should be [string_of_int v].
    - If [v] is a string, that string should be
      ["\"" ^ String.escaped v ^ "\""].
    - If [v] is a function, that string should be ["<function>"].
    - If [v] is a ref, that string should be ["<ref>"].
    - If [v] is a pair [(v1, v2)], that string should be
      ["(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"].
    - If [v] is a list, that string should be ["<list>"]. *)

val string_of_env : env -> string
(** [string_of_env env] is a string representation of [env]. It is up to
    you how to construct that string; it is to be used by you for the
    purposes of debugging and will not be used for grading. *)

val bind_pattern : pat -> value -> env option
(** [bind_pattern p v] tries to match [v] with [p]. If successful and
    bindings [b] are produced, then [b] is returned. This should return
    [None] if the pattern [p] does not match the value [v]. *)

val eval_expr : env -> store -> expr -> value * store
(** [eval_expr env st e] evaluates [e] under environment [env] and
    returns the resulting value and store.*)

val eval_defn : env -> store -> defn -> env * store
(** [eval_defn env st d] evaluates [d] under environment [env] and
    returns an an updated environment with the new mappings defined by
    [d], and the possibly updated store. *)

val eval_program : env -> store -> prog -> env * store
(** [eval_program env st prog] evaluates the the program [p] under the
    environment [env] and store [st] and returns the resulting
    environment and store. *)
