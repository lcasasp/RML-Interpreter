(** Functions in main may emit an error type if the function ran into either a
    parse error, constructed with [ParseError], or type error constructed with
    [TypeError] *)
type error =
  | ParseError of string
  | TypeError of string

val interp_expr :
  Checker.Context.t ->
  Eval.env ->
  Eval.store ->
  string ->
  (string * Eval.store, error) result
(** [interp_expr ctx env st rml_expr] is either the string representation of the
    value AND the new store that results from the evaluation of rml_expr to
    under the environment env, store st, and typing context ctx or an error
    thrown in interpretation. *)

val interp_prog :
  Checker.Context.t ->
  Eval.env ->
  Eval.store ->
  string ->
  (string * Eval.env * Eval.store * Checker.Context.t, error) result
(** [interp_expr ctx env st rml_prog] is either the string representation of the
    environment that [rml_prog] evaluates to under the typing context [ctx],
    enviornment [env], and store [st], or an error thrown in interpretation.
    @return
      (str, env', st', ctx') where str is the string representation. [env'] is
      the environment with all the definitions from [rml_prog], [st] is the
      store with all store values from [rml_prog], [ctx'] is the typing context
      with all the types of the definitions from [rml_prog] *)

val interp_file : string -> unit
(** [interp_file fn] prints the output to standard out or the parse error or
    type checker error to standard out. *)

val interp_file_repl :
  string ->
  Checker.Context.t ->
  Eval.env ->
  Eval.store ->
  (string * Eval.env * Eval.store * Checker.Context.t, error) result
(** Same as [interp_prog], but takes input from a file. Called from the repl
    with current state. *)
