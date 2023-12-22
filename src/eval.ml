open Ast

exception TypeError of string
exception InexhaustivePatterns

type value =
  | VBool of bool
  | VUnit of unit
  | VInt of int
  | VString of string
  | VClosure of env * pat * expr
  | VBuiltin of (value list -> value * store)
  | VPair of value * value
  | VList of value list
  | VLoc of handle

and env = (string * value) list
and store = (int * value) list

(*****************************************************************************
  Below are a few simple helper functions you need to implement. These are used
  in various places throughout the system in working with your value and
  environment types. This is also a good spot to add any additional helper
  functions you might need involving values and environments.
 ******************************************************************************)

let rec string_of_value (v : value) : string =
  match v with
  | VBool b -> string_of_bool b
  | VUnit u -> "()"
  | VInt n -> string_of_int n
  | VString s -> "\"" ^ s ^ "\""
  | VClosure _ -> "<function>"
  | VBuiltin _ -> "<builtin>"
  | VPair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
  | VList vs ->
      let rec string_of_list vs =
        match vs with
        | [] -> ""
        | [ v ] -> string_of_value v
        | v :: vs -> string_of_value v ^ "; " ^ string_of_list vs
      in
      "[" ^ string_of_list vs ^ "]"
  | VLoc h -> "<loc " ^ string_of_int h ^ ">"

let print_impl args =
  match args with
  | [ arg ] ->
      let s = string_of_value arg in
      print_string s;
      (VUnit (), [])
  | _ -> raise (TypeError "print expects a single string argument")

let println_impl args =
  match args with
  | [ arg ] ->
      let s = string_of_value arg in
      print_endline s;
      (VUnit (), [])
  | _ -> raise (TypeError "println expects a single string argument")

let int_of_string_impl args =
  match args with
  | [ VString s ] ->
      ( (try VInt (int_of_string s)
         with _ -> raise (TypeError "Invalid int conversion")),
        [] )
  | _ -> raise (TypeError "int_of_string expects a single string argument")

let string_of_int_impl args =
  match args with
  | [ VInt n ] -> (VString (string_of_int n), [])
  | _ -> raise (TypeError "string_of_int expects a single integer argument")

let initial_env =
  [
    ("print", VBuiltin print_impl);
    ("println", VBuiltin println_impl);
    ("int_of_string", VBuiltin int_of_string_impl);
    ("string_of_int", VBuiltin string_of_int_impl);
  ]

let initial_store = []

(*****************************************************************************
  Below are some helper functions we have provided for environments.
 ******************************************************************************)

(** [update env x v] is the environment that maps [x] to [v] and maps every
    other variable [y] to whatever [env] maps [y] to. *)
let update_env env x v = (x, v) :: env

(** [rev_env env] is the environment [env] with the order of mappings reversed.

    That is,
    [update_env empty x1 v1 |> (fun e -> update_env e x2 v2) |> rev_env |> pop_env_opt]
    must equal [Some (x1, v1, env')], where [env'] equals
    [update_env empty x2 v2]. *)
let rev_env (env : env) : env = List.rev env

(** [find_env env x] is the value associated to [x] in [env]. Raises:
    [Not_found] if [x] is not associated to any value in [env]. *)
let find_env (env : env) (x : string) : value = List.assoc x env

let prepend_env (env1 : env) (env2 : env) : env = env1 @ env2

let rec take_env (n : int) (env : env) : env =
  if n = 0 then [] else List.hd env :: take_env (n - 1) (List.tl env)

let size_env (env : env) : int = List.length env

let string_of_env (env : env) : string =
  env
  |> List.map (fun (x, v) -> "val " ^ x ^ " = " ^ string_of_value v ^ "\n")
  |> List.fold_left ( ^ ) ""

let new_ref_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

let update_store ref_id value st =
  (ref_id, value) :: List.remove_assoc ref_id st

(***************************************************************************
   The rest of the functions in this file compose the main part of the RML
   interpreter. It is your task to implement these functions. Good luck!
 *****************************************************************************)

let rec bind_pattern (p : pat) (v : value) : env option =
  match (p, v) with
  | PWild, _ -> Some [] (* Wildcard matches any value *)
  | PVar x, _ -> Some [ (x, v) ] (* Variable binds to the value *)
  | PUnit, VUnit () -> Some []
  | PBool b, VBool vb when b = vb -> Some []
  | PInt n, VInt vn when n = vn -> Some []
  | PString s, VString vs when s = vs -> Some []
  | PPair (p1, p2), VPair (v1, v2) -> (
      match (bind_pattern p1 v1, bind_pattern p2 v2) with
      | Some env1, Some env2 -> Some (env1 @ env2)
      | _ -> None)
  | PNil, VList [] -> Some []
  | PCons (ph, pt), VList (vh :: vt) -> (
      match (bind_pattern ph vh, bind_pattern pt (VList vt)) with
      | Some envh, Some envt -> Some (envh @ envt)
      | _ -> None)
  | _, _ -> None

let rec eval_expr (env : env) (st : store) (expr : expr) : value * store =
  match expr with
  | EUnit () -> (VUnit (), st)
  | EBool b -> (VBool b, st)
  | EInt n -> (VInt n, st)
  | EString s -> (VString s, st)
  | EVar x -> (find_env env x, st)
  | EUop (u, e) -> (
      let v, st' = eval_expr env st e in
      match (u, v) with
      | Neg, VInt n -> (VInt (-n), st')
      | Not, VBool b -> (VBool (not b), st')
      | _ -> raise (TypeError "Invalid unary operation"))
  | EBop (bop, e1, e2) -> (
      let v1, st1 = eval_expr env st e1 in
      let v2, st2 = eval_expr env st1 e2 in
      match (bop, v1, v2) with
      | And, VBool b1, VBool b2 -> (VBool (b1 && b2), st2)
      | Or, VBool b1, VBool b2 -> (VBool (b1 || b2), st2)
      | Add, VInt n1, VInt n2 -> (VInt (n1 + n2), st2)
      | Sub, VInt n1, VInt n2 -> (VInt (n1 - n2), st2)
      | Mul, VInt n1, VInt n2 -> (VInt (n1 * n2), st2)
      | Div, VInt n1, VInt n2 ->
          if n2 = 0 then raise (TypeError "Division by zero")
          else (VInt (n1 / n2), st2)
      | Mod, VInt n1, VInt n2 ->
          if n2 = 0 then raise (TypeError "Modulo by zero")
          else (VInt (n1 mod n2), st2)
      | Lt, VInt n1, VInt n2 -> (VBool (n1 < n2), st2)
      | Gt, VInt n1, VInt n2 -> (VBool (n1 > n2), st2)
      | Le, VInt n1, VInt n2 -> (VBool (n1 <= n2), st2)
      | Ge, VInt n1, VInt n2 -> (VBool (n1 >= n2), st2)
      | Eq, VInt n1, VInt n2 -> (VBool (n1 = n2), st2)
      | Eq, VBool b1, VBool b2 -> (VBool (b1 = b2), st2)
      | Eq, VString s1, VString s2 -> (VBool (s1 = s2), st2)
      | Ne, VInt n1, VInt n2 -> (VBool (n1 <> n2), st2)
      | Ne, VBool b1, VBool b2 -> (VBool (b1 <> b2), st2)
      | Ne, VString s1, VString s2 -> (VBool (s1 <> s2), st2)
      | Cat, VString s1, VString s2 -> (VString (s1 ^ s2), st2)
      | _ -> raise (TypeError "Invalid binary operation or type mismatch"))
  | ELet (pattern, e1, e2) -> (
      let v1, st1 = eval_expr env st e1 in
      let bp = bind_pattern pattern v1 in
      match bp with
      | None -> raise InexhaustivePatterns
      | Some env' ->
          let env'' = prepend_env env' env in
          eval_expr env'' st1 e2)
  | EFun (p, e) -> (VClosure (env, p, e), st)
  | EApp (e1, e2) -> (
      let v1, st1 = eval_expr env st e1 in
      let v2, st2 = eval_expr env st1 e2 in
      match v1 with
      | VBuiltin func -> func [ v2 ]
      | VClosure (env', p, e) -> (
          let bp = bind_pattern p v2 in
          match bp with
          | None -> raise InexhaustivePatterns
          | Some env'' ->
              let env''' = prepend_env env'' env' in
              eval_expr env''' st2 e)
      | _ -> raise (TypeError "Application of non-function"))
  | EIf (e1, e2, e3) -> (
      let v1, st1 = eval_expr env st e1 in
      match v1 with
      | VBool true -> eval_expr env st1 e2
      | VBool false -> eval_expr env st1 e3
      | _ -> raise (TypeError "Condition in if-else expression must be boolean")
      )
  | EPair (e1, e2) ->
      let v1, st1 = eval_expr env st e1 in
      let v2, st2 = eval_expr env st1 e2 in
      (VPair (v1, v2), st2)
  | ENil -> (VList [], st)
  | ECons (e1, e2) -> (
      let v1, st1 = eval_expr env st e1 in
      let v2, st2 = eval_expr env st1 e2 in
      match v2 with
      | VList vs -> (VList (v1 :: vs), st2)
      | _ -> raise (TypeError "Second argument of cons must be a list"))
  | ESeq (e1, e2) ->
      let _, st1 = eval_expr env st e1 in
      eval_expr env st1 e2
  | EMatch (e, cs) ->
      let v, st' = eval_expr env st e in
      eval_match env st' v cs
  | ERef e ->
      let v, st' = eval_expr env st e in
      let ref_id = new_ref_id () in
      (VLoc ref_id, (ref_id, v) :: st')
  | EDeref e -> (
      let v, st' = eval_expr env st e in
      match v with
      | VLoc ref_id -> (List.assoc ref_id st', st')
      | _ -> raise (TypeError "Dereference of non-reference value"))
  | EAssign (e1, e2) -> (
      let v1, st1 = eval_expr env st e1 in
      let v2, st2 = eval_expr env st1 e2 in
      match v1 with
      | VLoc ref_id -> (VUnit (), update_store ref_id v2 st2)
      | _ -> raise (TypeError "Assignment to non-reference value"))

and eval_match env st v cs =
  match cs with
  | [] -> raise InexhaustivePatterns
  | (p, e) :: rest -> (
      match bind_pattern p v with
      | None -> eval_match env st v rest
      | Some env' ->
          let env'' = prepend_env env' env in
          eval_expr env'' st e)

let eval_defn (env : env) (st : store) (defn : defn) : env * store =
  match defn with
  | DLet (p, e) -> (
      let v, st' = eval_expr env st e in
      match bind_pattern p v with
      | Some binding -> (binding @ env, st')
      | None -> raise (TypeError "Pattern match failure in let definition"))

let eval_program (env : env) (st : store) (prog : prog) : env * store =
  List.fold_left (fun (env, st) defn -> eval_defn env st defn) (env, st) prog
