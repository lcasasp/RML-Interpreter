(******************************************************************************
   You do not need to modify anything in this file.
 ******************************************************************************)

open Rml

exception Quit

type state = {
  env : Eval.env;
  st : Eval.store;
  context : Checker.Context.t;
}

let init_state =
  {
    env = Eval.initial_env;
    st = Eval.initial_store;
    context = Checker.Context.empty;
  }

(* evaluate user input *)
let rec eval state input =
  match input |> String.split_on_char ' ' |> List.filter (( <> ) "") with
  | [ "#quit" ] -> raise Quit
  | [ "#env" ] -> (state, Eval.string_of_env state.env)
  | [ "#use"; fn ] -> (
      match Main.interp_file_repl fn state.context state.env state.st with
      | Ok (result, env', st', ctx') ->
          ({ env = env'; st = st'; context = ctx' }, result)
      | Error (Main.TypeError err) | Error (Main.ParseError err) -> (state, err)
      )
  | _ -> (
      match Main.interp_prog state.context state.env state.st input with
      | Ok (result, env', st', ctx') ->
          ({ env = env'; st = st'; context = ctx' }, result)
      | Error (Main.TypeError err) -> (state, err)
      | Error (Main.ParseError _) -> (
          match Main.interp_expr state.context state.env state.st input with
          | Ok (value, store) -> ({ state with st = store }, value)
          | Error (Main.ParseError err) | Error (TypeError err) -> (state, err))
      )

(* collect user input, terminated by ;; *)
let rec collect acc =
  let input = read_line () in
  if Str.string_match (Str.regexp {|^.*;;|}) input 0 then
    let input = String.sub input 0 (String.length input - 2) in
    acc ^ input
  else collect (acc ^ input)

(* read-eval-print loop *)
let rec repl (state : state) : unit =
  try
    print_string "# ";
    let input = collect "" in
    let state, output = eval state input in
    print_endline output;
    repl state
  with Quit -> ()

let _ =
  print_endline "\nRML Repl\n";
  repl init_state
