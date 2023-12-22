open Rml
open OUnit2

let make_test (name : string) (expr_str : string) (val_str : string) =
  name >:: fun _ ->
  assert_equal val_str
    (Main.interp_expr Checker.Context.empty Eval.initial_env Eval.initial_store
       expr_str
     |> function
     | Ok (x, _) | Error (ParseError x) | Error (TypeError x) -> x)
    ~printer:(fun x -> x)

let tests =
  [
    make_test "unit" {|()|} {|()|};
    make_test "true" "true" "true";
    make_test "false" "false" "false";
    make_test "zero" "0" "0";
    make_test "negative" "-1" "-1";
    make_test "positive" "5" "5";
    make_test "forty-two" {| 42 |} {|42|};
    make_test "string" {| "zardoz" |} {|"zardoz"|};
    make_test "pair of ints" "(1,4)" "(1, 4)";
    make_test "int identity function" "fun (x : int) ->\n       x" "<function>";
    make_test "application" "(fun (x : int) -> x +\n       5) 37" "42";
    make_test "uop negate" "- (5 + 5)" "-10";
    make_test "bop add" "5 +5 +5 " "15";
    make_test "bop sub" "5 -3 -1 " "1";
    make_test "bop mul" "5 * 5 * 5" "125";
    make_test "bop div" "42 / 2" "21";
    make_test "bop mod" "42 % 37" "5";
    make_test "bop and" "true && false" "false";
    make_test "bop or" "true || false" "true";
    make_test "bop and" "true && true" "true";
    make_test "bop or" "false || false" "false";
    make_test "bop pipe" "5 |> (fun (x: int) -> x + 37)" "42";
    make_test "cons" "5 :: 3 ::\n       2 :: 1 :: []" "[5; 3; 2; 1]";
    make_test "basic sequence" "(); 5" "5";
    make_test "match bool true"
      "match 5 = 5 with | true -> 42 |\n       false -> 0 end" "42";
    (* make_test "let rec paren 1"
      "let rec (f : int -> int) = fun (x : int) -> if x = 0 then 1 else x * f \
       (x - 1) in f 5"
      "120";
    make_test "let rec no_paren 1"
      "let rec f : int -> int = fun (x : int) -> if x = 0 then 1 else x * f (x \
       - 1) in f 5"
      "120";  *)
  ]

let suite = "suite" >::: tests
let () = run_test_tt_main suite
