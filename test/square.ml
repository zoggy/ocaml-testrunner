open Testrunner

(** The function to test. It behaves correctly
   except if `foo` is true, the function then returns
   `2 * x` instead of `x * x`.*)
let buggy_square ?(foo=false) x = if foo then x * 2 else x * x

(** We define the function which will be associated to the "square"
   test type.*)
let square env =
  (* retrieve integer "x" from environment *)
  let x = Env.int env "x" in
  (* retrieve optional "foo" boolean from environment *)
  let foo = Env.(opt env "foo" bool_of_json) in
  (* retrieve expected integer from environment *)
  let expected = Env.int env "result" in
  (* call the function to test *)
  let res = buggy_square ?foo x in
  (* build the result, by comparing expected result and obtained value, *)
  (* and provide string representations of value for reports *)
  `Single (Tree.single_r
    ~expected: (string_of_int expected)
    ~result: (string_of_int res)
    (res = expected))
;;

(* Register our test type *)
let () = Testrunner_dl.add_handler "square" square;;
