open Testrunner

let buggy_square x = if x < 3 then x * x else x * 2

let square env =
  let x = Env.int env "x" in
  let expected = Env.int env "result" in
  let res = buggy_square x in
  Result.make
    ~expected: (string_of_int expected)
    ~result: (string_of_int res)
    (res = expected)
;;

let () = Testrunner_dl.add_handler "square" square;;
