# ocaml-testrunner

Simple framework to run tests and create test reports for OCaml libraries.

## How it works

Tests are described in JSON files, with nested JSON values.
Each leaf of these trees is a test, and the input values of the
test are defined from the root of the tree to the leaf. This allows
sharing input value definitions between tests. A new binding
between a name and a value hides the previous binding of this name
in the path.

These definitions can be split in separate JSON files, using
`_include: "myfile.json"` fields.

The type of test is defined by the `_type: "mytype"` fields.
For each type of test, a corresponding function must be registerered
in testrunner. This can be done in two ways:

- compile a testrunner plugin which register test types, and
run `testrunner` with `-l` option to load the plugin at runtime.
- or create your own testrunner executable embedding your code, which
also registers test types, with the provided `mk-testrunner script`
(invoked with `ocamlfind testrunner/mk-testrunner`).

The `testrunner` package contains the `Testrunner` and `Testrunner_dl`
modules used by the user code to define test functions: retrieving
input and expected values from environment (i.e. the values defined
along the test path, module `Testrunner.Env`), and creating test
result values (module `Testrunner.Result`).

Additional fields can be provided to create sectionning in reports.

`testrunner --help` lists the available options, including the various
output report formats (text, XML).

## Example

An example is provided in the `/test` directory. It tests a
`buggy_square : ?foo:bool -> int -> int` function. Here is the code
to define a function for the test type "square":

````ocaml
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
  Result.make
    ~expected: (string_of_int expected)
    ~result: (string_of_int res)
    (res = expected)
;;

(* Register our test type *)
let () = Testrunner_dl.add_handler "square" square;;
````

We can compile this code and create our own executable with the following
command. Of course, with real life code, you will add the required
`-package` options and the library your tests will call.

````
$ ocamlfind testrunner/mk-testrunner -o mytestrunner mycode.ml
````

Then we define our tests in a file like `test.json`:
````json
{ _type: "square",
  _id: "main",
  _include: "test1.json",
  _include: "test2.json",
  _list: [
    { _title: "test with 1",
      x: 1,
      result: 1
    },
    { _title: "test with 2",
      _id: "x2",
      x: 2,
      result: 4
    },
    { _id: "section2",
      _title: "Higher values of x",
      _list: [
      {
        _id: "x4",
        _title: "x = 4",
        x: 4
      }
     ]
    }
  ]
}
````

Then we can run our tests and create both XML and test reports:

````
$ ./mytestrunner --xml report.xml --text report.txt --count - test.json
````

Note that functions associated to test types can also return
`Lwt.t` values. In this case, they must be registered with
`Testrunner_dl.add_lwt_handler` and the testrunner executable must
be called with `--lwt`.

## Special JSON fields

Special fields in JSON objects begin with an underscore:

- `_type: "string"`: set the test type to `string`,
- `_id: "string"`: set the id of the section or the test, used in reports,
- `_include : "file"`: include the given file, allows to separate and reuse test
    and value defininitions,
- `_list: [ ...]`: add nodes to the given tree node,
- `_title: "string"`: set the title of a section. A section is a node in the
  definition tree.

