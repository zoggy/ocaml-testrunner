(*********************************************************************************)
(*                Testrunner                                                     *)
(*                                                                               *)
(*    Copyright (C) 2015 INRIA All rights reserved.                              *)
(*    Author: Maxence Guesdon, INRIA Saclay                                      *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License as             *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Lesser General Public License for more details.                        *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)


(** Main module of provided testrunner program. *)

open Testrunner

let handlers = ref SMap.empty

let run trees =
  let print = print_endline in
  let (pok, prerr) =
    if Unix.isatty Unix.stderr then
      let pok str = print_endline
        (Printf.sprintf "\027[1;32m%s\027[0m" str)
      in
      let prerr str = print_endline
        (Printf.sprintf "\027[1;31m%s\027[0m" str)
      in
      (Some pok, Some prerr)
    else
      (None, None)
  in
  let trees = Tree.run_list ~print ?pok ?prerr !handlers trees in
  print_endline (Report.string_of_count (Report.count trees))

let options = [

  ]

let usage = Printf.sprintf "Usage: %s [options] <json files>" Sys.argv.(0)

let main () =
  let args = ref [] in
  Arg.parse options
    (fun str -> args := str :: !args)
    (usage ^ "\nwhere options are:");
  match List.rev !args with
    [] -> failwith usage
  | files ->
      try
        let trees = List.flatten (List.map Tree.of_file files) in
        run trees
      with
        Error.Error e ->
          failwith (Error.to_string e)


(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let () = safe_main main