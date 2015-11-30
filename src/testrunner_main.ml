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

let run handlers trees =
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
  Tree.run_list ~print ?pok ?prerr handlers trees

let plugin_files = ref []

type report_mode = Count | Xml

(* FIXME: output directly to channels insteaf of using intermediate strings.
     This require having Xtmpl_xml.to_channel and Xtmpl_xml.to_file. *)
let report stdout_mode output_modes trees =
  begin
    match stdout_mode with
      None -> ()
    | Some Count ->
        print_endline (Report.string_of_count (Report.count trees))
    | Some Xml ->
        let xmls = Xml.to_xml trees in
        print_endline (Xtmpl_xml.to_string xmls)
  end;
  List.iter
    (fun (file, mode) ->
       let str =
         match mode with
           Count -> Report.string_of_count (Report.count trees)
         | Xml ->
             let xmls = Xml.to_xml trees in
             Xtmpl_xml.to_string xmls
       in
       Xtmpl_misc.file_of_string ~file str
    )
    output_modes

let stdout_mode = ref (Some Count)
let output_modes = ref ([] : (string * report_mode) list)

let add_output mode = function
  "-" -> stdout_mode := Some mode
| file -> output_modes := (file, mode) :: !output_modes

let options = [
    "-l", Arg.String (fun s -> plugin_files := s :: !plugin_files),
    "file.cm{o,a,xs} load the given plugin file" ;

    "--xml", Arg.String (add_output Xml),
    "file output XML report to file (or stdout if file is '-')" ;

    "--count", Arg.String (add_output Count),
    "file output count report to file (or stdout if file is '-')" ;

    "--nostdout", Arg.Unit (fun () -> stdout_mode := None),
    " do not report on stdout" ;
  ]

let usage =
  Printf.sprintf "Usage: %s [options] <json files>" Sys.argv.(0)

let merge_handlers =
  SMap.merge
    (fun k v1 v2 ->
       match v1, v2 with  _, Some f -> Some f
       | v, _ -> v)

let main () =
  let args = ref [] in
  Arg.parse options
    (fun str -> args := str :: !args)
    (usage ^ "\nwhere options are:");
  match List.rev !args with
    [] -> failwith usage
  | files ->
      try
        let print fmt = Printf.ksprintf
          (fun str -> output_string stderr str; flush stderr) fmt
        in
        let handlers = List.fold_left
          (fun acc file ->
             let h = Testrunner_dl.load ~print file in
             merge_handlers acc h)
            Testrunner.SMap.empty (List.rev !plugin_files)
        in
        let trees = List.flatten (List.map Tree.of_file files) in
        let trees = run handlers trees in
        report !stdout_mode !output_modes trees
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