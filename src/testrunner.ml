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

module SMap = Map.Make(String)
module J = Yojson.Safe;;
open Lwt.Infix
module Error =
  struct
    type error =
      | Unexpected_json of J.json
      | Missing_type
      | Unhandled_type of string
      | Exception_in_test of exn
      | Invalid_input of string

    exception Error of error
    let error e = raise (Error e)
    let lwt_error e = Lwt.fail (Error e)
    let unexpected_json json = error (Unexpected_json json)
    let missing_type () = error Missing_type
    let lwt_missing_type () = lwt_error Missing_type
    let unhandled_type typ = error (Unhandled_type typ)
    let lwt_unhandled_type typ = lwt_error (Unhandled_type typ)
    let exception_in_text e = error (Exception_in_test e)
    let invalid_input str = error (Invalid_input str)

    let exn_to_string = ref (function e -> Printexc.to_string e)
    let add_exn_to_string f =
      exn_to_string := (f !exn_to_string)

    let rec to_string = function
    | Unexpected_json json ->
        Printf.sprintf "Unexpected JSON: %s" (J.to_string json)
    | Missing_type -> "Missing _type"
    | Unhandled_type typ -> Printf.sprintf "Unhandled type %S" typ
    | Exception_in_test (Error e) ->
        Printf.sprintf "Uncaught exception in test: %s" (to_string e)
    | Exception_in_test e ->
        Printf.sprintf "Uncaught exception in test: %s" (!exn_to_string e)
    | Invalid_input str ->
        Printf.sprintf "Invalid input: %s" str
  end

module Env =
  struct
    type t = J.json SMap.t
    let to_json env =
      `Assoc
        (SMap.fold (fun name json acc -> (name, json) :: acc)
         env [])

    let opt env var (f:?var:string -> J.json -> 'a) =
      match SMap.find var env with
      | exception Not_found -> None
      | json -> Some (f ~var json)

    let string_of_json ?var = function
    | `String n -> n
    | json -> Error.invalid_input
        (Printf.sprintf "Invalid string value%s: %s" (J.to_string json)
         (match var with None -> "" | Some s -> Printf.sprintf "(%s)" s)
        )

    let int_of_json ?var = function
    | `Int n -> n
    | json -> Error.invalid_input
        (Printf.sprintf "Invalid integer value%s: %s" (J.to_string json)
         (match var with None -> "" | Some s -> Printf.sprintf "(%s)" s)
        )

    let float_of_json ?var = function
    | `Float n -> n
    | json -> Error.invalid_input
        (Printf.sprintf "Invalid float value%s: %s" (J.to_string json)
         (match var with None -> "" | Some s -> Printf.sprintf "(%s)" s)
        )

    let bool_of_json ?var = function
    | `Bool b -> b
    | json -> Error.invalid_input
        (Printf.sprintf "Invalid bool value%s: %s" (J.to_string json)
         (match var with None -> "" | Some s -> Printf.sprintf "(%s)" s)
        )

    let require env var (f:?var:string -> J.json -> 'a) =
      match SMap.find var env with
      | exception Not_found -> Error.invalid_input ("Missing "^var)
      | json -> f ~var json

    let string env var = require env var string_of_json
    let int env var = require env var int_of_json
    let float env var = require env var float_of_json
    let bool env var = require env var bool_of_json

    let pair_of_json
      (f1:?var:string -> J.json -> 'a)
      (f2:?var:string -> J.json -> 'b) ?var = function
      `List (x :: y :: _) -> (f1 ?var x, f2 ?var y)
    | json -> Error.invalid_input
        (Printf.sprintf "Invalid pair value%s: %s" (J.to_string json)
         (match var with None -> "" | Some s -> Printf.sprintf "(%s)" s)
        )
    let pair env var f1 f2 = require env var (pair_of_json f1 f2)
  end


module Result =
  struct
    type t = {
        ok : bool ;
        output : string option ;
        expected: string option ;
        result: string option ;
        xml_expected : Xtmpl_xml.tree list option ;
        xml_result : Xtmpl_xml.tree list option ;
      }

    let make ?output ?expected ?result ?xml_expected ?xml_result ok =
      { ok ; output ; expected ; result ; xml_expected ; xml_result }
  end

module Tree =
  struct
    type t = {
        typ: string option ;
        title: string option ;
        id: string option ;
        env: Env.t ;
        subs: t list ;
        result: [`R of Result.t | `E of exn] option ;
      }

    let empty =
      { typ = None ; title = None ; id = None ;
        env = SMap.empty ; subs = [] ;
        result = None ;
      }

    let rec of_json t = function
      `Assoc l -> [of_assoc t l]
    | `List l -> List.flatten (List.map (of_json t) l)
    | json -> Error.unexpected_json json

    and of_assoc =
        let f t (name, json) =
          match name with
          | "_type" ->
              begin
                match json with
                  `String str -> { t with typ = Some str }
                | _ -> Error.unexpected_json json
              end
          | "_id" ->
              begin
                match json with
                  `String str -> { t with id = Some str }
                | _ -> Error.unexpected_json json
              end
          | "_title" ->
              begin
                match json with
                  `String str -> { t with title = Some str }
                | _ -> Error.unexpected_json json
              end
          | "_include" ->
              begin
                match json with
                  `String str ->
                    let l = of_file ~t str in
                    { t with subs = t.subs @ l }
                | _ -> Error.unexpected_json json
              end
          | "_list" ->
              begin
                match json with
                  `List l ->
                    let t2 = { t with id = None ; subs = [] } in
                    let l = of_json t2 json in
                    { t with subs = t.subs @ l }
                | _ -> Error.unexpected_json json
              end
          | field ->
              match json with
                `Null ->
                  let env = SMap.remove field t.env in
                  { t with env }
              | _ ->
                  let env = SMap.add field json t.env in
                  { t with env }
        in
        fun t l ->
          List.fold_left f t l

    and of_file ?(t=empty) file =
      of_json { t with id = None ; subs = []} (J.from_file file)

    let string_of_opt = function None -> "" | Some s -> s

    let run_test ~print ~ok ~err handlers path t =
      let open Result in
      print (Printf.sprintf "Running test %s" path);
      match t.typ with
        None -> Error.missing_type ()
      | Some typ ->
          match SMap.find typ handlers with
          | exception Not_found -> Error.unhandled_type typ
      | f ->
        try
          let r = f t.env in
          let msg = if r.ok then ok "OK" else err "Fail" in
          print (Printf.sprintf "[%s] %s" path msg);
          let result = Some (`R r) in
          { t with result }
        with
          e ->
            print (Printf.sprintf "[%s] %s" path
              (err (Error.to_string (Error.Exception_in_test e)))) ;
            { t with result = Some (`E e) }

    let run handlers ?(print=fun _ -> ())
      ?(ok=fun s -> s) ?(err=fun s -> s) ?(re=Re_str.regexp ".*") t =
      let rec iter re path t =
        let path = path ^ "/" ^ (match t.id with None -> "" | Some s -> s) in
        match t.subs with
          [] ->
            if Re_str.string_match re path 0
            then run_test ~print ~ok ~err handlers path t
            else t
        | l ->
            print (Printf.sprintf "Testing section %s" path);
            let subs = List.map (iter re path) t.subs in
            { t with subs }
      in
      iter re "" t

    let run_list ?print ?ok ?err ?re handlers =
      List.map (run ?print ?ok ?err ?re handlers)

    let lwt_run_test ~print ~ok ~err handlers path t =
      let open Result in
      let%lwt () = print (Printf.sprintf "Running test %s" (string_of_opt t.id)) in
      match t.typ with
        None -> Error.lwt_missing_type ()
      | Some typ ->
          match SMap.find typ handlers with
          | exception Not_found -> Error.lwt_unhandled_type typ
      | f ->
        try%lwt
          let%lwt r = f t.env in
          let msg = if r.ok then ok "OK" else err "Fail" in
          print (Printf.sprintf "[%s] %s" path msg) >>= fun () ->
          let result = Some (`R r) in
          Lwt.return { t with result }
        with
          e ->
            print (Printf.sprintf "[%s] %s" path
             (err (Error.to_string (Error.Exception_in_test e))))
              >>= fun () ->
                Lwt.return { t with result = Some (`E e) }

    let lwt_run handlers ?(print=fun _ -> Lwt.return_unit)
      ?(ok=fun s -> s) ?(err=fun s -> s) ?(re=Re_str.regexp ".*") t =
      let rec iter re path t =
        let path = path ^ "/" ^ (match t.id with None -> "" | Some s -> s) in
        match t.subs with
          [] ->
            if Re_str.string_match re path 0
            then lwt_run_test ~print ~ok ~err handlers path t
            else Lwt.return t
        | l ->
            let%lwt () = print (Printf.sprintf "Testing section %s" path) in
            let%lwt subs = Lwt_list.map_p (iter re path) t.subs in
            Lwt.return { t with subs }
      in
      iter re "" t

    let lwt_run_list ?print ?ok ?err ?re handlers =
      Lwt_list.map_p (lwt_run ?print ?ok ?err ?re handlers)
  end

module Xml =
  struct
    module X = Xtmpl_xml
    open Tree
    open Result
    let section ?(atts=X.atts_empty) ?id ?title subs =
      let atts = match id with
        | None -> atts
        | Some str -> X.atts_one ~atts ("","id") (str, None)
      in
      let atts = match title with
        | None -> atts
        | Some str -> X.atts_one ~atts ("","title") (str, None)
      in
      X.node ("","test-section") ~atts subs

    let test ?(atts=X.atts_empty)
      ?id ?title ?input ?expected ?result ?xml_expected ?xml_result ?output status =
      let atts = match id with
        | None -> atts
        | Some str -> X.atts_one ~atts ("","id") (str, None)
      in
      let atts = match title with
        | None -> atts
        | Some str -> X.atts_one ~atts ("","title") (str, None)
      in
      let atts =
        X.atts_one ~atts ("","status") ((if status then "ok" else "ko"), None)
      in
      let subs = [] in
      let subs = match output with
        | None -> subs
        | Some str -> (X.node ("", "test-output") [X.cdata str]) :: subs
      in
      let subs =
        let xmls =
          match xml_result with
          | Some _ -> xml_result
          | None ->
              match result with
                None -> None
              | Some str -> Some [X.cdata str]
        in
        match xmls with
          None -> subs
        | Some xmls -> (X.node ("","test-result") xmls) :: subs
      in
      let subs =
        let xmls =
          match xml_expected with
          | Some _ -> xml_expected
          | None ->
              match expected with
                None -> None
              | Some str -> Some [X.cdata str]
        in
        match xmls with
          None -> subs
        | Some xmls -> (X.node ("","test-expected") xmls) :: subs
      in
      let subs = match input with
        | None -> subs
        | Some str -> (X.node ("","test-input") [X.cdata str]) :: subs
      in
      X.node ("","test") ~atts subs

    let to_xml =
      let xml_of_test t =
        let ret = test ?id: t.id ?title: t.title in
        match t.result with
          Some (`R r) ->
            let expected = string_of_opt r.expected in
            let result = string_of_opt r.result in
            let output = string_of_opt r.output in
            let xml_expected = r.xml_expected in
            let xml_result = r.xml_result in
            ret ~expected ~result ~output ?xml_expected ?xml_result r.ok
        | _ -> ret false
      in
      let rec iter t =
        match t.subs with
          [] -> xml_of_test t
        | subs ->
            let subs = List.map iter t.subs in
            section ?id: t.id ?title: t.title subs
      in
      List.map iter
  end

module Report =
  struct
    open Tree
    open Result
    type count = { ok: int; ko: int; err: int }

    let count =
      let rec iter count (t: Tree.t) =
        match t.result with
          None -> List.fold_left iter count t.subs
        | Some r ->
            match r with
              `R { ok = true } -> { count with ok = count.ok + 1 }
            | `R { ok = false } -> { count with ko = count.ko + 1 }
            | `E _ -> { count with err = count.err + 1 }
      in
      List.fold_left iter { ok = 0; ko = 0; err = 0 }

    let string_of_count count =
      let total = count.ok + count.ko + count.err in
      let total_f = float total in
      let (pct_ok, pct_fail, pct_err) =
        if total <= 0 then
          (0., 0., 0.)
        else
          ((float count.ok /. total_f) *. 100.,
           (float count.ko /. total_f) *. 100.,
           (float count.err /. total_f) *. 100.)
      in
      Printf.sprintf
        "Success: %d/%d (%.2f%%)\nFailed: %d/%d (%.2f%%)\nCould not run: %d/%d (%.2f%%)"
        count.ok total pct_ok
        count.ko total pct_fail
        count.err total pct_err

      let pb b fmt = Printf.bprintf b fmt
      let string_of_path l = "/" ^ (String.concat "/" l)

      let print_field b margin name str_opt =
         pb b "%s%s: " margin name ;
         Buffer.add_string b (string_of_opt str_opt) ;
         Buffer.add_string b "\n"

      let print_test b margin path t =
        pb b "%sTest %s: " margin (string_of_path (List.rev path)) ;
        match t.Tree.result with
          Some (`R { Result.ok = true }) ->
                pb b "%s" "OK\n"
        | Some (`R r) ->
                pb b "%s" "Fail\n";
                let margin = margin ^ "  " in
                print_field b margin "expected" r.expected ;
                print_field b margin "  result" r.result ;
                if r.output <> None then print_field b margin "  output" r.output ;
        | Some (`E e) ->
                pb b "%s" "Fail with exception\n" ;
                pb b "%s%s\n" margin (Error.to_string (Error.Exception_in_test e)) ;
        | None -> ()

      let print b =
         let rec iter margin path t =
           let path = (string_of_opt t.id) :: path in
           match t.subs with
            [] -> print_test b margin path t
          | subs ->
            pb b "%s=== %s\n" margin (string_of_opt t.title);
            List.iter (iter (margin^"  ") path) t.subs ;
      in
      List.iter (iter "" [])
  end
(*
let () =
  try

    let t = Tree.of_file Sys.argv.(1) in
    let handlers = SMap.singleton "square" square in
    let print = print_endline in
    let pok str = print_endline
      (Printf.sprintf "\027[1;32m%s\027[0m" str)
    in
    let prerr str = print_endline
      (Printf.sprintf "\027[1;31m%s\027[0m" str)
    in
    let t = Tree.run_list ~print ~pok ~prerr handlers t in
    let xml = Xml.to_xml t in
    print_endline (Xtmpl_xml.to_string xml);
    print_endline (Report.string_of_count (Report.count t));
  with
    Error.Error e ->
      prerr_endline (Error.to_string e); exit 1
*)
