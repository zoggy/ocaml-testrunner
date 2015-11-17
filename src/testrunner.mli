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

module SMap : Map.S with type key = string
module J = Yojson.Safe
module Error :
  sig
    type error =
        Unexpected_json of J.json
      | Missing_type
      | Unhandled_type of string
      | Exception_in_test of exn
      | Invalid_input of string
    exception Error of error
    val error : error -> 'a
    val unexpected_json : J.json -> 'a
    val missing_type : unit -> 'a
    val unhandled_type : string -> 'a
    val exception_in_text : exn -> 'a
    val invalid_input : string -> 'a
    val to_string : ?exn_to_string:(exn -> string) -> error -> string
  end
module Env :
  sig
    type t = J.json SMap.t
    val to_json : 'a SMap.t -> [> `Assoc of (SMap.key * 'a) list ]
    val int : J.json SMap.t -> SMap.key -> int
  end
module Result :
  sig
    type t = {
      ok : bool;
      output : string option;
      expected : string option;
      result : string option;
    }
    val make :
      ?output:string -> ?expected:string -> ?result:string -> bool -> t
  end
module Tree :
  sig
    type t = {
      typ : string option;
      title : string option;
      id : string option;
      env : Env.t;
      subs : t list;
      result : [ `E of exn | `R of Result.t ] option;
    }
    val empty : t
    val of_json : t -> J.json -> t list
    val of_assoc : t -> (SMap.key * J.json) list -> t
    val of_file : ?t:t -> string -> t list
    val string_of_opt : string option -> string
    val run_test :
      print:(string -> 'a) ->
      pok:(string -> 'b) ->
      prerr:(string -> 'b) -> (Env.t -> Result.t) SMap.t -> t -> t
    val run :
      (Env.t -> Result.t) SMap.t ->
      ?print:(string -> unit) ->
      ?pok:(string -> unit) ->
      ?prerr:(string -> unit) -> ?re:Re_str.regexp -> t -> t
    val run_list :
      ?print:(string -> unit) ->
      ?pok:(string -> unit) ->
      ?prerr:(string -> unit) ->
      ?re:Re_str.regexp -> (Env.t -> Result.t) SMap.t -> t list -> t list
  end
module Xml :
  sig
    module X = Xtmpl_xml
    val section :
      ?atts:string X.with_loc X.attributes ->
      ?id:string -> ?title:string -> X.tree list -> X.tree
    val test :
      ?atts:string X.with_loc X.attributes ->
      ?id:string ->
      ?title:string ->
      ?input:string ->
      ?expected:string -> ?result:string -> ?output:string -> bool -> X.tree
    val to_xml : Tree.t list -> X.tree list
  end
module Report :
  sig
    type count = { ok : int; ko : int; err : int; }
    val count : Tree.t list -> count
    val string_of_count : count -> string
  end
