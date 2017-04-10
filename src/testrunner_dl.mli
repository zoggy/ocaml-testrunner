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

val handlers : unit -> (Testrunner.Env.t -> Testrunner.Result.t) Testrunner.SMap.t

val lwt_handlers :
  unit -> (Testrunner.Env.t -> Testrunner.Result.t Lwt.t) Testrunner.SMap.t

val add_handler :
  Testrunner.SMap.key -> (Testrunner.Env.t -> Testrunner.Result.t) -> unit

val add_lwt_handler :
  Testrunner.SMap.key -> (Testrunner.Env.t -> Testrunner.Result.t Lwt.t) -> unit

val load :
  print:((string -> 'a, unit, string, 'a) format4 -> string -> 'a) ->
    string -> unit

val options : (Arg.key * Arg.spec * Arg.doc) list ref

val add_option : (Arg.key * Arg.spec * Arg.doc) -> unit
