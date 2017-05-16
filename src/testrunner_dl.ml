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

open Testrunner

let () = Dynlink.allow_unsafe_modules true

let handlers = ref (SMap.empty : (Env.t -> Tree.result) SMap.t)
let lwt_handlers = ref (SMap.empty : (Env.t -> Tree.result Lwt.t) SMap.t)

let add_handler name f =
  handlers := SMap.add name f !handlers

let add_lwt_handler name f =
  lwt_handlers := SMap.add name f !lwt_handlers

let load ~(print:('b, unit, string, 'a) Pervasives.format4 -> 'b) file =
  let file =
    try Dynlink.adapt_filename file
    with Invalid_argument _ -> file
  in
  print "Loading %S... " file ;
  try
    Dynlink.loadfile file;
    print "%s" "ok\n"; ()
  with Dynlink.Error e -> failwith (Dynlink.error_message e)

let handlers () = !handlers
let lwt_handlers () = !lwt_handlers

let options = ref []

let add_option o = options := !options @ [o]

let initializers = ref []
let add_initializer f = initializers := f :: !initializers
let initializers () = List.rev !initializers
