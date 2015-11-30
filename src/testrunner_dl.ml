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

let handlers = ref (SMap.empty : (Env.t -> Result.t) SMap.t)

let add_handler name f =
  handlers := SMap.add name f !handlers

let load ~(print:('b, unit, string, 'a) Pervasives.format4 -> 'b) file =
  handlers := SMap.empty ;
  let file = Dynlink.adapt_filename file in
  print "Loading %S... " file ;
  try
    Dynlink.loadfile file;
    print "%s" "ok\n";
    !handlers
  with Dynlink.Error e -> failwith (Dynlink.error_message e)
