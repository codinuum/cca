(*
   Copyright 2012-2017 Codinuum Software Lab <http://codinuum.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(* *)

module Otree = Otreediff.Otree
module UID   = Otreediff.UID
module GI    = Otreediff.GIndex
module Path  = Otreediff.Path


module OutChannel = struct

  type t = 
    | Pervasives of out_channel 
    | XChannel of Xchannel.out_channel
          
  let of_pervasives ch = Pervasives ch
  let of_xchannel ch = XChannel ch

  let is_stdout = function
    | Pervasives ch -> ch = Pervasives.stdout
    | _ -> false

  let to_pervasives = function
    | Pervasives ch -> ch
    | _ -> failwith "OutChannel.to_pervasives"

  let to_xchannel = function
    | XChannel ch -> ch
    | _ -> failwith "OutChannel.to_xchannel"

  let close = function
    | Pervasives ch -> Pervasives.close_out ch
    | XChannel ch -> ch#close

end


class type node_data_t_shared = object ('self)
  inherit Otree.data2

  method source_fid       : string
  method set_source_fid   : string -> unit

  method _digest      : Xhash.t option
  method digest       : Xhash.t option
  method set_digest   : Xhash.t -> unit
  method _set_digest  : Xhash.t -> unit
  method reset_digest : unit

  method label           : string
  method _label          : Obj.t
  method relabel_allowed : 'self -> bool
  method to_be_notified  : bool
  
  method eq             : 'self -> bool (* label *)
  method equals         : 'self -> bool (* label and digest *)
  method subtree_equals : 'self -> bool (* label and _digest (subtree) *)

  method set_loc : Loc.t -> unit
  method src_loc : Loc.t

  method to_rep    : string
  method to_string : string

  method feature : Obj.t * Xhash.t option

  method to_elem_data : string * (string * string) list * string

  method is_anonymous : bool

  method is_named           : bool   
  method is_named_orig      : bool   

  method get_name           : string 

  method _anonymized_label  : Obj.t  
  method _anonymized2_label : Obj.t  
  method _anonymized3_label : Obj.t  

  method is_partition       : bool   
  method is_boundary        : bool   

  method binding     : Binding.t
  method bindings    : Binding.t list


end (* of class type node_data_t_shared *)



type 'data node_t = (#node_data_t_shared as 'data) Otree.node2



class type [ 'node ] tree_t_shared = object ('self)
  inherit [ 'node ] Otree.otree2

  method parser_name               : string
  method get_digest                : 'node -> Xhash.t
  method set_source_path           : string -> unit      
  method source_path               : string
  method set_ignored_regions       : (int * int) list -> unit
  method ignored_regions           : (int * int) list
  method get_units_to_be_notified  : 'node list
  method make_subtree_from_uid     : UID.t -> 'self
  method make_subtree_from_node    : 'node -> 'self
  method make_subtree_from_path    : Path.t -> 'self
  method make_subtree_copy         : ?find_hook:('node -> 'node -> unit) -> 'node -> 'self
  method unparse_ch                : OutChannel.t -> unit
  method extra_namespaces          : (string * string) list

end (* of class type tree_t_shared *)


class extension = object (self)
  val mutable node = (None : extension Pxp_document.node option)
  method clone = {<>}
  method node =
    match node with
    | Some nd -> nd
    | None -> assert false
  method set_node nd = node <- Some nd
end

type xnode_t = extension Pxp_document.node



class type ['tree] tree_factory_t = object
  method namespace_manager : Pxp_dtd.namespace_manager
  method from_xnode        : xnode_t      -> 'tree
  method from_file         : Storage.file -> 'tree
end

