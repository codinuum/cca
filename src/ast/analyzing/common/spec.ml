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
module SB    = Spec_base



class type node_data_t = object ('self)
  inherit SB.node_data_t_shared

  method gid     : GI.t
  method set_gid : GI.t -> unit

  method origin     : string
  method set_origin : string -> unit
  method ending     : string
  method set_ending : string -> unit

  method frommacro     : string
  method is_frommacro  : bool
  method not_frommacro : bool
  method set_frommacro : string -> unit

  method _annotation     : Obj.t

  method is_sequence     : bool
  method is_phantom      : bool
  method is_special      : bool

  method anonymized_label  : string
  method anonymized2_label : string
  method anonymized3_label : string

  method to_simple_string  : string

  method is_anonymous_orig : bool

  method get_category : string
  method get_value    : string

  method get_ident_use : string

  method orig_lab_opt  : Obj.t option

  method set_binding : Binding.t -> unit
  method add_binding : Binding.t -> unit

  method successors    : ('self Otree.node2) Xset.t
  method add_successor : ('self Otree.node2) -> unit

  method get_ordinal : int -> int
  method add_to_ordinal_list : int list -> unit

  (* for searchast *)
  method char              : char
  method to_short_string   : string

end (* of class type node_data_t *)


type node_t = node_data_t SB.node_t


class type tree_t = object ('self)
  inherit [ node_t ] SB.tree_t_shared

  method unparse_subtree_ch       : node_t -> SB.OutChannel.t -> unit
  method unparse_ch               : SB.OutChannel.t -> unit

  method set_true_parent_tbl      : (UID.t, node_t) Hashtbl.t -> unit
  method find_true_parent         : UID.t -> node_t

  method set_true_children_tbl    : (node_t, node_t array) Hashtbl.t -> unit
  method recover_true_children    : initial_only:bool -> unit -> unit

  method set_source_info          : Storage.file -> unit


  method set_source_fullpath      : string -> unit
  method source_fullpath          : string
  method source_kind              : Storage.kind
  method set_source_kind          : Storage.kind -> unit
  method set_vkind                : Entity.vkind -> unit
  method vkind                    : Entity.vkind
  method set_version              : string -> unit
  method version                  : string
  method set_proj_root            : string -> unit
  method proj_root                : string
  method set_source_digest        : string -> unit
  method source_digest            : string
  method encoded_source_digest    : string
  method set_parser_name          : string -> unit

  method dump_astml               : ?comp:Compression.c -> string -> unit

  method collapse                 : unit

  method dump_line_ranges         : string -> unit



  method set_misparsed_regions    : (int * int) list -> unit
  method misparsed_regions        : (int * int) list
  method set_total_LOC            : int -> unit
  method total_LOC                : int
  method set_misparsed_LOC        : int -> unit
  method misparsed_LOC            : int

  method get_nearest_containing_unit      : UID.t -> node_t
  method label_match                      : string -> bool
  method dump_origin                      : int ->    (* bufsize *)
                                            string -> (* nctms file *)
                                            int ->    (* rev index *)
                                            string -> (* origin file *)
                                            string -> (* ending file *)
                                            int *                             (* size *)
                                            int *                             (* num of known origns *)
                                            float *                           (* coverage *)
                                            (node_t, node_t list) Hashtbl.t * 
                                            int *                             (* num of known endings *)
                                            float *                           (* coverage *)
                                            (node_t, node_t list) Hashtbl.t

  method align_fragments                  : (GI.t * GI.t) list -> 'self -> unit


  method nearest_common_ancestor          : ?closed:bool -> node_t list -> node_t * int
  method get_nearest_boundary             : node_t -> node_t

  method find_nodes_by_line_range         : (int * int) -> node_t list
  method find_nodes_by_line_col_range     : ((int * int) * (int * int)) -> node_t list

  method get_ident_use_list               : GI.t -> string list

  method merge_locs_adjusting_to_boundary : GI.t list -> Loc.t

  method subtree_to_simple_string         : GI.t -> string

  method make_anonymized_subtree_copy     : ?uids_left_named:(UID.t list) -> node_t -> 'self
  method make_anonymized2_subtree_copy    : ?uids_left_named:(UID.t list) -> node_t -> 'self

  method find_label                       : node_t -> node_t -> node_t list


  (* for searchast *)

  method find_token_node          : int -> node_t
  method find_token_node_pat      : int -> node_t
  method to_token_array           : string array
  method get_token_array_pat      : GIDfragment.c -> string array
  method match_pat_ch             : string -> 'self -> string array -> GIDfragment.c -> out_channel -> unit
  method match_token_array_pat_ch : string -> string -> string -> string array -> out_channel -> unit
  method match_pats               : string -> string -> 'self -> GIDfragment.c list -> unit
  method show_node_array_pat      : unit


end (* of class type tree_t *)


module type LABEL_T = sig

  type annotation

  val null_annotation      : annotation
  val annotation_to_string : annotation -> string

  type t

  val lang_prefix : string

  val to_string        : t -> string
  val to_char          : t -> char
  val to_short_string  : ?ignore_identifiers_flag:bool -> t -> string
  val to_simple_string : t -> string

  val to_elem_data     : ?strip:bool -> Loc.t -> t -> string * (string * string) list * string
  val of_elem_data     : string -> (string * string) list -> string -> t

  val relabel_allowed          : t * t -> bool
  val is_to_be_notified        : t -> bool
  val is_collapse_target       : #Parser_options.c -> t -> bool
  val is_hunk_boundary         : t -> t -> bool
  val forced_to_be_collapsible : t -> bool
  val is_named                 : t -> bool
  val is_named_orig            : t -> bool
  val keyroot_depth_min        : int

  val is_boundary        : t -> bool
  val is_partition       : t -> bool
  val is_sequence        : t -> bool

  val anonymize          : ?more:bool -> t -> t
  val anonymize2         : t -> t
  val anonymize3         : t -> t
  val get_ident_use      : t -> string

  val get_category       : t -> string
  val get_name           : t -> string
  val get_value          : t -> string

  val cannot_be_keyroot  : node_t -> bool

  val is_phantom         : t -> bool
  val is_special         : t -> bool

  val is_string_literal  : t -> bool
  val is_int_literal     : t -> bool
  val is_real_literal    : t -> bool

  val to_tag             : t -> string * (string * string) list

end (* of module type LABEL_T *)

