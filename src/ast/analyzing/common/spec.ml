(*
   Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>

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

  method set_prefix : string -> unit
  method get_prefix : string

  method set_suffix : string -> unit
  method get_suffix : string

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
  method has_value    : bool
  method has_non_trivial_value : bool
  method is_string_literal : bool
  method is_int_literal : bool
  method is_real_literal : bool

  method move_disallowed : bool
  method is_common       : bool

  method is_order_insensitive : bool

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

  method unparse_subtree_ch       : ?fail_on_error:bool -> node_t -> SB.OutChannel.t -> unit
  method unparse_ch               : ?fail_on_error:bool -> SB.OutChannel.t -> unit

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

(*
  method set_line_terminator      : string -> unit
  method line_terminator          : string
  method line_terminator_name     : string
*)

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





class type uidmapping_t = object ('self)

  method search_node_by_uid1 : UID.t -> node_t
  method search_node_by_uid2 : UID.t -> node_t

  method use_crossing_or_incompatible_matches_count_cache : bool

  method clear_crossing_or_incompatible_matches_count_cache : unit
  method crossing_or_incompatible_matches_count_cache_hit_count : int
  method size_of_crossing_or_incompatible_matches_count_cache : int

  method clear_starting_uid_pairs_for_glueing : unit
  method set_starting_uid_pairs_for_glueing   : ((UID.t * UID.t) list) -> unit
  method add_starting_uid_pairs_for_glueing   : ((UID.t * UID.t) list) -> unit
  method starting_uid_pairs_for_glueing       : ((UID.t * UID.t) list)


  method is_locked_uid     : UID.t -> bool
  method lock_uid          : ?key:Key.t -> UID.t -> unit
  method unlock_uid        : UID.t -> unit
  method key_of_locked_uid : UID.t -> Key.t

  method stable_pairs     : (UID.t, UID.t) Hashtbl.t
  method set_stable_pairs : (UID.t, UID.t) Hashtbl.t -> unit
  method is_stable_pair   : UID.t -> UID.t -> bool
  method add_stable_pair  : UID.t -> UID.t -> unit
  method find_stable_pair : UID.t -> UID.t list
  method iter_stable_pairs : (UID.t -> UID.t -> unit) -> unit

  method set_blacklist1 : (UID.t, bool) Hashtbl.t -> unit
  method set_blacklist2 : (UID.t, bool) Hashtbl.t -> unit

  method size : int

  method iter           : (UID.t -> UID.t -> unit) -> unit
  method iter_rev       : (UID.t -> UID.t -> unit) -> unit
  method iter_unsettled : (UID.t -> UID.t -> unit) -> unit
  method iter_settled   : (UID.t -> UID.t -> unit) -> unit

  method iter_sorted           : (UID.t -> UID.t -> int) -> (UID.t -> UID.t -> unit) -> unit
  method iter_unsettled_sorted : (UID.t -> UID.t -> int) -> (UID.t -> UID.t -> unit) -> unit
  method iter_settled_sorted   : (UID.t -> UID.t -> int) -> (UID.t -> UID.t -> unit) -> unit


  method find_unsettled : UID.t -> UID.t
  method find_settled   : UID.t -> UID.t
  method find           : UID.t -> UID.t
  method inv_find       : UID.t -> UID.t

  method add_unsettled     : UID.t -> UID.t -> unit
  method add_settled       : ?stable:bool -> UID.t -> UID.t -> unit
  method add_settled_roots    : UID.t -> UID.t -> unit
  method is_settled_root_pair : UID.t -> UID.t -> bool

  method iter_settled_roots : (UID.t -> UID.t -> unit) -> unit
  method iter_settled_roots_sorted : (UID.t -> UID.t -> int) -> (UID.t -> UID.t -> unit) -> unit

  method merge_no_override : 'self -> unit
  method merge             : 'self -> unit
  method merge_checked     : 'self -> unit

  method mem           : UID.t -> bool
  method mem_unsettled : UID.t -> bool
  method mem_settled   : UID.t -> bool

  method remove         : UID.t -> UID.t -> unit
  method remove_settled : UID.t -> UID.t -> unit

  method filter : (UID.t -> UID.t -> bool) -> unit

  method promote : UID.t -> UID.t -> unit
  method demote  : UID.t -> UID.t -> unit

  method to_list_unsettled : (UID.t * UID.t) list
  method to_list_settled   : (UID.t * UID.t) list
  method to_list           : (UID.t * UID.t) list

  method cod : UID.t list
  method dom : UID.t list

  method cod_unsettled : UID.t list
  method dom_unsettled : UID.t list

  method cod_settled : UID.t list
  method dom_settled : UID.t list

  method has_settled_mapping   : UID.t -> UID.t -> bool
  method has_unsettled_mapping : UID.t -> UID.t -> bool
  method has_mapping           : UID.t -> UID.t -> bool

  method to_string_gid : string
  method to_string     : string

  method dump : string -> unit

  method dump_with_info    : ?comp:Compression.c -> string -> unit

  method print_status : unit

  method setup_rev_map : unit

  method mem_dom : UID.t -> bool
  method mem_cod : UID.t -> bool

  method mem_dom_settled : UID.t -> bool
  method mem_cod_settled : UID.t -> bool

  method mem_dom_unsettled : UID.t -> bool
  method mem_cod_unsettled : UID.t -> bool

  method cleanup_ghost : unit

  method count_p_mapping : (node_t -> node_t -> node_t -> node_t -> bool) -> node_t -> node_t -> int
  method count_crossing_mapping                 : node_t -> node_t -> int
  method count_crossing_matches                 : node_t -> node_t -> int
  method count_crossing_or_incompatible_matches : node_t -> node_t -> int
  method count_compatible_noncrossing_matches   : node_t -> node_t -> int

  method iter_p_mapping : (node_t -> node_t -> node_t -> node_t -> bool) -> node_t -> node_t -> (UID.t -> UID.t -> unit) -> unit
  method iter_crossing_mapping                  : node_t -> node_t -> (UID.t -> UID.t -> unit) -> unit
  method iter_crossing_or_incompatible_mapping  : node_t -> node_t -> (UID.t -> UID.t -> unit) -> unit

  method setup_partitions : unit
  method partition_nodes1 : node_t list -> node_t list array
  method partition_nodes2 : node_t list -> node_t list array

  method get_subtree_match_score : node_t -> node_t -> int

end (* of class type uidmapping_t *)



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
  val is_compatible            : t -> t -> bool
  val is_order_insensitive     : t -> bool
  val move_disallowed          : t -> bool
  val is_common                : t -> bool
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
  val has_value          : t -> bool
  val has_non_trivial_value : t -> bool

  val cannot_be_keyroot  : node_t -> bool

  val is_phantom         : t -> bool
  val is_special         : t -> bool

  val is_string_literal  : t -> bool
  val is_int_literal     : t -> bool
  val is_real_literal    : t -> bool

  val to_tag             : t -> string * (string * string) list

end (* of module type LABEL_T *)

