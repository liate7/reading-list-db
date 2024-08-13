module Data : sig
  module Entry : sig
    type state = To_read | Reading | Read

    type t = {
      url : Uri.t;
      title : string;
      state : state;
      created_at : Ptime.t;
      tags : (string * string option) list;
    }

    type id = private int

    val id_of_int : int -> id
    val state_to_string : state -> string
    val state_of_string : string -> state option
    val to_string : t -> string
  end

  module Tag : sig
    type t = { name : string; description : string }
    type id = private int

    val id_of_int : int -> id
  end
end

open Data

type ('a, 'err) query =
  (module Caqti_lwt.CONNECTION) ->
  ('a, ([> Caqti_error.call_or_retrieve ] as 'err)) result Lwt.t

val init_entries : (unit, 'err) query
val init_tags : (unit, 'err) query
val init_tag_entries : (unit, 'err) query
val create_entry : url:string -> title:string -> (Entry.id, 'err) query
val create_tag : name:string -> descr:string -> (Tag.id, 'err) query
val tag_entry : Entry.id -> Tag.id -> string option -> (unit, 'err) query
val tag_entry' : Entry.id -> Tag.id list -> (unit, 'err) query
val select_all_entries : ((Entry.id * Entry.t) list, 'err) query

val select_filtered_entries :
  ?states:Entry.state list ->
  ?search:string ->
  ?tags:string list ->
  ((Entry.id * Entry.t) list, 'err) query

val select_all_tags : ((Tag.id * Tag.t) list, 'err) query
val select_tags_by_name : string list -> ((Tag.id * Tag.t) list, 'err) query
val entry_by_id : Entry.id -> (Entry.t, 'err) query
val tag_by_id : Tag.id -> (Tag.t, 'err) query
val tag_by_name : string -> ((Tag.id * Tag.t) option, 'err) query
val start_reading : Entry.id -> (unit, 'err) query
val restart_reading : Entry.id -> (unit, 'err) query
val finish_reading : Entry.id -> (unit, 'err) query
