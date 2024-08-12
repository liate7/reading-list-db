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
    val to_string : t -> string
  end

  module Tag : sig
    type t = { name : string; description : string }
    type id = private int

    val id_of_int : int -> id
  end
end

open Data

val init_entries :
  (module Caqti_lwt.CONNECTION) ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val init_tags :
  (module Caqti_lwt.CONNECTION) ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val init_tag_entries :
  (module Caqti_lwt.CONNECTION) ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val create_entry :
  url:string ->
  title:string ->
  (module Caqti_lwt.CONNECTION) ->
  (Entry.id, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val create_tag :
  name:string ->
  descr:string ->
  (module Caqti_lwt.CONNECTION) ->
  (Tag.id, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val tag_entry :
  Entry.id ->
  Tag.id ->
  string option ->
  (module Caqti_lwt.CONNECTION) ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val tag_entry' :
  Entry.id ->
  Tag.id list ->
  (module Caqti_lwt.CONNECTION) ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val select_all_entries :
  (module Caqti_lwt.CONNECTION) ->
  ((Entry.id * Entry.t) list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val select_filtered_entries :
  ?states:Entry.state list ->
  ?search:string ->
  ?tags:string list ->
  (module Caqti_lwt.CONNECTION) ->
  ((Entry.id * Entry.t) list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val select_all_tags :
  (module Caqti_lwt.CONNECTION) ->
  ((Tag.id * Tag.t) list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val select_tags_by_name :
  string list ->
  (module Caqti_lwt.CONNECTION) ->
  ((Tag.id * Tag.t) list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val entry_by_id :
  Entry.id ->
  (module Caqti_lwt.CONNECTION) ->
  (Entry.t, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val tag_by_id :
  Tag.id ->
  (module Caqti_lwt.CONNECTION) ->
  (Tag.t, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val tag_by_name :
  string ->
  (module Caqti_lwt.CONNECTION) ->
  ((Tag.id * Tag.t) option, [> Caqti_error.call_or_retrieve ]) result Lwt.t
