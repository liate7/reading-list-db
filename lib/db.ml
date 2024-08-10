open! ContainersLabels

module Data = struct
  module Entry = struct
    type state = To_read | Reading | Read

    let state_to_string = function
      | To_read -> "to-read"
      | Reading -> "reading"
      | Read -> "read"

    type t = {
      url : Uri.t;
      title : string;
      state : state;
      created_at : Ptime.t;
      tags : (string * string option) list;
    }

    type id = int

    let id_of_int = Fun.id

    let tags_to_yojson tags =
      `Assoc
        (List.map
           ~f:(function
             | key, Some value -> (key, `String value)
             | key, None -> (key, `Null))
           tags)

    let tags_of_yojson json =
      Yojson.Safe.Util.to_assoc json
      |> List.map ~f:(fun (key, value) ->
             (key, Yojson.Safe.Util.to_string_option value))

    let to_string { url; title; state; created_at = _; tags } =
      let tags_str =
        tags
        |> List.map ~f:(function
             | key, Some value -> [%string "%{key}: %{value}"]
             | key, None -> key)
        |> String.concat ~sep:", "
      in
      let tags_str = match tags_str with "" -> "" | s -> ": " ^ s in
      [%string
        "[[%{Uri.to_string url}][%{title}]](%{state_to_string \
         state})%{tags_str}"]
  end

  module Tag = struct
    type t = { name : string; description : string }
    type id = int

    let id_of_int = Fun.id
  end
end

open Data

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let url =
    custom string
      ~encode:Fun.(Uri.to_string %> Result.return)
      ~decode:Fun.(Uri.of_string %> Result.return)

  let state =
    custom string
      ~encode:Fun.(Entry.state_to_string %> Result.return)
      ~decode:(function
        | "to-read" -> Ok Entry.To_read
        | "reading" -> Ok Entry.Reading
        | "read" -> Ok Entry.Read
        | s -> Error ("Invalid state: " ^ s))

  let tags =
    custom string
      ~encode:
        Fun.(Entry.tags_to_yojson %> Yojson.Safe.to_string %> Result.return)
      ~decode:(fun tags ->
        try
          Yojson.Safe.from_string tags |> Entry.tags_of_yojson |> Result.return
        with Yojson.Json_error str -> Error str)

  let to_read =
    let open Entry in
    let intro url title state created_at tags =
      { url; title; state; created_at; tags }
    in
    product intro
    @@ proj url (fun t -> t.url)
    @@ proj string (fun t -> t.title)
    @@ proj state (fun t -> t.state)
    @@ proj ptime (fun t -> t.created_at)
    @@ proj tags (fun t -> t.tags)
    @@ proj_end

  let tag =
    let open Tag in
    let intro name descr = { name; description = descr } in
    product intro
    @@ proj string (fun t -> t.name)
    @@ proj string (fun t -> t.description)
    @@ proj_end

  let init_entries = (unit ->. unit) [%blob "resources/init_entries.sql"]
  let init_tags = (unit ->. unit) [%blob "resources/init_tags.sql"]

  let init_tag_entries =
    (unit ->. unit) [%blob "resources/init_tag_entries.sql"]

  let create_entry =
    (t2 string string ->! int) [%blob "resources/create_entry.sql"]

  let create_tag = (t2 string string ->! int) [%blob "resources/create_tag.sql"]

  let tag_entry =
    (t3 int int (option string) ->. unit)
      "insert or replace into tag_entry (entry, tag, payload) values (?, ?, ?)"

  let select_all_entries =
    (unit ->* t2 int to_read) [%blob "resources/select_all_entries.sql"]

  let select_filtered_entries =
    (t3 (option state) (option string) (option string) ->* t2 int to_read)
      [%blob "resources/select_filtered_entries.sql"]

  let select_all_tags =
    (unit ->* t2 int tag) "select id, name, description from tag"

  let entry_by_id = (int ->! to_read) [%blob "resources/entry_by_id.sql"]

  let tag_by_id =
    (int ->! tag) "select name, description from tag where tag.id = ?"
end

let init_entries (module Conn : Caqti_lwt.CONNECTION) =
  Conn.exec Q.init_entries ()

let init_tags (module Conn : Caqti_lwt.CONNECTION) = Conn.exec Q.init_tags ()

let init_tag_entries (module Conn : Caqti_lwt.CONNECTION) =
  Conn.exec Q.init_tag_entries ()

let create_entry ~url ~title (module Conn : Caqti_lwt.CONNECTION) =
  Conn.find Q.create_entry (url, title)

let create_tag ~name ~descr (module Conn : Caqti_lwt.CONNECTION) =
  Conn.find Q.create_tag (name, descr)

let tag_entry entry_id tag_id payload (module Conn : Caqti_lwt.CONNECTION) =
  Conn.exec Q.tag_entry (entry_id, tag_id, payload)

let select_all_entries (module Conn : Caqti_lwt.CONNECTION) =
  Conn.collect_list Q.select_all_entries ()

let select_filtered_entries ?state ?search ?tag
    (module Conn : Caqti_lwt.CONNECTION) =
  Conn.collect_list Q.select_filtered_entries (state, search, tag)

let select_all_tags (module Conn : Caqti_lwt.CONNECTION) =
  Conn.collect_list Q.select_all_tags ()

let entry_by_id id (module Conn : Caqti_lwt.CONNECTION) =
  Conn.find Q.entry_by_id id

let tag_by_id id (module Conn : Caqti_lwt.CONNECTION) = Conn.find Q.tag_by_id id
