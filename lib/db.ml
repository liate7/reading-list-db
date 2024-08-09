module To_read = struct
  type state = To_read | Reading | Read

  let state_to_string = function
    | To_read -> "to-read"
    | Reading -> "reading"
    | Read -> "read"

  type t = {
    (* url : Uri.t; *)
    url : string;
    title : string;
    state : state;
    created_at : float;
  }

  let to_string { url; title; state; created_at = _ } =
    [%string "[[%{url}][%{title}]]: %{state_to_string state}"]
end

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let state =
    custom string
      ~encode:(function
        | To_read.To_read -> Ok "to-read"
        | To_read.Reading -> Ok "reading"
        | To_read.Read -> Ok "read")
      ~decode:(function
        | "to-read" -> Ok To_read.To_read
        | "reading" -> Ok To_read.Reading
        | "read" -> Ok To_read.Read
        | s -> Error ("Invalid state: " ^ s))

  let to_read =
    let open To_read in
    let intro url title state created_at = { url; title; state; created_at } in
    product intro
    @@ proj string (fun t -> t.url)
    @@ proj string (fun t -> t.title)
    @@ proj state (fun t -> t.state)
    @@ proj float (fun t -> t.created_at)
    @@ proj_end

  let init_entries =
    (unit ->. unit)
    @@ {|
    create table if not exists entry (
      id integer primary key asc not null,
      url text not null,
      title text not null,
      state text not null
        default 'to-read'
        check (state in ('to-read', 'reading', 'read')),
      created_at integer not null
    )
    |}

  let init_tags =
    (unit ->. unit)
    @@ {|
    create table if not exists tag (
      id integer primary key not null,
      name text unique not null,
      description text not null default ''
    )
    |}

  let init_tag_entries =
    (unit ->. unit)
    @@ {|
    create table if not exists tag_entry (
      entry integer references entry(id) not null,
      tag integer references tag(id) not null,
      payload text
    )
    |}

  let create_entry =
    (t2 string string ->! int)
    @@ {|
    insert into entry (url, title, created_at)
      values (?, ?, unixepoch('now'))
      returning id
    |}

  let create_tag =
    (t2 string string ->! int)
    @@ "insert into tag (name, description) values (?, ?) returning id"

  let tag_entry =
    (t3 int int (option string) ->. unit)
    @@ "insert into tag_entry (entry, tag, payload) values (?, ?, ?)"

  let select_all_entries =
    (unit ->* t3 int to_read string)
    @@ {|
    select entry.id,
           entry.url,
           entry.title,
           entry.state,
           entry.created_at,
           json_group_array(tag.name)
      from entry, tag, tag_entry
      where tag.id = tag_entry.tag and tag_entry.entry = entry.id
    |}

  let select_filtered_entries =
    (t3 (option state) (option string) (option string) ->* t3 int to_read string)
    @@ {|
    select entry.id,
           entry.url,
           entry.title,
           entry.state,
           entry.created_at,
           json_group_array(tag.name)
      from entry, tag, tag_entry
      where tag.id = tag_entry.tag and tag_entry.entry = entry.id
        and ($1 is null or entry.state = $1)
        and ($2 is null or entry.title like '%' || $2 || '%')
        and ($3 is null or exists (
          select tag.id from tag, tag_entry
            where tag.name = $3
              and tag.id = tag_entry.tag and tag_entry.entry = entry.id
      ))
    |}

  let select_all_tags =
    (unit ->* t3 int string string) @@ "select id, name, description from tag"
end

let init_entries (module Conn : Caqti_lwt.CONNECTION) =
  Conn.exec Q.init_entries ()

let init_tags (module Conn : Caqti_lwt.CONNECTION) = Conn.exec Q.init_tags ()

let init_tag_entries (module Conn : Caqti_lwt.CONNECTION) =
  Conn.exec Q.init_tag_entries ()

let create_entry ~url ~title (module Conn : Caqti_lwt.CONNECTION) =
  Conn.find Q.create_entry (url, title)

let create_tag tag (module Conn : Caqti_lwt.CONNECTION) =
  Conn.find Q.create_tag tag

let tag_entry entry_id tag_id payload (module Conn : Caqti_lwt.CONNECTION) =
  Conn.exec Q.tag_entry (entry_id, tag_id, payload)

let select_all_entries (module Conn : Caqti_lwt.CONNECTION) =
  Conn.collect_list Q.select_all_entries ()

let select_filtered_entries ?state ?search ?tag
    (module Conn : Caqti_lwt.CONNECTION) =
  Conn.collect_list Q.select_filtered_entries (state, search, tag)

let select_all_tags (module Conn : Caqti_lwt.CONNECTION) =
  Conn.collect_list Q.select_all_tags ()
