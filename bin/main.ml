open Lwt.Infix
open Reading_list

let main =
  let ( let* ) :
        'a 'b 'err.
        ('a, 'err) result Lwt.t ->
        ('a -> ('b, 'err) result Lwt.t) ->
        ('b, 'err) result Lwt.t =
   fun vow f ->
    vow >>= function Ok value -> f value | Error err -> Lwt.return (Error err)
  and ( let+ ) vow f =
    vow >|= function Ok value -> Ok (f value) | Error err -> Error err
  in
  Caqti_lwt_unix.with_connection (Uri.of_string "sqlite3:foo.db?create=true")
  @@ fun db ->
  let* () = Db.init_entries db in
  let* () = Db.init_tags db in
  let* () = Db.init_tag_entries db in
  let* entry_id =
    Db.create_entry ~url:"https://example.com" ~title:"Example" db
  in
  let* tag_id = Db.create_tag ("example", "") db in
  let* () = Db.tag_entry entry_id tag_id None db in
  let+ entries = Db.select_all_entries db in
  entries
  |> List.iter (fun (id, entry, tags) ->
         Printf.printf "%i: %s %s\n" id (Db.To_read.to_string entry) tags)

let () = Lwt_main.run (main >>= Caqti_lwt.or_fail)
