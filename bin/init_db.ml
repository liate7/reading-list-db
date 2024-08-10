open! ContainersLabels
open Reading_list
open Lwt.Infix

let init_dream_sessions =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  let request =
    (unit ->. unit)
      {|
    create table if not exists dream_session (
      id text primary key,
      label text not null,
      expires_at real not null,
      payload text not null
    )
    |}
  in
  fun (module Conn : Caqti_lwt.CONNECTION) -> Conn.exec request ()

let ( let* ) vow f =
  vow >>= function Ok value -> f value | Error err -> Lwt.return (Error err)

let () =
  Lwt_main.run
  @@ (Caqti_lwt_unix.with_connection
        ("sqlite3:" ^ Sys.argv.(1) |> Uri.of_string)
        (fun conn ->
          let* () = init_dream_sessions conn in
          let* () = Db.init_entries conn in
          let* () = Db.init_tags conn in
          Db.init_tag_entries conn)
     >>= Caqti_lwt.or_fail)
