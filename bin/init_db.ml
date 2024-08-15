open! ContainersLabels
open Reading_list
open Lwt.Infix

let () =
  Lwt_main.run
  @@ (Caqti_lwt_unix.with_connection
        ("sqlite3:" ^ Sys.argv.(1) |> Uri.of_string)
        Db.init_db
     >>= Caqti_lwt.or_fail)
