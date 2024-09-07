open! ContainersLabels
open! Reading_list
open! Lwt.Infix

let run_with_db uri f : unit =
  Lwt_main.run (Caqti_lwt_unix.with_connection uri f >>= Caqti_lwt.or_fail)

(* Goal: ~add-entry db url-or-file title tags...~
   to add an entry into ~db~ *)

let _ = run_with_db
