open! ContainersLabels
open Reading_list
open! Lwt.Infix

let ( let* ) = Lwt.bind

let ( let*! ) vow body =
  let* res = vow in
  match res with Ok v -> body v | Error e -> Lwt.return @@ Error e

let () =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  Lwt_main.run
    (( Caqti_lwt_unix.with_connection
         ("sqlite3:" ^ Sys.argv.(1) |> Uri.of_string)
     @@ fun ((module Conn : Caqti_lwt.CONNECTION) as conn) ->
       Conn.with_transaction @@ fun () ->
       let*! things =
         Conn.collect_list
           ((unit ->* t3 octets string octets)
              "select hash, mime, content from cas")
           ()
       in
       let*! () = Conn.exec ((unit ->. unit) "delete from cas") () in
       let rec loop = function
         | (hash, t, blob) :: rest ->
             let*! hash' =
               Cas.write
                 (Cas.filetype_of_string t |> Option.get_exn_or "wtf")
                 blob conn
             in
             let*! () =
               Conn.exec
                 ((t2 octets octets ->. unit)
                    "update entry set url = '/local/' || ? where url LIKE '%' \
                     || ? || '%'")
                 ((hash' :> string), hash)
             in
             loop rest
         | [] -> Lwt.return @@ Result.return ()
       in
       loop things )
    >>= Caqti_lwt.or_fail)
