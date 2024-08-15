open ContainersLabels

type filetype = Pdf
type hash = string

let hash_of_string = Fun.id
let filetype_to_string = function Pdf -> "application/pdf"
let filetype_of_string = function "application/pdf" -> Some Pdf | _ -> None

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let hash = custom octets ~encode:Result.return ~decode:Result.return

  let filetype =
    enum ~encode:filetype_to_string
      ~decode:Fun.(filetype_of_string %> Option.to_result "unknown filetype")
      "filetype"

  let cas_store =
    (t3 hash filetype octets ->! hash)
      "insert or ignore into cas (hash, mime, content) values (?, ?, ?) \
       returning hash"

  let cas_get =
    (hash ->? t2 filetype octets) "select mime, content from cas where hash = ?"
end

let ( let* ) = Lwt.bind
let ( let+ ) vow f = Lwt.map f vow

let write filetype contents (module Conn : Caqti_lwt.CONNECTION) =
  let hash : hash =
    Digestif.(digest_string sha3_256 contents |> to_raw_string sha3_256)
  in
  let+ res = Conn.find Q.cas_store (hash, filetype, contents) in
  Result.(res >|= Dream.to_base64url)

let get hash (module Conn : Caqti_lwt.CONNECTION) =
  match Dream.from_base64url hash with
  | Some hash -> Conn.find_opt Q.cas_get hash
  | None -> Lwt.return @@ Error `Not_a_base64

let loader _root path req =
  let hash =
    Stringext.trim_left_sub path ~pos:0 ~len:(String.length path) ~chars:"/"
  in
  let* blob = Dream.sql req @@ get hash in
  match blob with
  | Ok None -> Dream.empty `Not_Found
  | Error _ -> Dream.empty `Internal_Server_Error
  | Ok (Some (filetype, hash)) ->
      Dream.respond
        ~headers:
          [
            ("content-type", filetype_to_string filetype);
            ("content-disposition", "inline");
          ]
        hash
