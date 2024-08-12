open Dream_html
open! ContainersLabels

let ( let* ) = Lwt.bind
let ( let+ ) vow f = Lwt.map f vow
let app_name = "Reading list"

let htmx_script =
  HTML.(
    script
      [
        src "https://unpkg.com/htmx.org@2.0.1";
        integrity
          "sha384-QWGpdj554B4ETpJJC9z+ZHJcA/i59TyjxEPXiiUgN2WmTyV5OEZWCD6gQhgkdpB/";
        crossorigin `anonymous;
      ]
      "")

let wrap_page vow_fn template req =
  let* res = vow_fn req in
  let token = Dream.csrf_token req in
  match res with
  | Ok entries -> entries |> template token |> Dream_html.respond
  | Error err ->
      Dream.log "%a@." Caqti_error.pp err;
      Dream.empty `Internal_Server_Error

let get_forms req =
  match Dream.header req "X-CSRF-Token" with
  | None -> Lwt.return (Error `No_CSRF_token)
  | Some tok -> (
      let* state = Dream.verify_csrf_token req tok in
      match state with
      | `Invalid -> Lwt.return (Error (`Invalid_CSRF_token tok))
      | `Expired _ | `Wrong_session | `Ok -> (
          let+ form_res = Dream.form ~csrf:false req in
          match form_res with
          | `Ok form -> Ok form
          | err -> Error (`Form_response err)))

let wrap_post_response vow_fn template req =
  let* res = get_forms req in
  match res with
  | Ok form -> (
      let* res = vow_fn form req in
      match res with
      | Ok entries -> entries |> template form |> Dream_html.respond
      | Error err ->
          Dream.log "%a@." Caqti_error.pp err;
          Dream.empty `Internal_Server_Error)
  | Error `No_CSRF_token ->
      Dream.log "CSRF token missing";
      Dream.empty `Bad_Request
  | Error (`Invalid_CSRF_token token) ->
      Dream.log "Invalid CSRF token %s" token;
      Dream.empty `Bad_Request
  | Error (`Form_response _) -> Dream.empty `Bad_Request
