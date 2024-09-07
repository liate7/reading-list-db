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
        defer;
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

let get_form :
      'a.
      form_getter:(?csrf:bool -> Dream.request -> 'a Dream.form_result Lwt.t) ->
      Dream.request ->
      ( 'a,
        [> `Form_response of 'a Dream.form_result
        | `Invalid_CSRF_token of string
        | `No_CSRF_token ] )
      result
      Lwt.t =
 fun ~form_getter req ->
  (* match Dream.header req "X-CSRF-Token" with
     | None -> Lwt.return (Error `No_CSRF_token)
     | Some tok -> (
         let* state = Dream.verify_csrf_token req tok in
         match state with
         | `Invalid -> Lwt.return (Error (`Invalid_CSRF_token tok))
         | `Expired _ | `Wrong_session | `Ok -> (
             let+ form_res = form_getter ~csrf:false req in
             match form_res with
             | `Ok form -> Ok form
             | err -> Error (`Form_response err))) *)
  let+ form_res = form_getter ~csrf:false req in
  match form_res with `Ok form -> Ok form | err -> Error (`Form_response err)

let wrap_post_response vow_fn template req =
  let* res = get_form ~form_getter:Dream.form req in
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

let header_elt : url:string -> content:string -> node =
 fun ~url ~content:content' ->
  HTML.(li [] [ a [ href "%s" url ] [ txt "%s" content' ] ])

let basic_template ~title ~token body' =
  let actual_title = HTML.title [] "%s - %s" app_name title in
  HTML.(
    html
      [ lang "en" ]
      [
        head []
          [
            meta [ charset "UTF-8" ];
            meta
              [
                name "viewport"; content "width=device-width, initial-scale=1.0";
              ];
            actual_title;
            link [ href "/assets/style.css"; rel "stylesheet" ];
            link [ href "/assets/favicon.png"; rel "icon" ];
            script [ src "/assets/fixups.js"; defer ] "";
            htmx_script;
          ];
        body
          [ Hx.headers {|{"X-CSRF-Token": "%s"}|} token ]
          (header
             [ class_ "header" ]
             [
               nav
                 [ class_ "nav" ]
                 [
                   ul []
                     [
                       header_elt ~url:"/" ~content:"Main";
                       header_elt ~url:"/add" ~content:"Add an entry";
                     ];
                 ];
             ]
          :: body');
      ])
