open Dream_html
open! ContainersLabels
open Db.Data

let ( let* ) = Lwt.bind
let ( let+ ) = Lwt.map
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

let entry_to_row (_, { url; Entry.title = title'; state; created_at; tags }) =
  let created_at =
    Timedesc.Utils.timestamp_of_ptime created_at
    |> Timedesc.of_timestamp_exn ~tz_of_date_time:Timedesc.Time_zone.utc
    |> Timedesc.to_string
         ~format:
           "{year}-{mon:0X}-{day:0X}T{hour:0X}:{min:0X}{tzoff-sign}{tzoff-hour:0X}{tzoff-min:0X}"
  in
  HTML.(
    tr []
      [
        td [] [ a [ HTML.href "%s" @@ Uri.to_string url ] [ txt "%s" title' ] ];
        td [] [ txt "%s" @@ Entry.state_to_string state ];
        td [ class_ "time" ] [ txt "%s" created_at ];
        List.map tags ~f:(function
          | name, Some value -> txt "%s(%s)" name value
          | name, None -> txt "%s" name)
        |> List.intersperse ~x:(txt ", ")
        |> td [];
      ])

let render_entries entries = List.map entries ~f:entry_to_row

let table_container entries =
  HTML.(
    div
      [ class_ "table-container" ]
      [
        table []
          [
            thead []
              [
                tr []
                  [
                    th [] [ txt "Title" ];
                    th [] [ txt "State" ];
                    th [] [ txt "Date" ];
                    th [] [ txt "Tags" ];
                  ];
              ];
            tbody [ id "search-results" ] @@ render_entries entries;
          ];
      ])

let input_container _ =
  HTML.(
    div
      [ class_ "input-container" ]
      [
        input
          [
            class_ "form-control";
            type_ "search";
            name "search";
            placeholder "Search titles…";
            Hx.post "/search";
            Hx.trigger "input changed delay:500ms, search";
            Hx.target "#search-results";
            Hx.indicator ".htmx-indicator";
          ];
      ])

let main_page_template token entries =
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
            title [] "%s - Things to read" app_name;
            style [] "%s" [%blob "resources/style.css"];
            script [] "%s" [%blob "resources/fix_dates.js"];
            htmx_script;
          ];
        body
          [ Hx.headers {|{"X-CSRF-Token": "%s"}|} token ]
          [
            div
              [ class_ "container" ]
              [ table_container entries; input_container () ];
          ];
      ])

let main_page =
  wrap_page (fun req -> Dream.sql req Db.select_all_entries) main_page_template

let search_response req =
  match Dream.header req "X-CSRF-Token" with
  | None ->
      Dream.log "CSRF token missing";
      Dream.empty `Bad_Request
  | Some tok -> (
      let* state = Dream.verify_csrf_token req tok in
      match state with
      | `Invalid ->
          Dream.log "Invalid CSRF token: %s" tok;
          Dream.empty `Bad_Request
      | `Expired _ | `Wrong_session | `Ok -> (
          let* form_res = Dream.form ~csrf:false req in
          match form_res with
          | `Ok form -> (
              let search =
                Option.(
                  form |> List.assoc_opt ~eq:String.equal "search" >>= function
                  | "" -> None
                  | s -> Some s)
              in
              let* res = Dream.sql req @@ Db.select_filtered_entries ?search in
              match res with
              | Ok entries ->
                  entries |> render_entries |> HTML.null |> Dream_html.respond
              | Error err ->
                  Dream.log "%a@." Caqti_error.pp err;
                  Dream.empty `Internal_Server_Error)
          | _ -> Dream.empty `Bad_Request))
