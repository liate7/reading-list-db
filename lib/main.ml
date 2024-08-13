open Dream_html
open! ContainersLabels
open Db.Data
open Route

let render_domain uri =
  let domain = Uri.host_with_default ~default:"local" uri in
  match Stringext.chop_prefix ~prefix:"www." domain with
  | Some domain -> domain
  | None -> domain

let entry_to_row
    ((entry_id, { url; title = title'; state; created_at; tags }) :
      Entry.id * Entry.t) =
  let created_at =
    Timedesc.Utils.timestamp_of_ptime created_at
    |> Timedesc.of_timestamp_exn ~tz_of_date_time:Timedesc.Time_zone.utc
    |> Timedesc.to_string
         ~format:
           "{year}-{mon:0X}-{day:0X}T{hour:0X}:{min:0X}{tzoff-sign}{tzoff-hour:0X}{tzoff-min:0X}"
  and common =
    [
      Hx.target "#search-results";
      Hx.vals {|{"entry": %d}|} (entry_id :> int);
      Hx.include_ "#input-container";
    ]
  in
  HTML.(
    tr []
      [
        td []
          [
            a
              ([
                 href "%s" @@ Uri.to_string url;
                 Hx.post "/state/view";
                 Hx.trigger "click";
                 onclick "window.open(this.href, '_blank'); return false;";
               ]
              @ common)
              [ txt "%s" title' ];
            br [];
            div [ class_ "entry-meta" ] [ txt "%s" (render_domain url) ];
          ];
        td []
          [
            txt "%s " @@ Entry.state_to_string state;
            br [];
            div
              [ class_ "field-controls" ]
              ((match state with
               | To_read -> []
               | Read | Reading ->
                   [
                     button
                       ([ Hx.post "/state/reset" ] @ common)
                       [ txt "Reset" ];
                   ])
              @ [
                  button ([ Hx.post "/state/finish" ] @ common) [ txt "Finish" ];
                ]);
          ];
        td [] [ time [ datetime "%s" created_at ] [ txt "%s" created_at ] ];
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
                    th [] [ txt "Date added" ];
                    th [] [ txt "Tags" ];
                  ];
              ];
            tbody [ id "search-results" ] @@ render_entries entries;
          ];
      ])

let input_container =
  let common_htmx =
    [
      Hx.post "/search";
      Hx.target "#search-results";
      Hx.indicator ".htmx-indicator";
      Hx.include_ "closest form";
    ]
  in
  HTML.(
    form
      [ class_ "input-container" ]
      [
        img
          [
            src "/assets/spinner.svg";
            class_ "htmx-indicator";
            alt "Loading indicator";
          ];
        fieldset
          [ class_ "title-search" ]
          [
            legend [] [ txt "Search titles:" ];
            input
              ([
                 class_ "form-control";
                 type_ "search";
                 name "search";
                 placeholder "Search titles…";
                 Hx.trigger "input changed delay:500ms, search";
               ]
              @ common_htmx);
          ];
        fieldset
          [ name "state"; class_ "to-read-states" ]
          (let checkbox_common =
             [ type_ "checkbox"; Hx.trigger "click delay:500ms" ] @ common_htmx
           in
           [
             legend [] [ txt "To-read state:" ];
             input ([ name "to-read"; id "to-read"; checked ] @ checkbox_common);
             label [ for_ "to-read" ] [ txt "To read" ];
             input ([ name "reading"; id "reading"; checked ] @ checkbox_common);
             label [ for_ "reading" ] [ txt "Reading" ];
             input ([ name "read"; id "read" ] @ checkbox_common);
             label [ for_ "read" ] [ txt "Read" ];
           ]);
        fieldset
          [ class_ "tags-search" ]
          [
            legend [] [ txt "Tags (comma-separated):" ];
            input
              ([
                 class_ "form-control";
                 type_ "text";
                 name "tags";
                 placeholder "Comma, separated, list, of tags…";
                 Hx.trigger "input changed delay:500ms, search";
               ]
              @ common_htmx);
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
            link [ href "/assets/style.css"; rel "stylesheet" ];
            script [ src "/assets/fix_dates.js" ] "";
            htmx_script;
          ];
        body
          [ Hx.headers {|{"X-CSRF-Token": "%s"}|} token ]
          [
            div
              [ class_ "entries" ]
              [ table_container entries; input_container ];
          ];
      ])

let page =
  wrap_page
    (fun req ->
      Dream.sql req
      @@ Db.select_filtered_entries ~states:[ Entry.To_read; Entry.Reading ])
    main_page_template

let search_query form req =
  Dream.log "Form: %a"
    (List.pp @@ fun fmt (k, v) -> Format.fprintf fmt "'%s': '%s'" k v)
    form;
  let search =
    Option.(
      form |> List.assoc_opt ~eq:String.equal "search" >>= function
      | "" -> None
      | s -> Some s)
  and tags =
    form
    |> List.assoc_opt ~eq:String.equal "tags"
    |> Option.get_or ~default:"" |> String.split ~by:","
    |> List.map ~f:String.trim
    |> List.filter ~f:(fun s -> String.(s <> ""))
    |> List.map ~f:String.lowercase_ascii
  and states =
    form
    |> List.filter_map ~f:(fun (k, v) ->
           match Entry.state_of_string k with
           | Some s when String.(v = "on") -> Some s
           | _ -> None)
  in
  Dream.sql req @@ Db.select_filtered_entries ~tags ?search ~states

let search_response =
  Route.wrap_post_response search_query
    Fun.(fun _ -> render_entries %> HTML.null)

let state_handler query =
  Route.wrap_post_response
    (fun form req ->
      Dream.log "Form: %a"
        (List.pp @@ fun fmt (k, v) -> Format.fprintf fmt "'%s': '%s'" k v)
        form;
      let id =
        List.assoc ~eq:String.equal "entry" form
        |> Int.of_string_exn |> Entry.id_of_int
      in
      let* res = Dream.sql req @@ query id in
      match res with
      | Error err -> Lwt.return @@ Error err
      | Ok () -> search_query form req)
    Fun.(fun _ -> render_entries %> HTML.null)

let view_response = state_handler Db.start_reading
let reset_response = state_handler Db.restart_reading
let finish_response = state_handler Db.finish_reading

let routes =
  [
    Dream.get "/" page;
    Dream.get "/search" (fun req -> Dream.redirect req "/");
    Dream.post "/search" search_response;
    Dream.post "/state/view" view_response;
    Dream.post "/state/reset" reset_response;
    Dream.post "/state/finish" finish_response;
  ]
