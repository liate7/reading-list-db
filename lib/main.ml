open Dream_html
open! ContainersLabels
open Db.Data
open Route

let render_domain uri =
  let domain = Uri.host_with_default ~default:"local" uri in
  match Stringext.chop_prefix ~prefix:"www." domain with
  | Some domain -> domain
  | None -> domain

let entry_to_row : Entry.id * Entry.t -> node =
 fun (entry_id, { url; title = title'; state; created_at; tags }) ->
  let created_at =
    Timedesc.Utils.timestamp_of_ptime created_at
    |> Timedesc.of_timestamp_exn ~tz_of_date_time:Timedesc.Time_zone.utc
    |> Timedesc.to_string
         ~format:
           "{year}-{mon:0X}-{day:0X}T{hour:0X}:{min:0X}{tzoff-sign}{tzoff-hour:0X}{tzoff-min:0X}"
  and with_htmx f attrs =
    f
      (attrs
      @ [
          Hx.target "#search-results";
          Hx.vals {|{"entry": %d}|} (entry_id :> int);
          Hx.include_ ".input-container";
        ])
  in
  HTML.(
    tr []
      [
        td []
          [
            with_htmx a
              [
                href "%s" @@ Uri.to_string url;
                Hx.post "/state/view";
                Hx.trigger "click";
                onclick "window.open(this.href, '_blank'); return false;";
              ]
              [ txt "%s" title' ];
            br [];
            ul
              [ class_ "entry-meta" ]
              [
                li [] [ txt "From: %s" (render_domain url) ];
                li []
                  [
                    txt "Added: ";
                    time [ datetime "%s" created_at ] [ txt "%s" created_at ];
                  ];
              ];
          ];
        List.map tags ~f:(fun ({ Tag.name; description }, value) ->
            span
              [ title_ "%s" description; class_ "tag" ]
              [
                (match value with
                | Some value -> txt "%s(%s)" name value
                | None -> txt "%s" name);
              ])
        |> List.intersperse ~x:(txt ", ")
        |> td [];
        td []
          [
            txt "%s " @@ Entry.state_to_string state;
            br [];
            div
              [ class_ "field-controls" ]
              [
                (match state with
                | To_read -> null []
                | Read | Reading ->
                    with_htmx button
                      [ Hx.post "/state/reset" ]
                      [ txt "Restart" ]);
                (match state with
                | To_read | Reading ->
                    with_htmx button
                      [ Hx.post "/state/finish" ]
                      [ txt "Finish" ]
                | Read -> null []);
              ];
          ];
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
                    th [] [ txt "Tags" ];
                    th [] [ txt "State" ];
                  ];
              ];
            tbody [ id "search-results" ] @@ render_entries entries;
          ];
      ])

let input_container =
  let input_field attrs =
    HTML.(
      input
        (attrs
        @ [
            Hx.post "/search";
            Hx.target "#search-results";
            Hx.indicator ".htmx-indicator";
            Hx.include_ "closest form";
          ]))
  in
  let make_state_checkbox ?(checked_ = false) ~state label_name =
    HTML.(
      div
        [ class_ "state-checkbox-container" ]
        [
          label []
            [
              input_field
                ([
                   name "%s" state;
                   id "%s" state;
                   type_ "checkbox";
                   Hx.trigger "click";
                 ]
                @ if checked_ then [ HTML.checked ] else []);
              txt "%s" label_name;
            ];
        ])
  in

  HTML.(
    form
      [ class_ "input-container" ]
      [
        fieldset
          [ class_ "title-search" ]
          [
            legend [] [ txt "Search titles:" ];
            input_field
              [
                class_ "form-control";
                type_ "search";
                name "search";
                placeholder "Search titles…";
                Hx.trigger "input changed delay:500ms, search";
              ];
            (* img *)
            (*   [ *)
            (*     src "/assets/spinner.svg"; *)
            (*     class_ "htmx-indicator"; *)
            (*     alt "Loading indicator"; *)
            (*   ]; *)
          ];
        div
          [ class_ "meta-search-container"; id "meta-search-container" ]
          [
            fieldset
              [ name "state"; class_ "to-read-states" ]
              [
                legend [] [ txt "State:" ];
                make_state_checkbox ~checked_:true ~state:"to-read" "To read";
                make_state_checkbox ~checked_:true ~state:"reading" "Reading";
                make_state_checkbox ~state:"read" "Read";
              ];
            fieldset
              [ class_ "tags-search" ]
              [
                legend [] [ txt "Tags:" ];
                input_field
                  [
                    class_ "form-control";
                    type_ "text";
                    name "tags";
                    placeholder "Comma, separated, list, of tags…";
                    Hx.trigger "input changed delay:500ms, search";
                  ];
              ];
          ];
      ])

let page =
  wrap_page (fun req ->
      Dream.sql req
      @@ Db.select_filtered_entries ~states:[ Entry.To_read; Entry.Reading ])
  @@ fun token entries ->
  Route.basic_template ~title:"Things to read" ~token
    HTML.
      [ div [ class_ "entries" ] [ table_container entries; input_container ] ]

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
