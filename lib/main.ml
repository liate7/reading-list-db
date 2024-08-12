open Dream_html
open! ContainersLabels
open Db.Data
open Route

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

let input_container =
  HTML.(
    div
      [ class_ "input-container" ]
      [
        img [ src "/assets/spinner.svg"; class_ "htmx-indicator" ];
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
  wrap_page (fun req -> Dream.sql req Db.select_all_entries) main_page_template

let search_response =
  Route.wrap_post_response
    (fun form req ->
      let search =
        Option.(
          form |> List.assoc_opt ~eq:String.equal "search" >>= function
          | "" -> None
          | s -> Some s)
      in
      Dream.sql req @@ Db.select_filtered_entries ?search)
    Fun.(fun _ -> render_entries %> HTML.null)
