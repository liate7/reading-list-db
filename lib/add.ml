open Dream_html
open! ContainersLabels
open Db.Data
open Route

let tag_to_option (_, { Tag.name; description }) =
  HTML.option [ HTML.value "%s" name ] "%s (%s)" name description

let add_page_tags_template tags =
  let add_tag_attrs =
    [ Hx.post "/add/tag"; Hx.target "#submitted-tags"; Hx.swap "beforeend" ]
  in
  HTML.(
    div
      [ class_ "tags"; name "tags"; Hx.sync ".tags" ]
      [
        datalist [ id "existing-tags" ] @@ List.map ~f:tag_to_option tags;
        div
          [ class_ "tag-div" ]
          [
            input
              ([
                 list "existing-tags";
                 type_ "text";
                 placeholder "Tag name";
                 name "tag-name";
                 id "tag-name";
                 Hx.trigger "keyup[key=='Enter']";
                 Hx.on_ ~event:"after-request" "this.value=''";
               ]
              @ add_tag_attrs);
            button
              ([ id "submit-tag"; Hx.include_ "#tag-name" ] @ add_tag_attrs)
              [ txt "Add tag" ];
          ];
        div [ id "submitted-tags" ] [];
      ])

let add_page_template token (tags, _) =
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
            title [] "%s - Add an entry" app_name;
            link [ href "/assets/style.css"; rel "stylesheet" ];
            htmx_script;
          ];
        body
          [ Hx.headers {|{"X-CSRF-Token": "%s"}|} token ]
          [
            form
              [
                id "new-entry";
                class_ "new-entry";
                Hx.post "/add/";
                Hx.include_ "#submitted-tags > .tag";
              ]
              [
                label [ for_ "url" ] [ txt "URL: " ];
                input [ name "url" ];
                label [ for_ "title" ] [ txt "Title: " ];
                input [ name "title" ];
                label [ for_ "tags" ] [ txt "Tags: " ];
                add_page_tags_template tags;
                button [ id "submit-entry" ] [ txt "Submit entry" ];
              ];
          ];
      ])

let page =
  wrap_page
    (fun req ->
      Dream.sql req @@ fun conn ->
      let+ t = Db.select_all_tags conn in
      Result.(t >|= fun t -> (t, ())))
    add_page_template

let tag_template tag =
  HTML.(
    div
      [ name "%s" tag.Tag.name; class_ "tag" ]
      [
        input [ type_ "hidden"; name "tag"; value "%s" tag.name ];
        output [ name "%s" tag.name ] [ txt "%s (%s)" tag.name tag.description ];
        button
          [ Hx.delete "/add/remove-tag"; Hx.target "closest div" ]
          [ txt "Remove" ];
      ])

let create_tag_template tag_name =
  let new_tag_attrs =
    [ Hx.post "/add/new-tag"; Hx.include_ "closest form"; Hx.swap "outerHTML" ]
  in
  HTML.(
    form
      [ name "%s" tag_name ]
      [
        input [ type_ "hidden"; name "tag-name"; value "%s" tag_name ];
        output [ name "%s" tag_name ] [ txt "%s (" tag_name ];
        input
          ([
             type_ "text";
             name "description";
             placeholder "Enter description";
             Hx.trigger "keyup[key=='Enter']";
           ]
          @ new_tag_attrs);
        txt ")";
        button new_tag_attrs [ txt "Create tag" ];
        button
          [
            Hx.delete "/add/remove-tag";
            Hx.target "closest div";
            (* Hx.replace_url "outerHTML"; *)
          ]
          [ txt "Cancel" ];
      ])

let add_tag_response =
  Route.wrap_post_response
    (fun form req ->
      Dream.log "%a"
        (List.pp (fun fmt (k, v) -> Format.fprintf fmt "%s: %s" k v))
        form;
      let name = form |> List.assoc ~eq:String.equal "tag-name" in
      let+ tag = Dream.sql req @@ Db.tag_by_name name in
      match tag with
      | Ok (Some (_, tag)) -> Ok (`Existing tag)
      | Ok None -> Ok (`New name)
      | Error err -> Error err)
    (fun _ -> function
      | `New name -> create_tag_template name
      | `Existing tag -> tag_template tag)

let new_tag_response =
  Route.wrap_post_response
    (fun form req ->
      Dream.log "%a"
        (List.pp (fun fmt (k, v) -> Format.fprintf fmt "%s: %s" k v))
        form;
      let name =
        form
        |> List.assoc ~eq:String.equal "tag-name"
        |> String.lowercase_ascii |> String.trim
      and descr = form |> List.assoc ~eq:String.equal "description" in
      let+ res = Dream.sql req @@ Db.create_tag ~name ~descr in
      res |> Result.map (fun _ -> Tag.{ name; description = descr }))
    (fun _ -> tag_template)

let assoc_all ~eq key assoc =
  assoc |> List.filter_map ~f:(fun (k, v) -> if eq k key then Some v else None)

let response req =
  let ( let*! ) vow body =
    let* res = vow in
    match res with Ok v -> body v | Error e -> Lwt.return @@ Error e
  in
  let ( let+! ) res body =
    let*! res = Lwt.return res in
    body res
  and ( and+! ) = Result.both in
  let* res =
    let*! form = get_forms req in
    let eq = String.equal in
    let tags =
      form |> assoc_all ~eq "tag" |> List.sort_uniq ~cmp:String.compare
    in
    let+! title =
      form |> List.assoc ~eq "title" |> function
      | "" -> Error (`Missing "title")
      | s -> Ok s
    and+! url =
      form |> List.assoc ~eq "url" |> function
      | "" -> Error (`Missing "url")
      | s -> Ok s
    in
    Dream.sql req @@ fun (module Conn : Caqti_lwt.CONNECTION) ->
    Conn.with_transaction @@ fun () ->
    let*! tag_ids = Db.select_tags_by_name tags (module Conn) in
    let tag_ids = tag_ids |> List.map ~f:(fun (i, _) -> i) in
    let*! () =
      Lwt.return
      @@
      if List.compare_lengths tags tag_ids = 0 then Ok ()
      else Error `Invalid_tags
    in
    let*! entry = Db.create_entry ~url ~title (module Conn) in
    Db.tag_entry' entry tag_ids (module Conn)
  in
  match res with
  | Ok () -> Dream.redirect req "/"
  | Error _ -> Dream.empty `Bad_Request

let routes =
  [
    Dream.get "/add/" page;
    Dream.post "/add/" response;
    Dream.post "/add/tag" add_tag_response;
    Dream.post "/add/new-tag" new_tag_response;
    (Dream.delete "/add/remove-tag" @@ fun _ -> Dream.html "");
  ]
