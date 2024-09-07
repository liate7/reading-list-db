open Dream_html
open! ContainersLabels
open Db.Data
open Route

let tag_to_option (_, { Tag.name; description }) =
  HTML.option [ HTML.value "%s" name ] "%s (%s)" name description

let add_page_tags_template tags =
  let add_tag_attrs =
    [
      Hx.post "/add/tag";
      Hx.target "#submitted-tags";
      Hx.swap "beforeend";
      Hx.params "tag-name";
      Hx.on_ ~event:"submit" "event.preventDefault();";
    ]
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
                 Hx.on_ ~event:"htmx:after-request" "this.value=''";
               ]
              @ add_tag_attrs);
            button
              ([ id "submit-tag"; Hx.include_ "#tag-name" ] @ add_tag_attrs)
              [ txt "Add tag" ];
          ];
        div [ id "submitted-tags" ] [];
      ])

let referent_template url =
  HTML.(
    fieldset []
      [
        legend [] [ txt "Item:" ];
        label
          [ class_ "form-item" ]
          [
            span [ class_ "label-text" ] [ txt "URL:" ];
            input
              [
                name "url";
                type_ "text";
                (match url with Some url -> value "%s" url | None -> null_);
              ];
          ];
        div []
          [
            label
              [ class_ "form-item" ]
              [
                span [ class_ "label-text" ] [ txt "File:" ];
                input
                  [
                    name "file";
                    id "file";
                    type_ "file";
                    accept "application/pdf,.html";
                  ];
                button
                  [
                    onclick "document.getElementById('file').value = '';";
                    Hx.on_ ~event:"submit" "event.preventDefault();";
                  ]
                  [ txt "Clear file" ];
              ];
          ];
      ])

let add_page_template token (tags, url, title') =
  basic_template ~title:"Add an entry" ~token
    HTML.
      [
        form
          [
            id "new-entry"; class_ "new-entry"; method_ `POST; enctype `formdata;
          ]
          [
            referent_template url;
            fieldset []
              [
                legend [] [ txt "Title:" ];
                input
                  [
                    name "title";
                    type_ "text";
                    class_ "form-item";
                    required;
                    (match title' with
                    | Some title -> value "%s" title
                    | None -> null_);
                  ];
              ];
            fieldset []
              [ legend [] [ txt "Tags: " ]; add_page_tags_template tags ];
            div [] [ button [ class_ "submit-entry" ] [ txt "Submit entry" ] ];
          ];
      ]

let page =
  wrap_page
    (fun req ->
      let+ t = Dream.sql req @@ fun conn -> Db.select_all_tags conn in
      Result.(
        t >|= fun t -> (t, Dream.query req "url", Dream.query req "title")))
    add_page_template

let tag_template tag =
  HTML.(
    div
      [ name "%s" tag.Tag.name; class_ "tag" ]
      [
        input [ type_ "hidden"; name "tag"; value "%s" tag.name ];
        output
          [ name "%s" tag.name ]
          [
            txt "%s (%s)" tag.name
              (if String.is_empty tag.description then "none"
               else tag.description);
          ];
        button
          [
            Hx.delete "/add/remove-tag";
            Hx.target "closest div";
            Hx.params "tag";
            Hx.on_ ~event:"submit" "event.preventDefault();";
          ]
          [ txt "Remove" ];
      ])

let create_tag_template tag_name =
  let new_tag_attrs =
    [
      Hx.post "/add/new-tag";
      Hx.include_ "closest .tag-creation-container";
      Hx.swap "outerHTML";
      Hx.target "closest .tag-creation-container";
      Hx.on_ ~event:"submit" "event.preventDefault();";
    ]
  in
  HTML.(
    div
      [ name "%s" tag_name; class_ "tag-creation-container" ]
      [
        input [ type_ "hidden"; name "new-tag-name"; value "%s" tag_name ];
        output [ name "%s" tag_name ] [ txt "%s (" tag_name ];
        input
          ([
             type_ "text";
             name "description";
             placeholder "Enter description";
             Hx.trigger "keyup[key=='Enter']";
             class_ "form-input";
           ]
          @ new_tag_attrs);
        txt ")";
        button new_tag_attrs [ txt "Create tag" ];
        button
          [
            Hx.delete "/add/remove-tag";
            Hx.target "closest .tag-creation-container";
            Hx.trigger "click";
            Hx.on_ ~event:"submit" "event.preventDefault();";
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
        |> List.assoc ~eq:String.equal "new-tag-name"
        |> String.lowercase_ascii |> String.trim
      and descr = form |> List.assoc ~eq:String.equal "description" in
      let+ res = Dream.sql req @@ Db.create_tag ~name ~descr in
      res |> Result.map (fun _ -> Tag.{ name; description = descr }))
    (fun _ -> tag_template)

let ( let*! ) vow body =
  let* res = vow in
  match res with Ok v -> body v | Error e -> Lwt.return @@ Error e

let ( let+! ) res body =
  let*! res = Lwt.return res in
  body res

let ( and+! ) = Result.both

let assoc_all ~eq key assoc =
  assoc |> List.filter_map ~f:(fun (k, v) -> if eq k key then Some v else None)

let list_unpack = function
  | [ x ] -> Ok x
  | list -> Error (`Not_a_singleton list)

(* let validate_url url =  *)

let add_entry ~url ~file ~tags ~title
    ((module Conn : Caqti_lwt.CONNECTION) as conn) =
  Conn.with_transaction @@ fun () ->
  let*! url, _hash =
    match (url, file) with
    | _, Some (_, contents) ->
        let*! hash = Cas.write Cas.Pdf contents conn in
        Lwt.return @@ Result.return
        @@ (Printf.sprintf "/local/%s" (hash :> string), Some hash)
    | "", None -> Lwt.return @@ Error (`Missing "referent")
    | url, None -> Lwt.return @@ Result.return (url, None)
  in
  let*! tag_ids = Db.select_tags_by_name tags conn in
  let tag_ids = tag_ids |> List.map ~f:(fun (i, _) -> i) in
  let*! () =
    Lwt.return
    @@
    if List.compare_lengths tags tag_ids = 0 then Ok () else Error `Invalid_tags
  in
  let*! entry = Db.create_entry ~url ~title conn in
  let*! () = Db.multi_tag_entry entry tag_ids conn in
  Lwt.return @@ Result.return ()

let response req =
  let* res =
    let*! form = get_form ~form_getter:Dream.multipart req in
    Dream.log "%a"
      (List.pp (fun fmt (name, _) -> Format.fprintf fmt "%s: …" name))
      form;
    let eq = String.equal in
    let tags =
      form |> List.assoc_opt ~eq "tag" |> Option.get_or ~default:[]
      |> List.map ~f:(fun (_, v) -> v)
    in
    let+! title =
      form |> List.assoc ~eq "title" |> function
      | [ (None, "") ] | [] -> Error (`Missing "url")
      | [ (None, title) ] -> Ok title
      | _ -> assert false
    and+! _, url = form |> List.assoc ~eq "url" |> list_unpack in
    let file =
      form |> List.assoc_opt ~eq "file" |> function
      | None -> None
      | Some [] -> None
      | Some [ elt ] -> Some elt
      | _ -> assert false
    in
    Dream.sql req @@ add_entry ~url ~file ~title ~tags
  in
  match res with
  | Ok () -> Dream.redirect req "/"
  | Error _ -> Dream.empty `Bad_Request

let routes =
  [
    Dream.any "/add/" (fun req ->
        Dream.redirect ~status:`Moved_Permanently req "/add");
    Dream.get "/add" page;
    Dream.post "/add" response;
    Dream.post "/add/tag" add_tag_response;
    Dream.post "/add/new-tag" new_tag_response;
    Dream.delete "/add/remove-tag" (fun _ -> Dream.html "");
  ]
