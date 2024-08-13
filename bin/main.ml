open Reading_list

let () =
  Dream.run @@ Dream.logger
  @@ Dream.sql_pool ("sqlite3:" ^ Sys.argv.(2))
  @@ Dream.sql_sessions
     (* @@ Dream.livereload (* Doesn't work, breaks HTMX html fragments *) *)
  @@ Dream.router
       ([ Dream.get "/assets/**" @@ Dream.static Sys.argv.(1) ]
       @ Main.routes @ Add.routes)
