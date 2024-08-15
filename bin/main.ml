open Reading_list

let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.sql_pool ("sqlite3:" ^ Sys.argv.(2))
  @@ Dream.sql_sessions @@ Dream.origin_referrer_check
  @@ Dream.router
       ([
          Dream.get "/assets/**" @@ Dream.static Sys.argv.(1);
          Dream.get "/local/**" @@ Dream.static ~loader:Cas.loader "";
        ]
       @ Main.routes @ Add.routes)
