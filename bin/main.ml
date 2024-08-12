open Reading_list

let () =
  Dream.run @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:/tmp/test.db"
  @@ Dream.sql_sessions
  @@ Dream.router
       [
         Dream.get "/" Main.page;
         Dream.get "/search" (fun req -> Dream.redirect req "/");
         Dream.post "/search" Main.search_response;
         Dream.scope "/add" [] Add.routes;
       ]
