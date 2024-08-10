open Reading_list

let () =
  Dream.run @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:/tmp/test.db"
  @@ Dream.sql_sessions
  @@ Dream.router
       [
         Dream.get "/" Route.main_page;
         Dream.get "/search" (fun req -> Dream.redirect req "/");
         Dream.post "/search" Route.search_response;
       ]
