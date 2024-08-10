create table if not exists tag (
  id integer primary key not null,
  name text unique not null,
  description text not null default ''
)
