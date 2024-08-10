create table if not exists entry (
  id integer primary key asc not null,
  url text not null,
  title text not null,
  state text not null
    default 'to-read'
    check (state in ('to-read', 'reading', 'read')),
  created_at text not null,
  unique (url, title)
)
