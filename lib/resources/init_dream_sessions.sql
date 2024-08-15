create table if not exists dream_session (
  id text primary key,
  label text not null,
  expires_at real not null,
  payload text not null
)
