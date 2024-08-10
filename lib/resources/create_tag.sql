insert into tag (name, description) values (?, ?)
  on conflict (name) do
    update set description = excluded.description
  returning id
