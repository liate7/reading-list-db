insert into entry (url, title, created_at)
  values (?, ?, datetime('now', 'subsec'))
  on conflict do nothing
  returning id
