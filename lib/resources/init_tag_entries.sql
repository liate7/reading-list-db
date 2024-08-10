create table if not exists tag_entry (
  entry integer references entry(id) not null,
  tag integer references tag(id) not null,
  payload text,
  unique (entry, tag)
)
