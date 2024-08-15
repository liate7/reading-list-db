create table if not exists cas (
  hash blob primary key,
  mime text not null,
  content blob not null
)
