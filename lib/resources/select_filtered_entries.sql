select entry.id,
       entry.url,
       entry.title,
       entry.state,
       entry.created_at,
	   coalesce (
         json_group_object(tag.name, tag_entry.payload),
		 '{}')
  from
    entry
  left join
    tag_entry on entry.id = tag_entry.entry
  left join
    tag on tag.id = tag_entry.tag
  where ($1 is null or entry.state in (select value from json_each($1)))
    and ($2 is null or entry.title like '%' || $2 || '%')
    and ($3 is null or exists (
      select tag.id from tag, tag_entry
        where tag.name in (select value from json_each($3))
          and tag.id = tag_entry.tag and tag_entry.entry = entry.id
  ))
  group by entry.id
  order by entry.created_at desc
