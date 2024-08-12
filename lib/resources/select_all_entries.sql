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
  group by entry.id
  order by entry.created_at desc
