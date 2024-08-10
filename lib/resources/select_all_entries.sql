select entry.id,
       entry.url,
       entry.title,
       entry.state,
       entry.created_at,
       json_group_object(tag.name, tag_entry.payload)
  from entry, tag, tag_entry
  where tag.id = tag_entry.tag and tag_entry.entry = entry.id
  group by entry.id
  order by entry.created_at desc
