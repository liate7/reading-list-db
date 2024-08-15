select entry.url,
       entry.title,
       entry.state,
       entry.created_at,
	   coalesce (
         json_group_array(
		   json_array(
		     json_object('name', tag.name, 'description', tag.description),
			 tag_entry.payload)
		 ),
		 '{}')
  from entry, tag, tag_entry
  where tag.id = tag_entry.tag and tag_entry.entry = entry.id
    and entry.id = ?
