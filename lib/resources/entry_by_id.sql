select entry.url,
       entry.title,
       entry.state,
       entry.created_at,
	   case
	     when count(tag.id) = 0 then '[]'
		 else 
         json_group_array(
		     json_object(
			   'name', tag.name,
			   'description', tag.description,
			   'payload', tag_entry.payload
			 )
		 )
	   end as tags
  from entry, tag, tag_entry
  where tag.id = tag_entry.tag and tag_entry.entry = entry.id
    and entry.id = ?
