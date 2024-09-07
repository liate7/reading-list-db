select entry.id,
       entry.url,
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
  from
    entry
  left join
    tag_entry on entry.id = tag_entry.entry
  left join 
    tag on tag.id = tag_entry.tag
  group by entry.id
  order by entry.created_at desc
