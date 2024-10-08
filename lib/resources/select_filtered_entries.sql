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
  where ($1 is null or $1 is '' or entry.state in (select value from json_each($1)))
    and ($2 is null or $2 is ''
      or entry.title like '%' || $2 || '%'
      or entry.url like '%' || $2 || '%')
    and ($3 is null or $3 is '' or (
      select count(distinct tag.name)
        from tag
        join tag_entry on tag.id = tag_entry.tag
        where tag_entry.entry = entry.id
          and tag.name in (select value from json_each($3))
    ) = (select count(*) from json_each($3))
  )
  group by entry.id
  order by entry.created_at desc
