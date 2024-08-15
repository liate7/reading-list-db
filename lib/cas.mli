type filetype = Pdf
type hash = private string

val hash_of_string : string -> hash
val filetype_to_string : filetype -> string
val filetype_of_string : string -> filetype option
val write : filetype -> string -> (hash, 'err) Db.query
val get : hash -> ((filetype * string) option, [> `Not_a_base64 ]) Db.query
val loader : string -> string -> Dream.handler
