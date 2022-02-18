type classify =
  | With_slash of string
  | Without_slash of string

let classify pattern =
  let rec ending idx =
    if idx = 0
    then 0
    else if pattern.[idx - 1] = '/'
    then ending (idx - 1)
    else idx
  in
  let last = ending (String.length pattern) in
  if last = String.length pattern
  then Without_slash pattern
  else With_slash (String.sub pattern 0 last)

let redirect req =
  let path = Dream.path req in
  let path =
    match List.rev path with
    | "" :: path -> 
      List.rev path
    | path ->
      List.rev ("" :: path)
  in
  let queries = Dream.all_queries req in
  let queries = if queries = [] then ""
    else
      "?" ^ String.concat "&" (List.map (fun (k, v) -> k^"="^v) queries)
  in
  Dream.redirect ~status:`Permanent_Redirect req ("/" ^ String.concat "/" path ^ queries)


let normalize meth pattern handler =
  match classify pattern with
  | Without_slash pattern ->
    [ meth pattern handler; meth (pattern ^ "/") redirect ]
  | With_slash pattern ->
    [ meth (pattern ^ "/") handler; meth pattern redirect ]
