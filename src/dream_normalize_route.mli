val normalize : (string -> Dream.handler -> Dream.route) -> string -> Dream.handler -> Dream.route list
(** [normalize route_constructor pattern handler] applies [route_constructor] on [pattern] and [handler], and also applies [route_constructor] on a pattern that
 * - is equivalent to [pattern ^ "/"] if [pattern] does not have a trailing slash
 * - is equivalent to [pattern] without trailing slashes otherwise
 * and a handler that redirects to the former pattern. *)
