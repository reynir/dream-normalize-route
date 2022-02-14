let not_found _req =
  Dream.respond ~status:`Not_Found "NOT FOUND"

let handler req =
  Dream.respond ("/" ^ String.concat "/" (Dream.path req))

let run routes target =
  let open Lwt.Syntax in
  let path =
    let* resp = (Dream.router routes) not_found (Dream.request ~target "") in
    Dream.body resp
  in
  Lwt_main.run path

let test check_string pattern path expected =
  let routes = Dream_normalize_route.normalize Dream.get pattern handler in
  Alcotest.(check string check_string) (run routes path) expected

let test_root () =
  test "root" "/" "/" "/"

let test_empty () =
  test "empty" "" "/" "/"

let test_no_slash_no_slash () =
  test "no slash no slash" "/hello-world" "/hello-world" "/hello-world"

let test_no_slash_slash () =
  test "no slash slash" "/hello-world" "/hello-world/" "/hello-world"

let test_slash_slash () =
  test "slash slash" "/hello-world/" "/hello-world/" "/hello-world/"

let test_slash_no_slash () =
  test "slash no slash" "/hello-world/" "/hello-world" "/hello-world/"

let test_extra_slash_slash () =
  test "extra slash" "/hello-world//" "/hello-world/" "/hello-world/"

let test_extra_slash_no_slash () =
  test "extra slash" "/hello-world//" "/hello-world" "/hello-world/"

let test_variable_slash_slash () =
  test "variable" "/:var/" "/hello/" "/hello/"

let test_variable_slash_no_slash () =
  test "variable" "/:var/" "/hello" "/hello/"

let test_path_wildcard () =
  test "path wildcard" "/**" "/hello-world" "/hello-world"

let () =
  let open Alcotest in
  run "Dream_normalize_route" [
    "simple", [
      test_case "Root" `Quick test_root;
      test_case "Empty root" `Quick test_empty;
      test_case "No slash/no slash" `Quick test_no_slash_no_slash;
      test_case "No slash/slash" `Quick test_no_slash_slash;
      test_case "Slash/slash" `Quick test_slash_slash;
      test_case "Slash/no slash" `Quick test_slash_no_slash;
    ];
    "extra-slash", [
      test_case "Extra slash/slash" `Quick test_extra_slash_slash;
      test_case "Extra slash/no slash" `Quick test_extra_slash_no_slash;
    ];
    "variable", [
      test_case "Variable slash/slash" `Quick test_variable_slash_slash;
      test_case "Variable slash/no slash" `Quick test_variable_slash_no_slash;
    ];
    "path-wildcard", [
      test_case "Path wildcard" `Quick test_path_wildcard;
    ];
  ]
