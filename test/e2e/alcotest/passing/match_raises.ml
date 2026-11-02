let raise_failure () = raise (Failure "")
let expect_failure = function Failure _ -> true | _ -> false

let test () =
  Alcotest.match_raises "Generates Failure" expect_failure raise_failure

let () =
  Alcotest.run "Exceptions"
    [
      ( "matches_raises",
        [
          Alcotest.test_case "True means the exception is expected" `Quick test;
        ] );
    ]
