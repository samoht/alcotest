let to_test () = raise (Failure "")
let expect_failure = function Failure _ -> true | _ -> false
let test () = Alcotest.match_raises "Generates Failure" expect_failure to_test

let () =
  Alcotest.run "Exceptions"
    [
      ( "matches_raises",
        [
          Alcotest.test_case "True means the exception is expected" `Quick test;
        ] );
    ]
