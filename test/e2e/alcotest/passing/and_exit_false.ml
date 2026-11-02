let () =
  let open Alcotest in
  let id () = () in
  run ~and_exit:false __FILE__
    [ ("test-a", [ test_case "Test case" `Quick id ]) ];
  Fmt.pr " Program execution continued!%!"
