let () =
  let root = Sys.argv.(1) and suite = Sys.argv.(2) in
  let mode = Sys.argv.(3) and payload = Sys.argv.(4) in
  let test () =
    print_endline payload;
    if mode = "hold" || mode = "hold-fail" then (
      let ready = open_out Sys.argv.(5) in
      output_string ready "ready\n";
      close_out ready;
      try ignore (input_line stdin) with End_of_file -> ());
    if mode = "crash" then exit 7;
    if mode = "fail" || mode = "hold-fail" then Alcotest.fail payload
  in
  try
    Alcotest.run ~argv:[| "runner" |] ~and_exit:false ~log_dir:root suite
      [ ("case", [ Alcotest.test_case "payload" `Quick test ]) ]
  with Alcotest.Test_error -> exit 1
