let env_key = "ALCOTEST_SOURCE_CODE_POSITION"

let check_output root index (setting, expected) =
  Unix.putenv env_key setting;
  let log_dir = Filename.concat root (string_of_int index) in
  let line = __LINE__ + 2 in
  let callback () =
    Alcotest.(check bool) "callsite" true false;
    assert false
  in
  (match
     Alcotest.run ~argv:[| "callsite" |] ~and_exit:false ~log_dir "position"
       [ ("case", [ Alcotest.test_case "failing assertion" `Quick callback ]) ]
   with
  | () -> Alcotest.fail "failing assertion unexpectedly returned"
  | exception Alcotest.Test_error -> ());
  let output =
    Test_support.read (Filename.concat log_dir "position/case.000.output")
  in
  let marker = Fmt.str "File %S, line %d, character " __FILE__ line in
  let observed = Re.execp (Re.compile (Re.str marker)) output in
  Alcotest.(check bool)
    (Fmt.str "source position with %S" setting)
    expected observed

let () =
  Test_support.with_directory (fun root ->
      List.iteri (check_output root)
        [
          ("true", true);
          ("false", false);
          ("invalid", true);
          (String.make 100 '?', true);
          ("", false);
          ("0", false);
          ("n", false);
          ("yes", true);
        ])
