let test_nice i = Alcotest.(check int) "Is it a nice integer?" 42 i

let number =
  let doc = "What is your preferred number?" in
  Cmdliner.Arg.(required & opt (some int) None & info [ "n" ] ~doc ~docv:"NUM")

let () =
  Alcotest.run_with_args "foo" number
    [ ("all", [ ("nice", `Quick, test_nice) ]) ]
