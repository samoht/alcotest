(* The explicit test command preserves the caller options of Cli.S.run. *)
let () =
  Printexc.record_backtrace false;
  let during = ref None in
  Alcotest.run ~and_exit:false ~record_backtrace:false
    ~argv:[| "explicit-options"; "test" |] __FILE__
    [
      ( "options",
        [
          Alcotest.test_case "observe the caller's backtrace setting" `Quick
            (fun () -> during := Some (Printexc.backtrace_status ()));
        ] );
    ];
  Alcotest.(check (option bool))
    "caller backtrace setting inside test" (Some false) !during;
  Alcotest.(check bool)
    "caller backtrace setting after command" false
    (Printexc.backtrace_status ());
  print_endline "Explicit test command returned to its caller."
