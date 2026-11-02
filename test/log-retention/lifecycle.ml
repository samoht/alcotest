module Deferred = struct
  type 'a t = unit -> 'a

  let return x () = x
  let bind action f () = f (action ()) ()
  let catch f on_error () = try f () () with exn -> on_error exn ()
end

module Platform = Alcotest.Unix_platform (Deferred)

module Runner =
  Alcotest_engine.V1.Core.Make_with_log_trap (Alcotest.Unix_platform) (Deferred)

(* The 1.9.1 maker still typechecks unchanged against the original functor. *)
module Old_platform : Alcotest_engine.Platform.MAKER = Alcotest.Unix_platform
module Old_runner = Alcotest_engine.V1.Core.Make (Old_platform) (Deferred)

let expect_exit f =
  match f () with
  | () -> Alcotest.fail "callback exception was lost"
  | exception Exit -> ()

module Run_id = Alcotest_engine.Private.Run_id

let check_identifier () =
  List.iter
    (fun input -> assert (Run_id.of_string input = None))
    [ ""; "ABC1234"; "ABC123456"; "abcd1234"; "ABCD/123"; "ABCD\000123" ];
  let accepted = "AZ019XYZ" in
  match Run_id.of_string accepted with
  | None -> Alcotest.fail "canonical identifier rejected"
  | Some id -> assert (Run_id.to_string id = accepted)

let check_names root =
  List.iteri
    (fun index (raw, escaped) ->
      let log_dir = Filename.concat root ("names-" ^ string_of_int index) in
      Alcotest.run ~argv:[| "names" |] ~and_exit:false ~log_dir raw
        [ (raw, [ Alcotest.test_case "escaped path" `Quick (fun () -> ()) ]) ];
      assert (
        Sys.file_exists
          (Filename.concat
             (Filename.concat log_dir escaped)
             (escaped ^ ".000.output"))))
    [
      ("../", "..-");
      ("\000", "U+0000");
      ("\206\169", "U+03A9");
      ("\255", "\239\191\189");
    ];
  assert (Fmt.str "%a" Alcotest_engine.Private.Pp.plural 0 = "");
  assert (Fmt.str "%a" Alcotest_engine.Private.Pp.plural 1 = "");
  assert (Fmt.str "%a" Alcotest_engine.Private.Pp.plural 2 = "s")

let run root =
  check_identifier ();
  check_names root;
  let nested_root = Filename.concat root "nested" in
  let first = Filename.concat nested_root "NESTED01" in
  Platform.with_log_trap ~root:nested_root ~uuid:"NESTED01" ~name:"nested"
    (fun () ->
      Platform.with_log_trap ~root:nested_root ~uuid:"NESTED02" ~name:"nested"
        (fun () -> Deferred.return ((), false))
        ();
      assert (Sys.is_directory first);
      Deferred.return ((), false))
    ();
  assert (not (Sys.file_exists first));
  assert (Sys.is_directory (Filename.concat nested_root "NESTED02"));
  let exception_root = Filename.concat root "exceptions" in
  let action =
    Platform.with_log_trap ~root:exception_root ~uuid:"EXCEPT01"
      ~name:"exceptions" (fun () -> fun () -> raise Exit)
  in
  assert (not (Sys.file_exists exception_root));
  expect_exit action;
  let failed = Filename.concat exception_root "EXCEPT01" in
  assert (Sys.is_directory failed);
  Platform.with_log_trap ~root:exception_root ~uuid:"EXCEPT02"
    ~name:"exceptions"
    (fun () -> Deferred.return ((), true))
    ();
  assert (not (Sys.file_exists failed));
  let value =
    Platform.with_log_trap ~root:exception_root ~uuid:"EXCEPT03"
      ~name:"exceptions"
      (fun () -> Deferred.return (42, false))
      ()
  in
  assert (value = 42);
  let deferred_root = Filename.concat root "deferred" in
  let action =
    Runner.run ~log_dir:deferred_root ~and_exit:false "deferred"
      [ ("case", [ Runner.test_case "deferred body" `Quick Deferred.return ]) ]
  in
  assert (not (Sys.file_exists deferred_root));
  action ();
  assert (Sys.is_directory deferred_root);
  Old_runner.run
    ~log_dir:(Filename.concat root "compatibility")
    ~and_exit:false "legacy maker"
    [ ("case", [ Old_runner.test_case "old contract" `Quick Deferred.return ]) ]
    ();
  let original_cwd = Sys.getcwd () in
  let relative_root = Filename.concat root "relative" in
  Unix.mkdir relative_root 0o700;
  Fun.protect
    ~finally:(fun () -> Sys.chdir original_cwd)
    (fun () ->
      Sys.chdir relative_root;
      Alcotest.run ~argv:[| "relative" |] ~log_dir:"logs" ~and_exit:false
        "relative"
        [
          ( "case",
            [
              Alcotest.test_case "change cwd" `Quick (fun () ->
                  Sys.chdir original_cwd);
              Alcotest.test_case "same resolved root" `Quick (fun () ->
                  print_endline "root remains bound");
            ] );
        ];
      assert (
        Sys.file_exists
          (Filename.concat relative_root "logs/relative/case.001.output")))

let () = Test_support.with_directory run
