let chop_extension = Filename.chop_extension

(* Read a file-path to a list of strings corresponding to lines in the file. *)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    assert false
  with End_of_file ->
    close_in chan;
    List.rev !lines

(* A test executable [foo.ml] may require certain options to be passed as
   specified in a [foo.opts] file. *)
let options_of_test_file file =
  let options_file = chop_extension file ^ ".opts" in
  if not (Sys.file_exists options_file) then [] else read_file options_file

let global_stanza ~libraries filenames =
  let bases = List.map chop_extension filenames in
  let libraries = List.map (( ^ ) " ") libraries in
  let pp_sexp_list = Fmt.(list ~sep:(const string "\n   ")) in
  Fmt.pr
    {|(executables
 (names
   %a
 )
 (libraries alcotest alcotest.stdlib_ext%a)
 %s
 (modules
   %a
 )
)
|}
    (pp_sexp_list Fmt.string) bases
    Fmt.(list string)
    libraries "(modes exe)" (pp_sexp_list Fmt.string) bases

let example_rule_stanza ~expect_failure filename =
  let base = chop_extension filename in
  let options = options_of_test_file filename |> List.map (( ^ ) " ") in
  let accepted_exit_codes =
    Fmt.str "(or %s %d %d)"
      (if expect_failure then
         (* 1 = failing test, 2 = failed assertion outside runner *)
         "1 2"
       else "0")
      Cmdliner.Cmd.Exit.cli_error Cmdliner.Cmd.Exit.internal_error
  in
  (* Run Alcotest to get *.actual, then pass through the strip_randomness
     sanitiser to get *.processed. *)
  Fmt.pr
    {|
(rule
 (target %s.actual)
 (action
  (with-outputs-to %%{target}
   (with-accepted-exit-codes %s
    (run %s%a)))))
|}
    base accepted_exit_codes
    (Printf.sprintf "%%{dep:%s.exe}" base)
    Fmt.(list string)
    options;

  Fmt.pr
    {|
(rule
 (target %s.processed)
 (action
  (with-outputs-to %%{target}
   (run ../../strip_randomness.exe %%{dep:%s.actual}))))
|}
    base base

let example_alias_stanza ~package filename =
  let base = chop_extension filename in
  Fmt.pr
    {|
(rule
 (alias %s)
 (package %s)
 (action
   (diff %s.expected %s.processed)))
|}
    "runtest" package base base

let is_example filename = Filename.check_suffix filename ".ml"

let main package expect_failure libraries =
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_example
  |> function
  | [] -> () (* no tests to execute *)
  | tests ->
      global_stanza ~libraries tests;
      List.iter
        (fun test ->
          example_rule_stanza ~expect_failure test;
          example_alias_stanza ~package test)
        tests

open Cmdliner

let package =
  let doc =
    Arg.info ~doc:"Package with which to associate the executables"
      [ "package" ]
  in
  Arg.(required & opt (some string) None & doc)

let libraries =
  let doc =
    Arg.info ~doc:"Additional libraries to make available to the executable"
      [ "libraries" ]
  in
  Arg.(value & opt (list string) [] & doc)

let expect_failure =
  let doc =
    Arg.info ~doc:"Negate the return status of the tests" [ "expect-failure" ]
  in
  Arg.(value & flag doc)

let cmd =
  let info = Cmd.info ~version:"%%VERSION%%" "gen_dune_rules" in
  Cmd.v info Term.(const main $ package $ expect_failure $ libraries)

let () = exit @@ Cmd.eval cmd
