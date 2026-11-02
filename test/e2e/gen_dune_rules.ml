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

(* Use the compiler's dependency collector rather than duplicate the
   fixture's imports in per-case library lists. *)
let libraries_of_file ~libraries filename =
  let channel = open_in filename in
  let structure =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () ->
        let lexbuf = Lexing.from_channel channel in
        Location.init lexbuf filename;
        Parse.implementation lexbuf)
  in
  Depend.free_structure_names := Depend.String.Set.empty;
  Depend.add_implementation Depend.String.Map.empty structure;
  let imports = !Depend.free_structure_names in
  List.filter
    (fun library ->
      let root =
        String.map (function '.' -> '_' | c -> c) library
        |> String.capitalize_ascii
      in
      Depend.String.Set.mem root imports)
    ([ "alcotest"; "alcotest.stdlib_ext"; "fmt" ] @ libraries)

(* Each fixture is a real Dune test: its executable, options, sanitizer and
   exact transcript comparison belong to one action and one test stanza. *)
let example_stanza ~package ~expect_failure ~libraries filename =
  let base = chop_extension filename in
  let libraries = libraries_of_file ~libraries filename in
  let options = options_of_test_file filename |> List.map (( ^ ) " ") in
  let accepted_exit_codes =
    Fmt.str "(or %s %d %d)"
      (if expect_failure then
         (* 1 = failing test, 2 = failed assertion outside runner *)
         "1 2"
       else "0")
      Cmdliner.Cmd.Exit.cli_error Cmdliner.Cmd.Exit.internal_error
  in
  Fmt.pr
    {|(test
 (name %s)
 (package %s)
 (libraries %a)
 (modules %s)
 (modes exe)
 (deps %s.expected)
 (action
  (progn
   (with-outputs-to %s.actual
    (with-accepted-exit-codes %s
     (run %%{test}%a)))
   (run %%{dep:../../strip_randomness.exe} %s.actual))))
|}
    base package
    Fmt.(list ~sep:(const string " ") string)
    libraries base base base accepted_exit_codes
    Fmt.(list string)
    options base

let is_example filename = Filename.check_suffix filename ".ml"

let main package expect_failure libraries =
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_example
  |> function
  | [] -> () (* no tests to execute *)
  | tests ->
      List.iteri
        (fun i test ->
          if i > 0 then Fmt.pr "\n";
          example_stanza ~package ~expect_failure ~libraries test)
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
    Arg.info
      ~doc:"Additional libraries selected when their root module is imported"
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
