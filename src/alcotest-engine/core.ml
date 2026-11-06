(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

include Core_intf
open! Import
open Model

exception Check_error of unit Fmt.t
exception Skip

let () =
  let print_error =
    (* We instantiate the error print buffer lazily, so as to be sensitive to
       [Fmt_tty.setup_std_outputs]. *)
    lazy
      (let buf = Buffer.create 0 in
       let ppf = Format.formatter_of_buffer buf in
       Fmt.set_style_renderer ppf
         Fmt.(style_renderer (Formatters.get_stderr () :> Format.formatter));
       Fmt.set_utf_8 ppf
         Fmt.(utf_8 (Formatters.get_stderr () :> Format.formatter));
       fun error ->
         Fmt.pf ppf "Alcotest assertion failure@.%a@." error ();
         let contents = Buffer.contents buf in
         Buffer.clear buf;
         contents)
  in
  Printexc.register_printer (function
    | Check_error err -> Some (Lazy.force print_error err)
    | _ -> None)

let frame_name slot = Option.value ~default:"" (Printexc.Slot.name slot)

let is_alcotest_frame slot =
  String.is_prefix ~affix:"Alcotest" (frame_name slot)

(* The compilation unit of a frame such as [Stdlib__List.iter]. *)
let frame_library slot =
  let before sep s =
    match String.cut ~sep s with Some (x, _) -> x | None -> s
  in
  before "__" (before "." (frame_name slot))

(* Library frames, which are not where a test failed. A frame without a name has
   no location to report. *)
let is_internal_frame slot =
  let library = frame_library slot in
  library = ""
  || List.mem library [ "Stdlib"; "Base"; "Core" ]
  || List.exists
       (fun affix -> String.is_prefix ~affix library)
       [ "Alcotest"; "Lwt"; "Async"; "Cmdliner" ]

(* Reading locations from [Printexc] slots, rather than parsing backtrace text,
   follows [Test_fmt.bt_slot_loc] in B0_testing by Daniel Bünzli:
   src/std/b0_testing.ml in https://github.com/b0-system/b0, copyright The b0
   programmers, ISC. *)
let frame_location slot =
  match Printexc.Slot.location slot with
  | Some { filename; line_number; start_char; _ }
    when not (is_internal_frame slot) ->
      Some (filename, line_number, start_char)
  | _ -> None

let first_location slots =
  match List.filter_map frame_location slots with
  | location :: _ -> Some location
  | [] -> None

(* The first caller outside Alcotest within [depth] frames, if the executable
   has debug information. *)
let caller_location ~depth =
  match Printexc.(backtrace_slots (get_callstack depth)) with
  | None -> None
  | Some slots -> first_location (Array.to_list slots)

let rec drop_alcotest_frames = function
  | slot :: slots when is_alcotest_frame slot -> drop_alcotest_frames slots
  | slots -> slots

(* The frames between the raise and Alcotest, innermost first: the last one with
   a location is in the test body, the others may be in libraries it calls. *)
let rec test_body_location acc = function
  | [] -> acc
  | slot :: _ when is_alcotest_frame slot -> acc
  | slot :: slots ->
      let acc =
        match frame_location slot with Some _ as l -> l | None -> acc
      in
      test_body_location acc slots

let backtrace_location () =
  match Printexc.(backtrace_slots (get_raw_backtrace ())) with
  | None -> None
  | Some slots ->
      test_body_location None (drop_alcotest_frames (Array.to_list slots))

(* The [test_case] tuple has no slot for its metadata, which is kept here and
   found by physical equality on the tuple: a function can be shared by several
   test cases. The hash is on the name, as a tuple can move in memory. *)
type case_info = {
  location : (string * int * int) option;
  children : Obj.t option;  (** The ['a test_case list] of a group. *)
}

module Case_infos = Map.Make (String)

(* Test cases can be created from any domain. *)
let case_infos = Atomic.make Case_infos.empty

let cases_named name infos =
  Option.value ~default:[] (Case_infos.find_opt name infos)

let find_case_info ((name, _, _) as case) =
  List.assq_opt (Obj.repr case) (cases_named name (Atomic.get case_infos))

let rec register_case_info ((name, _, _) as case) info =
  let infos = Atomic.get case_infos in
  let cases = (Obj.repr case, info) :: cases_named name infos in
  if
    not
      (Atomic.compare_and_set case_infos infos
         (Case_infos.add name cases infos))
  then register_case_info case info

let case_location case =
  match find_case_info case with None -> None | Some i -> i.location

let case_children case =
  match find_case_info case with None -> None | Some i -> i.children

(* A given position is printed first, see [Test.pp_location]. *)
let has_location pp = String.is_prefix ~affix:"File \"" (Fmt.str "%a" pp ())

(* See the documentation of this variable in [Cli]. It is read once, at startup:
   [Sys.getenv] raises when it is unset, which would overwrite the backtrace of
   the failure being reported. *)
let guess_location =
  match Sys.getenv "ALCOTEST_SOURCE_CODE_POSITION" with
  | "" | "false" | "no" | "n" | "0" -> false
  | _ | (exception Not_found) -> true

(* A failed check already shows where it failed if given a position. *)
let with_location location pp =
  match location with
  | Some l when not (has_location pp) -> Fmt.(const Pp.location l ++ pp)
  | _ -> pp

module Make (P : Platform.MAKER) (M : Monad.S) = struct
  module P = P (M)

  module Pp = struct
    include Pp
    include Pp.Make (P)
  end

  module M = Monad.Extend (M)
  module Suite = Suite (M)
  module Log_trap = Log_trap.Make (M) (P)
  include M.Syntax

  (* Types *)
  type return = unit M.t
  type 'a run = 'a -> unit M.t

  type tag =
    [ `Quick
    | `Slow
    | `Always
    | `Tag of string
    | `And of tag list
    | `Or of tag list
    | `Not of tag ]

  type speed_level = tag

  module Tag = struct
    let v s : tag = `Tag s
    let quick : tag = `Quick
    let slow : tag = `Slow
    let always : tag = `Always
    let ( && ) a b : tag = `And [ a; b ]
    let ( || ) a b : tag = `Or [ a; b ]
    let not a : tag = `Not a
  end

  exception Test_error

  type 'a test_case = string * tag * 'a run

  let test_case n t f =
    let case = (n, t, f) in
    register_case_info case
      { location = caller_location ~depth:16; children = None };
    case

  let test ?(tags = `Always) n f = test_case n tags f

  let run_cases cases args =
    List.fold_left
      (fun acc (_, _, f) -> M.bind acc (fun () -> f args))
      (M.return ()) cases

  (* The function only matters if the user rebuilds the tuple, which then runs
     the cases as a single test. *)
  let group ?(tags = `Always) name cases =
    if name = "" then
      Pp.user_error
        "Group name cannot be empty. Please pass a non-empty string to `group`.";
    let case = (name, tags, run_cases cases) in
    let children = Some (Obj.repr cases) in
    register_case_info case { location = None; children };
    case

  type 'a test = string * 'a test_case list

  let suite name cases = (name, cases)

  (* global state *)
  type 'a t = {
    (* library values. *)
    suite : 'a Suite.t;
    (* runtime state. *)
    mutable errors : unit Fmt.t list;
    (* runtime options. *)
    max_label : int;
        (** Longest test label in the suite, in UTF-8 characters. *)
    config : Config.t;
    run_id : string;
    log_trap : Log_trap.t;
    stdout : Formatters.stdout;
    stderr : Formatters.stderr;
    file : string option;
        (** The source file that calls [run], for the JUnit report. *)
  }

  let gen_run_id =
    let random_state = lazy (Random.State.make_self_init ()) in
    let random_hex _ =
      let state = Lazy.force random_state in
      match Random.State.int state 36 with
      | n when n < 10 -> Char.chr (n + Char.code '0')
      | n -> Char.chr (n - 10 + Char.code 'A')
    in
    fun () -> String.v ~len:8 random_hex

  let empty ~config ~trap_logs ~suite_name:unescaped_name =
    let errors = [] in
    let suite =
      match Suite.v ~name:unescaped_name with
      | Ok s -> s
      | Error `Empty_name ->
          Pp.user_error
            "Suite name cannot cannot be empty. Please pass a non-empty string \
             to `run`."
    in
    let max_label = 0 in
    let run_id = gen_run_id () in
    let log_trap =
      match trap_logs with
      | false -> Log_trap.inactive
      | true ->
          Log_trap.active ~root:config#log_dir ~uuid:run_id
            ~suite_name:(Suite.name suite)
    in
    let stdout = config#stdout in
    let stderr = config#stderr in
    {
      suite;
      errors;
      max_label;
      config;
      run_id;
      log_trap;
      stdout;
      stderr;
      file = None;
    }

  let pp_suite_results t =
    let log_dir =
      Option.map
        (fun _ -> Log_trap.pp_current_run_dir t.log_trap)
        (Log_trap.run_dir t.log_trap)
    in
    Pp.suite_results ~log_dir t.config

  let pp_event ~isatty ~prior_error ~tests_so_far t =
    let config = t.config in
    let selector_on_failure =
      (not prior_error) && not (config#verbose || config#show_errors)
    in
    if not config#json then
      Pp.event ~isatty ~compact:config#compact ~max_label:t.max_label
        ~doc_of_test_name:(Suite.doc_of_test_name t.suite)
        ~selector_on_failure ~tests_so_far
    else Fmt.nop

  let color c ppf fmt = Fmt.(styled c string) ppf fmt
  let red_s fmt = color `Red fmt
  let red ppf fmt = Fmt.kstr (fun str -> red_s ppf str) fmt

  let pp_error t ppf e =
    let path, error_fmt =
      match e with
      | `Error (p, f) -> (p, f)
      | `Exn (p, _, l, f) -> (p, with_location l f)
    in
    let pp_logs ppf () =
      let pp_logs =
        Log_trap.recover_logs ~tail:t.config#tail_errors t.log_trap path
      in
      match (t.config#verbose, pp_logs) with
      | true, _ | _, None -> Fmt.pf ppf "%a@," error_fmt ()
      | false, Some pp_logs ->
          let pp_log_dir =
            Pp.map_theta
              ~f:(fun s -> Pp.quoted (Fmt.styled `Cyan s))
              (Log_trap.pp_log_location t.log_trap path)
          in
          Fmt.pf ppf "%tLogs saved to %t.@," pp_logs pp_log_dir
    in
    Fmt.(
      Pp.with_surrounding_box
        (const
           (Pp.event_line ~margins:3 ~max_label:t.max_label
              ~doc_of_test_name:(Suite.doc_of_test_name t.suite))
           (`Result (path, e)))
      ++ pp_logs
      ++ Pp.horizontal_rule
      ++ cut)
      ppf ()

  let has_run : Run_result.t -> bool = function
    | `Ok | `Error _ | `Exn _ -> true
    | `Skip | `Todo _ -> false

  let bt () = match Printexc.get_backtrace () with "" -> "" | s -> "\n" ^ s

  let exn path name location pp =
    `Exn (path, name, location, Fmt.(pp ++ const lines (bt ())))

  let result_of_exn ~registered path e =
    let location =
      if guess_location then Option.(backtrace_location () || registered)
      else None
    in
    match e with
    | Check_error err ->
        `Error (path, with_location location Fmt.(err ++ const string (bt ())))
    | Skip -> `Skip
    | Failure s -> exn path "failure" location Fmt.(const string s)
    | Invalid_argument s -> exn path "invalid" location Fmt.(const string s)
    | e -> exn path "exception" location Fmt.(const exn e)

  let protect_test ~registered path (f : 'a run) : 'a -> Run_result.t M.t =
   fun args ->
    M.catch
      (fun () -> f args >|= fun () -> `Ok)
      (fun e -> M.return (result_of_exn ~registered path e))

  type running_state = { tests_so_far : int; first_error : int option }
  (** State that is kept during the test executions. *)

  let with_captured_logs t name fn args =
    if t.config#verbose then fn args
    else
      Log_trap.with_captured_logs t.log_trap name
        (fun () ->
          (* When capturing the logs of a test, also add the result of the test
             at the end. *)
          let+ result = fn args in
          Pp.rresult_error (t.stdout :> Format.formatter) result;
          result)
        ()

  let perform_test t args { tests_so_far; first_error }
      (test : _ Suite.test_case) =
    let open Suite in
    let print_event =
      pp_event t
        ~prior_error:(Option.is_some first_error)
        ~tests_so_far ~isatty:(P.stdout_isatty ())
        (t.stdout :> Format.formatter)
    in
    let* () = M.return () in
    print_event (`Start test.name);
    let+ result, errored =
      match test.fn with
      | `Skip -> M.return (`Skip, false)
      | `Run fn ->
          Fmt.(flush (t.stdout :> Format.formatter))
            () (* Show event before any test stderr *);
          let+ result = with_captured_logs t test.name fn args in
          (* Store errors *)
          let errored : bool =
            let error, errored =
              match result with
              | (`Error _ | `Exn _) as e -> ([ Fmt.const (pp_error t) e ], true)
              | _ -> ([], false)
            in
            t.errors <- error @ t.errors;
            errored
          in
          (* Show any remaining test output before the event *)
          Fmt.(flush (t.stdout :> Format.formatter) ());
          Fmt.(flush (t.stderr :> Format.formatter) ());
          (result, errored)
    in
    print_event (`Result (test.name, result));
    let error = if errored then Some tests_so_far else None in
    let state =
      {
        tests_so_far = tests_so_far + 1;
        first_error = Option.(first_error || error);
      }
    in
    (state, result)

  (* [running] is the test that runs, which the report marks as stopped if the
     process exits during it. *)
  type junit = {
    path : string option;
    mutable report : Junit.t;
    mutable running : (unit -> Junit.case) option;
  }

  let junit_write t j =
    let report =
      Option.fold ~none:j.report
        ~some:(fun c -> Junit.add j.report (c ()))
        j.running
    in
    try Option.iter (fun p -> P.write_file p (Junit.to_string report)) j.path
    with Sys_error msg ->
      Fmt.pf
        (t.stderr :> Format.formatter)
        "cannot write the JUnit report: %s@." msg

  (* The reports of all runs are in one directory, so that tools can read them
     together. The report of the run in [run/<name>/<time>-<id>] is
     [junit/<name>-<time>-<id>.xml]. *)
  let junit_file t dir =
    let file =
      Fmt.str "%s-%s.xml" (Suite.name t.suite) (Filename.basename dir)
    in
    List.fold_left Filename.concat t.config#log_dir [ "junit"; file ]

  let junit_open t =
    let path = Option.map (junit_file t) (Log_trap.run_dir t.log_trap) in
    let suite = Fmt.to_to_string Suite.pp_name t.suite in
    let report =
      Junit.v ~suite ~file:t.file ~timestamp:(P.timestamp (P.time ()))
    in
    let j = { path; report; running = None } in
    at_exit (fun () -> junit_write t j);
    j

  let junit_outcome (result : Run_result.t) : Junit.outcome =
    (* The console prints this in a box too, which keeps the location on its own
       line. *)
    let text = Fmt.str "%a" (Fmt.vbox Pp.rresult_error) result in
    match result with
    | `Ok -> Passed
    | `Skip -> Skipped
    | `Error _ -> Failed { kind = "Check_error"; text }
    | `Todo _ -> Failed { kind = "todo"; text }
    | `Exn (_, kind, _, _) -> Errored { kind; text }

  let junit_output t name (result : Run_result.t) =
    if Run_result.is_failure result then
      Log_trap.recover_logs ~tail:t.config#tail_errors t.log_trap name
      |> Option.map (Fmt.str "%t")
    else None

  let junit_case t (test : _ Suite.test_case) result ~time : Junit.case =
    let doc = Suite.doc_of_test_name t.suite test.name in
    {
      group = Safe_string.to_unescaped_string (Test_name.name test.name);
      name =
        (if String.is_suffix ~affix:"." doc then
           String.with_range ~len:(String.length doc - 1) doc
         else doc);
      index = Test_name.index test.name;
      time;
      location = Option.map (fun (f, l, _) -> (f, l)) test.location;
      outcome = junit_outcome result;
      output = junit_output t test.name result;
    }

  let junit_running t j (test : _ Suite.test_case) =
    match test.fn with
    | `Skip -> ()
    | `Run _ ->
        let msg = "The process stopped while this test was running." in
        let crash = `Exn (test.name, "crash", None, Fmt.(const string msg)) in
        j.running <- Some (fun () -> junit_case t test crash ~time:0.)

  let junit_finished t j test result ~time =
    j.report <- Junit.add j.report (junit_case t test result ~time);
    j.running <- None

  let perform_tests t junit tests args =
    let currently_bailing acc =
      Option.is_some acc.first_error && t.config#bail
    in
    let+ state, test_results =
      M.List.fold_map_s
        (fun acc test ->
          if currently_bailing acc then (
            junit_finished t junit test `Skip ~time:0.;
            M.return ({ acc with tests_so_far = succ acc.tests_so_far }, `Skip))
          else (
            junit_running t junit test;
            let start_time = P.time () in
            let+ acc, result = perform_test t args acc test in
            junit_finished t junit test result ~time:(P.time () -. start_time);
            (acc, result)))
        { tests_so_far = 0; first_error = None }
        tests
    in
    let () =
      if currently_bailing state then
        match state.tests_so_far - Option.get state.first_error - 1 with
        | n when n > 0 ->
            Formatters.pr "@\n  %a@\n"
              Fmt.(styled `Faint string)
              (Fmt.str "... with %d subsequent test%a skipped." n Pp.pp_plural n)
        | 0 -> ()
        | _ -> assert false
    in
    test_results

  let skip_label test_case = Suite.{ test_case with fn = `Skip }

  let filter_test_case p test_case =
    match p with
    | None -> true
    | Some p -> (
        let name, index =
          let tn = test_case.Suite.name in
          Test_name.(Safe_string.to_unescaped_string (name tn), index tn)
        in
        match p ~name ~index with `Run -> true | `Skip -> false)

  let filter_test_cases ~subst path test_cases =
    let filter_test_case = filter_test_case path in
    test_cases
    |> List.filter_map (fun tc ->
        if filter_test_case tc then Some tc
        else if subst then Some (skip_label tc)
        else None)

  let apply_tag context s =
    if String.is_prefix ~affix:"-" s then
      let removed = String.with_range ~first:1 s in
      List.filter (fun t -> t <> removed) context
    else s :: context

  let context t =
    let base =
      if t.config#quick_only then [ "quick" ] else [ "quick"; "slow" ]
    in
    List.fold_left apply_tag base t.config#tags

  let rec holds context = function
    | `Quick -> List.mem "quick" context
    | `Slow -> List.mem "slow" context
    | `Always -> true
    | `Tag s -> List.mem s context
    | `And l -> List.for_all (holds context) l
    | `Or l -> List.exists (holds context) l
    | `Not c -> not (holds context c)

  let is_enabled t test = holds (context t) test.Suite.condition

  let select_condition t (test_case : 'a Suite.test_case as 'tc) : 'tc =
    if is_enabled t test_case then test_case else skip_label test_case

  let result t test args =
    let initial_backtrace_status = Printexc.backtrace_status () in
    if t.config#record_backtrace then Printexc.record_backtrace true;
    let start_time = P.time () in
    let test = List.map (select_condition t) test in
    let junit = junit_open t in
    let+ results = perform_tests t junit test args in
    junit_write t junit;
    let time = P.time () -. start_time in
    let failures = List.length (List.filter Run_result.is_failure results) in
    let success = List.length (List.filter has_run results) - failures in
    if t.config#record_backtrace then
      Printexc.record_backtrace initial_backtrace_status;
    Pp.{ time; success; failures; errors = List.rev t.errors }

  let rec pp_condition ppf = function
    | `Quick -> Fmt.string ppf "quick"
    | `Slow -> Fmt.string ppf "slow"
    | `Always -> Fmt.string ppf "always"
    | `Tag s -> Fmt.string ppf s
    | `And l -> pp_conditions " and " ppf l
    | `Or l -> pp_conditions " or " ppf l
    | `Not c -> Fmt.pf ppf "not %a" pp_operand c

  and pp_conditions sep ppf =
    Fmt.(list ~sep:(fun ppf () -> string ppf sep) pp_operand) ppf

  and pp_operand ppf = function
    | (`And _ | `Or _) as c -> Fmt.pf ppf "(%a)" pp_condition c
    | c -> pp_condition ppf c

  (* The conditions of existing tests are not worth listing. *)
  let doc_with_condition condition doc =
    match condition with
    | `Quick | `Slow -> doc
    | c -> Fmt.str "[%a] %s" pp_condition c doc

  let pp_listed_test ~max_label suite ppf (test : _ Suite.test_case) =
    let doc = Suite.doc_of_test_name suite test.name in
    let doc = doc_with_condition test.condition doc in
    Pp.info ~max_label ~doc_of_test_name:(fun _ -> doc) ppf test.name

  let list_registered_tests t () =
    Suite.tests t.suite
    |> List.sort (fun a b -> Test_name.compare a.Suite.name b.Suite.name)
    |> Fmt.(
         list ~sep:(const string "\n")
           (pp_listed_test ~max_label:t.max_label t.suite)
           (t.stdout :> Format.formatter))

  type 'a entry = {
    entry_name : string;
    index : int;
    doc : string;
    tag : tag;
    location : (string * int * int) option;
    run : 'a run;
  }

  (* The entries of a test, in the order of its cases. A name can come back after
     a group, so each name keeps its own count. *)
  let compile (name, cases) =
    let counts = Hashtbl.create 8 in
    let next_index name =
      let index = Option.value ~default:0 (Hashtbl.find_opt counts name) in
      Hashtbl.replace counts name (index + 1);
      index
    in
    let rec entries name parent acc cases =
      List.fold_left
        (fun acc ((doc, tag, run) as case) ->
          let tag =
            match parent with `Always -> tag | _ -> Tag.(parent && tag)
          in
          match case_children case with
          | Some children ->
              (* A group and its cases have the same ['a]. *)
              entries (name ^ "." ^ doc) tag acc (Obj.obj children)
          | None ->
              let location = case_location case in
              let index = next_index name in
              { entry_name = name; index; doc; tag; location; run } :: acc)
        acc cases
    in
    List.rev (entries name `Always [] cases)

  let add_entry (type a) (t : a t) (e : a entry) : a t =
    let path = Test_name.v ~name:e.entry_name ~index:e.index in
    let doc =
      if e.doc = "" || e.doc.[String.length e.doc - 1] = '.' then e.doc
      else e.doc ^ "."
    in
    let test a = protect_test ~registered:e.location path e.run a in
    match Suite.add t.suite (path, doc, e.tag, e.location, `Run test) with
    | Ok suite ->
        let max_label = max t.max_label (String.length_utf8 e.entry_name) in
        { t with suite; max_label }
    | Error (`Duplicate_test_path path) ->
        Fmt.kstr Pp.user_error "Duplicate test path: `%s'" path

  let register (type a) (t : a t) ((name, _) as test : a test) : a t =
    let t = { t with max_label = max t.max_label (String.length_utf8 name) } in
    List.fold_left add_entry t (compile test)

  let register_all t cases = List.fold_left register t cases

  let nothing_selected t matching =
    t.config#tags <> []
    && matching <> []
    && not (List.exists (is_enabled t) matching)

  let rec condition_tags = function
    | `Quick -> [ "quick" ]
    | `Slow -> [ "slow" ]
    | `Always -> []
    | `Tag s -> [ s ]
    | `And l | `Or l -> List.concat_map condition_tags l
    | `Not c -> condition_tags c

  let suite_tags suite =
    List.concat_map (fun x -> condition_tags x.Suite.condition) suite
    |> List.sort_uniq compare

  let available_tags suite =
    List.sort_uniq compare ("quick" :: "slow" :: suite_tags suite)

  let tag_name s =
    if String.is_prefix ~affix:"-" s then String.with_range ~first:1 s else s

  let check_tags t suite =
    if t.config#strict_tags then
      let available = available_tags suite in
      let is_unknown s = s <> "" && not (List.mem (tag_name s) available) in
      match List.filter is_unknown t.config#tags with
      | [] -> ()
      | unknown ->
          Fmt.kstr Pp.user_error "Unknown tags `%s'. Available tags: %s."
            (String.concat ~sep:"," unknown)
            (String.concat ~sep:", " available)

  let warn_nothing_selected t suite =
    Fmt.flush (t.stdout :> Format.formatter) ();
    Fmt.kstr Pp.user_warning
      "tags `%s' enable no tests. Tests use the tags: %s."
      (String.concat ~sep:"," t.config#tags)
      (String.concat ~sep:", " (suite_tags suite))

  let run_tests t () args =
    let filter = t.config#filter in
    let suite = Suite.tests t.suite in
    check_tags t suite;
    let matching = filter_test_cases ~subst:false filter suite in
    let+ result =
      if matching = [] && Option.is_some filter then (
        (* NOTE(dinosaure): [Stdlib.flush_all] is really deep in OCaml and try to flush
           all opened file descriptors (including [1] and [2]). Even if the user create
           its own [Format.formatter], if it uses a file-descriptor, it will be flushed
           too. We don't need to register a channel even if the user specify its own
           [Format.formatter] for [stdout] and/or [stderr]. *)
        flush_all ();
        Fmt.(
          pf
            (Formatters.get_stderr () :> Format.formatter)
            "%a\n%!" red
            "Invalid request (no tests to run, filter skipped everything)!");
        exit 1)
      else
        let tests = filter_test_cases ~subst:true filter suite in
        result t tests args
    in
    (pp_suite_results t) (t.stdout :> Format.formatter) result;
    if nothing_selected t matching then warn_nothing_selected t suite;
    result.failures

  (* Under dune, the current directory is in [_build/default], where dune deletes
     the files that no rule produced: the logs go to the workspace instead. *)
  let default_log_dir () =
    let root =
      match Sys.getenv_opt "DUNE_SOURCEROOT" with
      | Some root when not (Filename.is_relative root) -> root
      | _ -> P.getcwd ()
    in
    List.fold_left Filename.concat root [ "_build"; "_tests" ]

  type 'a with_options = 'a Config.with_options

  let registered (type a) (tl : a test list) =
    (* TODO: refactor [register_all] to not require dummy state *)
    let config =
      Config.apply_defaults ~default_log_dir:"<not-shown-to-user>"
        (Config.User.create ())
    in
    register_all
      (empty ~config ~trap_logs:false ~suite_name:"<not-shown-to-user>")
      tl

  let list_tests tl =
    list_registered_tests (registered tl) ();
    M.return ()

  let list_tags tl =
    let t = registered tl in
    List.iter
      (Fmt.pf (t.stdout :> Format.formatter) "%s@.")
      (available_tags (Suite.tests t.suite));
    M.return ()

  let run_with_args' (config : Config.User.t) name (type a) (args : a)
      (tl : a test list) =
    let config =
      Config.apply_defaults ~default_log_dir:(default_log_dir ()) config
    in
    let t = empty ~config ~trap_logs:P.log_trap_supported ~suite_name:name in
    (* Read now: with a concurrency monad, the report is opened later, from a
       call stack that no longer reaches the caller. Async calls [run] from its
       scheduler, about 30 frames away from the caller. *)
    let file = Option.map (fun (f, _, _) -> f) (caller_location ~depth:64) in
    let t = register_all { t with file } tl in
    let stdout' = Formatters.get_stdout () in
    let stderr' = Formatters.get_stderr () in
    Formatters.set_stdout t.stdout;
    Formatters.set_stderr t.stderr;
    let+ test_failures =
      (* Only print inside the concurrency monad *)
      let* () = M.return () in
      let open Fmt in
      if config#ci = `Github_actions then
        Formatters.pr "::group::{%a}\n" Suite.pp_name t.suite;
      Formatters.pr "Testing %a.@,"
        (Pp.quoted Fmt.(styled `Bold Suite.pp_name))
        t.suite;
      Formatters.pr "@[<v>%a@]"
        (styled `Faint (fun ppf () ->
             pf ppf "This run has ID %a.@,@," (Pp.quoted string) t.run_id))
        ();
      let r = run_tests t () args in
      if config#ci = `Github_actions then Formatters.pr "::endgroup::\n";
      r
    in
    at_exit
      (Format.pp_print_flush (Formatters.get_stderr () :> Format.formatter));
    Formatters.set_stdout stdout';
    Formatters.set_stderr stderr';
    match (test_failures, t.config#and_exit) with
    | 0, true -> exit 0
    | 0, false -> ()
    | _, true -> exit 1
    | _, false -> raise Test_error

  let run' config name (tl : unit test list) = run_with_args' config name () tl

  let run_with_args ?stdout ?stderr ?and_exit ?verbose ?compact ?tail_errors
      ?quick_only ?show_errors ?json ?filter ?log_dir ?bail ?record_backtrace
      ?ci =
    Config.User.kcreate run_with_args' ?stdout ?stderr ?and_exit ?verbose
      ?compact ?tail_errors ?quick_only ?show_errors ?json ?filter ?log_dir
      ?bail ?record_backtrace ?ci

  let run = Config.User.kcreate run'
end

module V1 = struct
  include V1_types
  module Make = Make

  exception Skip = Skip
end
