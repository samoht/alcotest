module Support = Test_support

let ( / ) = Filename.concat
let check condition message = if not condition then failwith message
let entries path = Sys.readdir path |> Array.to_list |> List.sort String.compare

let exists path =
  try
    ignore (Unix.lstat path);
    true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let rec resolve path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_LNK ->
      let target = Unix.readlink path in
      resolve
        (if Filename.is_relative target then Filename.dirname path / target
         else target)
  | _ -> path

let contents path =
  List.map (fun name -> (name, Support.read (path / name))) (entries path)

let status expected actual output =
  match actual with
  | Unix.WEXITED code when code = expected -> ()
  | _ -> Fmt.failwith "unexpected helper exit: %s" output

let run runner root suite mode payload =
  let output = Filename.dirname root / "command.log" in
  let result, diagnostic =
    Support.run ~output runner [ root; suite; mode; payload ]
  in
  (result, diagnostic)

let invoke runner root suite mode payload =
  let result, diagnostic = run runner root suite mode payload in
  status
    (match mode with "fail" -> 1 | "crash" -> 7 | _ -> 0)
    result diagnostic;
  resolve (root / suite)

let completed runner root =
  let suite = invoke runner root "suite" in
  let first = suite "pass" "obsolete" in
  let latest = suite "pass" "latest" in
  check (not (exists first)) ("obsolete successful run retained: " ^ first);
  check
    (Support.read (latest / "case.000.output") = "latest\n")
    "latest payload changed";
  let failure = suite "fail" "first failure" in
  let diagnostic = contents failure in
  let latest = suite "pass" "after failure" in
  check (contents failure = diagnostic) "last failure changed after success";
  let other = invoke runner root "other" "pass" "independent suite" in
  let replacement = suite "fail" "replacement failure" in
  check (not (exists failure)) "superseded failure retained";
  check (not (exists latest)) "superseded success retained";
  check (exists other && exists replacement) "another suite lost its logs"

let active ~failed runner root =
  let ready = Filename.dirname root / ("ready-" ^ Filename.basename root) in
  Unix.mkfifo ready 0o600;
  let descriptor = Unix.openfile ready [ Unix.O_RDWR; Unix.O_NONBLOCK ] 0 in
  Unix.set_close_on_exec descriptor;
  let input, writer = Unix.pipe () in
  Unix.set_close_on_exec input;
  Unix.set_close_on_exec writer;
  let writer = ref (Some writer) in
  let release () =
    match !writer with
    | None -> ()
    | Some fd ->
        Unix.close fd;
        writer := None
  in
  let output =
    Filename.dirname root / ("active-" ^ Filename.basename root ^ ".log")
  in
  let pid =
    Support.spawn ~input ~output runner
      [
        root;
        "shared";
        (if failed then "hold-fail" else "hold");
        "active writer";
        ready;
      ]
  in
  Unix.close input;
  let reaped = ref false in
  let reap () =
    if not !reaped then (
      reaped := true;
      Some (Support.wait pid))
    else None
  in
  Fun.protect
    ~finally:(fun () ->
      release ();
      ignore (reap ());
      Unix.close descriptor)
    (fun () ->
      let readable, _, _ = Unix.select [ descriptor ] [] [] 20. in
      check (readable <> []) "runner did not start";
      let buffer = Bytes.create 16 in
      let count = Unix.read descriptor buffer 0 16 in
      check
        (Bytes.sub_string buffer 0 count = "ready\n")
        "invalid readiness event";
      let first = resolve (root / "shared") in
      let latest =
        invoke runner root "shared" "pass" "completed during active run"
      in
      check (exists first) "active runner's exact directory was removed";
      release ();
      (match reap () with
      | Some result ->
          status (if failed then 1 else 0) result (Support.read output)
      | None -> assert false);
      let diagnostic = Support.read output in
      check
        (Astring.String.is_infix ~affix:first diagnostic)
        "concurrent suite alias redirected the completed run diagnostic";
      if failed then (
        check (exists first) "concurrent completed failure was removed";
        check
          (Astring.String.is_infix
             ~affix:(first / "case.000.output")
             diagnostic)
          "concurrent suite alias redirected the failure log location")
      else check (not (exists first)) "completed superseded active run retained";
      check (exists latest) "concurrent latest run removed")

let legacy runner root =
  Unix.mkdir root 0o700;
  let old = root / "OLD12345" in
  Unix.mkdir old 0o700;
  Support.write (old / "case.000.output") "legacy writer's output";
  let saved = contents old in
  ignore (invoke runner root "new" "pass" "payload");
  ignore (invoke runner root "new" "pass" "payload");
  check (contents old = saved) "unowned legacy output was changed"

let refusal runner root =
  let failure = invoke runner root "guard" "fail" "retained failure" in
  let latest = invoke runner root "guard" "pass" "retained success" in
  let sentinel = Filename.dirname root / "outside" in
  Support.write sentinel "outside ownership";
  Unix.symlink sentinel (latest / "foreign.output");
  let before = entries root and saved = contents failure in
  for _ = 1 to 2 do
    let result, diagnostic = run runner root "guard" "pass" "must refuse" in
    check (result <> Unix.WEXITED 0) "unowned symlink was accepted for deletion";
    check
      (Astring.String.is_infix ~affix:"foreign.output" diagnostic)
      (Fmt.str "expected foreign.output refusal, observed %S" diagnostic);
    check (entries root = before) "refusal allocated a run";
    check (contents failure = saved) "refusal partially retired logs";
    check
      (Support.read sentinel = "outside ownership")
      "outside sentinel changed";
    check
      ((Unix.lstat (latest / "foreign.output")).Unix.st_kind = Unix.S_LNK)
      "foreign link removed"
  done;
  Unix.unlink (latest / "foreign.output");
  Support.write (latest / "unrecognized") "not a framework output";
  let result, diagnostic = run runner root "guard" "pass" "must refuse" in
  check
    (result <> Unix.WEXITED 0
    && Astring.String.is_infix ~affix:"unrecognized" diagnostic)
    "unknown file accepted";
  check (entries root = before) "refusal allocated a run";
  Unix.unlink (latest / "unrecognized");
  Unix.chmod latest 0o500;
  Fun.protect
    ~finally:(fun () -> Unix.chmod latest 0o700)
    (fun () ->
      let result, _ = run runner root "guard" "pass" "must refuse" in
      check (result <> Unix.WEXITED 0)
        "read-only output accepted for retirement";
      check
        (entries root = before && contents failure = saved)
        "permission refusal changed retained logs")

let bound runner root =
  List.iter
    (fun count ->
      for _ = 1 to count do
        ignore (invoke runner root "bounded" "pass" "fixed payload")
      done;
      let runs = entries (root / ".alcotest" / "runs") in
      let files =
        List.concat
          (List.map
             (fun id ->
               List.map (fun name -> root / id / name) (entries (root / id)))
             runs)
      in
      check (List.length runs = 1) "completed run count grew";
      check (List.length files = 1) "completed output count grew";
      let bytes =
        List.fold_left
          (fun total file -> total + (Unix.stat file).Unix.st_size)
          0 files
      in
      Fmt.pr
        "%d completed runs: %d registered run, %d output file, %d log bytes@."
        count (List.length runs) (List.length files) bytes)
    [ 10; 100 ]

let () =
  Printexc.record_backtrace true;
  let runner = Sys.argv.(1) in
  Support.with_directory (fun base ->
      completed runner (base / "completed");
      active ~failed:false runner (base / "active");
      active ~failed:true runner (base / "active-failure");
      let crashed =
        invoke runner (base / "crash") "crash" "crash" "interrupted run"
      in
      ignore (invoke runner (base / "crash") "crash" "pass" "payload");
      check (not (exists crashed)) "dead process kept its kernel lease";
      legacy runner (base / "legacy");
      refusal runner (base / "guard");
      bound runner (base / "bound"));
  Fmt.pr
    "log retention: completed, failure, concurrent, legacy and refusal cases \
     passed@."
