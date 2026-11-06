(* Compiled to JavaScript the C stubs and most of Unix are absent: tests print
   straight to the output, with no log store and no terminal size. *)
let native =
  match Sys.backend_type with Native | Bytecode -> true | Other _ -> false

module Unix_platform (M : Alcotest_engine.Monad.S) = struct
  module M = Alcotest_engine.Monad.Extend (M)
  open M.Syntax

  let time = Unix.gettimeofday
  let getcwd = Sys.getcwd
  let prepare_log_trap = Log_store.prepare_legacy

  let with_log_trap ~root ~uuid ~name f =
    let* () = M.return () in
    if Sys.win32 then (
      prepare_log_trap ~root ~uuid ~name;
      let+ value, _ = f () in
      value)
    else
      let owner = Log_store.start ~root ~uuid ~name in
      let* outcome =
        M.catch
          (fun () ->
            let+ result = f () in
            Ok result)
          (fun exn -> M.return (Error (exn, Printexc.get_raw_backtrace ())))
      in
      let failed =
        match outcome with Ok (_, failed) -> failed | Error _ -> true
      in
      Log_store.finish owner ~failed;
      match outcome with
      | Ok (value, _) -> M.return value
      | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace

  let stdout_isatty () = Unix.(isatty stdout)

  let stdout_columns () =
    match Option.bind (Sys.getenv_opt "ALCOTEST_COLUMNS") int_of_string_opt with
    | Some columns when columns > 0 -> Some columns
    | _ -> (
        match if native then Terminal.dimensions () else None with
        | Some { columns; _ } when columns > 0 -> Some columns
        | _ -> None)

  external before_test :
    output:out_channel -> stdout:out_channel -> stderr:out_channel -> unit
    = "alcotest_before_test"

  external after_test : stdout:out_channel -> stderr:out_channel -> unit
    = "alcotest_after_test"

  type file_descriptor = out_channel

  let log_trap_supported = native
  let file_exists = Sys.file_exists
  let open_write_only = open_out
  let close = close_out

  let with_redirect fd_file fn =
    let* () = M.return () in
    Fmt.flush (Alcotest_engine.Formatters.get_stdout () :> Format.formatter) ();
    Fmt.flush (Alcotest_engine.Formatters.get_stderr () :> Format.formatter) ();
    before_test ~output:fd_file ~stdout:Stdlib.stdout ~stderr:Stdlib.stderr;
    let+ r =
      M.catch
        (fun () ->
          let+ value = fn () in
          Ok value)
        (fun exn -> M.return (Error (exn, Printexc.get_raw_backtrace ())))
    in
    Fmt.flush (Alcotest_engine.Formatters.get_stdout () :> Format.formatter) ();
    Fmt.flush (Alcotest_engine.Formatters.get_stderr () :> Format.formatter) ();
    after_test ~stdout:Stdlib.stdout ~stderr:Stdlib.stderr;
    match r with
    | Ok value -> value
    | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace

  let contains s1 s2 =
    let exception Found in
    try
      let len = String.length s2 in
      for i = 0 to String.length s1 - len do
        if String.sub s1 i len = s2 then raise_notrace Found
      done;
      false
    with Found -> true

  let setup_std_outputs ?style_renderer ?utf_8
      (stdout : Alcotest_engine.Formatters.stdout)
      (stderr : Alcotest_engine.Formatters.stderr) =
    let style_renderer oc =
      match style_renderer with
      | Some value -> value
      | None ->
          let dumb =
            match Sys.getenv "TERM" with
            | "dumb" | "" -> true
            | _ -> false
            | exception Not_found -> true
          in
          let is_a_tty =
            try Unix.(isatty (descr_of_out_channel oc))
            with Unix.Unix_error _ -> false
          in
          if (not dumb) && is_a_tty then `Ansi_tty else `None
    in
    let utf_8 =
      match utf_8 with
      | Some value -> value
      | None ->
          let has_utf_8 var =
            try contains "UTF-8" (String.uppercase_ascii (Sys.getenv var))
            with Not_found -> false
          in
          has_utf_8 "LANG" || has_utf_8 "LC_ALL" || has_utf_8 "LC_CTYPE"
    in
    Fmt.set_style_renderer
      (stdout :> Format.formatter)
      (style_renderer Stdlib.stdout);
    Fmt.set_utf_8 (stdout :> Format.formatter) utf_8;
    Fmt.set_style_renderer
      (stderr :> Format.formatter)
      (style_renderer Stdlib.stderr);
    Fmt.set_utf_8 (stderr :> Format.formatter) utf_8

  (* Implementation similar to that of [Bos.Os.Dir]. *)
  let home_directory () =
    let env_var_fallback () =
      try Ok (Sys.getenv "HOME")
      with Not_found -> Error (`Msg "HOME environment variable is undefined")
    in
    if Sys.win32 then env_var_fallback ()
    else
      try
        let uid = Unix.getuid () in
        Ok (Unix.getpwuid uid).Unix.pw_dir
      with Not_found -> env_var_fallback ()
end

module V1 = struct
  include Alcotest_engine.V1.Test

  module T =
    Alcotest_engine.V1.Cli.Make_with_log_trap
      (Unix_platform)
      (Alcotest_engine.Monad.Identity)

  include T
end

include V1
