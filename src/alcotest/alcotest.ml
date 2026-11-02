module Unix_platform (M : Alcotest_engine.Monad.S) = struct
  module M = Alcotest_engine.Monad.Extend (M)

  module Unix = struct
    open Astring
    include Unix

    let mkdir_p path mode =
      let is_win_drive_letter x =
        String.length x = 2 && x.[1] = ':' && Char.Ascii.is_letter x.[0]
      in
      let sep = Filename.dir_sep in
      let rec mk parent = function
        | [] -> ()
        | name :: names ->
            let path = parent ^ sep ^ name in
            (try Unix.mkdir path mode
             with Unix.Unix_error (Unix.EEXIST, _, _) ->
               if Sys.is_directory path then () (* the directory exists *)
               else Fmt.str "mkdir: %s: is a file" path |> failwith);
            mk path names
      in
      match String.cuts ~empty:true ~sep path with
      | "" :: xs -> mk sep xs
      (* check for Windows drive letter *)
      | dl :: xs when is_win_drive_letter dl -> mk dl xs
      | xs -> mk "." xs
  end

  open M.Syntax

  let time = Unix.gettimeofday
  let getcwd = Sys.getcwd

  let unlink_if_exists file =
    let rec inner ~retries =
      try Unix.unlink file with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
      | Unix.Unix_error (Unix.EINTR, _, _) ->
          if retries > 5 then
            Fmt.failwith "Failed %d times to unlink file %s (Unix.EINTR)."
              retries file
          else inner ~retries:(retries + 1)
    in
    inner ~retries:0

  let symlink ~to_dir ~target ~link_name =
    let rec inner ~retries =
      try Unix.symlink ~to_dir target link_name
      with Unix.Unix_error (Unix.EEXIST, _, _) ->
        if retries > 5 then
          Fmt.failwith "Failed %d times to create symlink %s (Unix.EEXIST)"
            retries target
        else (
          unlink_if_exists link_name;
          inner ~retries:(retries + 1))
    in
    inner ~retries:0

  let prepare_log_trap ~root ~uuid ~name =
    let dir = Filename.concat root uuid in
    if not (Sys.file_exists dir) then (
      Unix.mkdir_p dir 0o770;
      if (Sys.unix || Sys.cygwin) && Unix.has_symlink () then (
        let this_exe = Filename.concat root name
        and latest = Filename.concat root "latest" in
        unlink_if_exists this_exe;
        unlink_if_exists latest;
        symlink ~to_dir:true ~target:dir ~link_name:this_exe;
        symlink ~to_dir:true ~target:dir ~link_name:latest))
    else if not (Sys.is_directory dir) then
      Fmt.failwith "exists but is not a directory: %S" dir

  let stdout_isatty () = Unix.(isatty stdout)

  let stdout_columns () =
    match Option.bind (Sys.getenv_opt "ALCOTEST_COLUMNS") int_of_string_opt with
    | Some columns when columns > 0 -> Some columns
    | _ -> (
        match Terminal.get_dimensions () with
        | Some { columns; _ } when columns > 0 -> Some columns
        | _ -> None)

  external before_test :
    output:out_channel -> stdout:out_channel -> stderr:out_channel -> unit
    = "alcotest_before_test"

  external after_test : stdout:out_channel -> stderr:out_channel -> unit
    = "alcotest_after_test"

  type file_descriptor = out_channel

  let log_trap_supported = true
  let file_exists = Sys.file_exists
  let open_write_only = open_out
  let close = close_out

  let with_redirect fd_file fn =
    let* () = M.return () in
    Fmt.flush (Alcotest_engine.Formatters.get_stdout () :> Format.formatter) ();
    Fmt.flush (Alcotest_engine.Formatters.get_stderr () :> Format.formatter) ();
    before_test ~output:fd_file ~stdout:Stdlib.stdout ~stderr:Stdlib.stderr;
    let+ r = try fn () >|= fun o -> `Ok o with e -> M.return @@ `Error e in
    Fmt.flush (Alcotest_engine.Formatters.get_stdout () :> Format.formatter) ();
    Fmt.flush (Alcotest_engine.Formatters.get_stderr () :> Format.formatter) ();
    after_test ~stdout:Stdlib.stdout ~stderr:Stdlib.stderr;
    match r with `Ok x -> x | `Error e -> raise e

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
            | exception _ -> true
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
    Alcotest_engine.V1.Cli.Make (Unix_platform) (Alcotest_engine.Monad.Identity)

  include T
end

include V1
