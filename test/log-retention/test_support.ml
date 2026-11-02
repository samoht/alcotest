let rec remove path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_directory f =
  let path = Filename.temp_file "alcotest-retention-" "" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> remove path) (fun () -> f path)

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel contents)

let spawn ~input ~output program arguments =
  let fd =
    Unix.openfile output [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  Unix.set_close_on_exec fd;
  match Unix.fork () with
  | 0 -> (
      (* The native equivalent of start_new_session: every helper has its own
         session before exec, without touching another process's group. *)
      try
        ignore (Unix.setsid ());
        let input = Unix.dup ~cloexec:true input in
        let output = Unix.dup ~cloexec:true fd in
        Unix.dup2 ~cloexec:false input Unix.stdin;
        Unix.dup2 ~cloexec:false output Unix.stdout;
        Unix.dup2 ~cloexec:false output Unix.stderr;
        Unix.execv program (Array.of_list (program :: arguments))
      with Unix.Unix_error (error, call, argument) ->
        Fmt.epr "%s(%S): %s@." call argument (Unix.error_message error);
        Unix._exit 127)
  | pid ->
      Unix.close fd;
      pid

let rec wait pid =
  try snd (Unix.waitpid [] pid)
  with Unix.Unix_error (Unix.EINTR, _, _) -> wait pid

let run ~output program arguments =
  let input = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  Unix.set_close_on_exec input;
  let pid =
    Fun.protect
      ~finally:(fun () -> Unix.close input)
      (fun () -> spawn ~input ~output program arguments)
  in
  let result = wait pid in
  (result, read output)
