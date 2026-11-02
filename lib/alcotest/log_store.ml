module Run_id = Alcotest_engine.Private.Run_id

type identity = int * int
type store = { root : Fpath.t; identity : identity; metadata : Fpath.t }

type t = {
  store : store;
  id : Run_id.t;
  name : string;
  lease : Unix.file_descr;
}

(* POSIX record locks belong to the process. Never open and close a second
   descriptor for an in-process lease: that would release the original lock. *)
let active : (identity * string) list ref = ref []
let ( / ) = Fpath.( / )
let path = Fpath.to_string
let refuse p why = Fmt.failwith "Alcotest log ownership: %a: %s" Fpath.pp p why

let stat p =
  try Some (Unix.lstat (path p))
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let identity s = (s.Unix.st_dev, s.Unix.st_ino)
let same a b = identity a = identity b

let owned p kind =
  match stat p with
  | Some s ->
      let recognized =
        match (kind, s.Unix.st_kind) with
        | Unix.S_REG, Unix.S_REG
        | Unix.S_DIR, Unix.S_DIR
        | Unix.S_LNK, Unix.S_LNK ->
            true
        | _ -> false
      in
      if recognized && s.Unix.st_uid = Unix.getuid () then s
      else refuse p "expected an owned object of the declared kind"
  | None -> refuse p "expected object is missing"

let empty_regular p =
  let s = owned p Unix.S_REG in
  if s.Unix.st_size <> 0 || s.Unix.st_nlink <> 1 then
    refuse p "expected an unshared empty lease or format file";
  s

let rec mkdir_p p =
  match stat p with
  | Some s when s.Unix.st_kind = Unix.S_DIR -> ()
  | Some s when s.Unix.st_kind = Unix.S_LNK ->
      if (Unix.stat (path p)).Unix.st_kind <> Unix.S_DIR then
        refuse p "configured log-root link does not resolve to a directory"
  | Some _ -> refuse p "log root is not a directory"
  | None -> (
      mkdir_p (Fpath.parent p);
      try Unix.mkdir (path p) 0o770
      with Unix.Unix_error (Unix.EEXIST, _, _) ->
        if (Unix.stat (path p)).Unix.st_kind <> Unix.S_DIR then
          refuse p "concurrent root creation found a non-directory")

let close_on_exec fd =
  try
    Unix.set_close_on_exec fd;
    fd
  with exn ->
    Unix.close fd;
    raise exn

let empty p =
  Unix.openfile (path p) [ Unix.O_CREAT; Unix.O_EXCL; Unix.O_RDWR ] 0o600
  |> close_on_exec

let open_empty p =
  let before = empty_regular p in
  let fd = Unix.openfile (path p) [ Unix.O_RDWR ] 0 |> close_on_exec in
  let after = Unix.fstat fd in
  if not (same before after) then (
    Unix.close fd;
    refuse p "object identity changed while opening");
  fd

let locked root f =
  let p = root / ".alcotest.lock" in
  let fd =
    match stat p with
    | Some _ -> open_empty p
    | None -> (
        try empty p with Unix.Unix_error (Unix.EEXIST, _, _) -> open_empty p)
  in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.lockf fd Unix.F_LOCK 0;
      f ())

let id_exn p s =
  match Run_id.of_string s with
  | Some id -> id
  | None -> refuse p "invalid registered execution identifier"

let run_dir store id = store.root / Run_id.to_string id
let marker store id = store.metadata / "runs" / Run_id.to_string id

let link_target p =
  ignore (owned p Unix.S_LNK);
  match Fpath.of_string (Unix.readlink (path p)) with
  | Ok target -> target
  | Error (`Msg why) -> refuse p why

let read_reference store p =
  let target = link_target p in
  let id = id_exn p (Fpath.basename target) in
  if not (Fpath.equal target (run_dir store id)) then
    refuse p "reference does not name a canonical run in this root";
  ignore (empty_regular (marker store id));
  ignore (owned (run_dir store id) Unix.S_DIR);
  id

let entries p =
  ignore (owned p Unix.S_DIR);
  Sys.readdir (path p) |> Array.to_list |> List.sort String.compare

let initialize root identity =
  let metadata = root / ".alcotest" in
  (match stat metadata with
  | None -> Unix.mkdir (path metadata) 0o700
  | Some _ -> ignore (owned metadata Unix.S_DIR));
  let format = metadata / "format-v1" in
  (match stat format with
  | None ->
      if entries metadata <> [] then
        refuse metadata "unrecognized or incomplete ownership metadata";
      Unix.close (empty format)
  | Some _ -> ignore (empty_regular format));
  List.iter
    (fun name ->
      if
        not
          (List.mem name
             [ "format-v1"; "runs"; "latest"; "failures"; "pending" ])
      then refuse (metadata / name) "unrecognized ownership metadata")
    (entries metadata);
  List.iter
    (fun name ->
      let p = metadata / name in
      match stat p with
      | None -> Unix.mkdir (path p) 0o700
      | Some _ -> ignore (owned p Unix.S_DIR))
    [ "runs"; "latest"; "failures" ];
  let store = { root; identity; metadata } in
  let pending = metadata / "pending" in
  (match stat pending with
  | None -> ()
  | Some _ ->
      let target = link_target pending in
      if Fpath.equal (Fpath.parent target) (metadata / "latest") then
        ignore (read_reference store target)
      else ignore (read_reference store pending);
      Unix.unlink (path pending));
  store

let references store ~replace_latest =
  List.concat
    (List.map
       (fun family ->
         let directory = store.metadata / family in
         List.filter_map
           (fun name ->
             let p = directory / name in
             let id = read_reference store p in
             (* Verify even the reference being replaced before mutation. *)
             match (family, replace_latest) with
             | "latest", Some (suite, _) when suite = name -> None
             | _ -> Some (Run_id.to_string id))
           (entries directory))
       [ "latest"; "failures" ])
  @
  match replace_latest with
  | None -> []
  | Some (_, id) -> [ Run_id.to_string id ]

let publish store destination target =
  let pending = store.metadata / "pending" in
  if stat pending <> None then refuse pending "publication slot is occupied";
  Unix.symlink ~to_dir:true (path target) (path pending);
  Unix.rename (path pending) (path destination)

let public_alias store name target =
  let p = store.root / name in
  (* A suite may have the spelling of a reserved name or an execution ID. *)
  if
    name <> ".alcotest"
    && name <> ".alcotest.lock"
    && Run_id.of_string name = None
  then
    match stat p with
    | Some s when s.Unix.st_kind <> Unix.S_LNK -> ()
    | Some _ | None -> publish store p target

let removable store id =
  let directory = run_dir store id in
  match stat directory with
  | None -> (None, [])
  | Some _ ->
      let directory_identity = owned directory Unix.S_DIR in
      Unix.access (path directory) [ Unix.W_OK; Unix.X_OK ];
      let files =
        List.map
          (fun name ->
            let p = directory / name in
            if not (Filename.check_suffix name ".output") then
              refuse p "unrecognized file in a retired run";
            let identity = owned p Unix.S_REG in
            if identity.Unix.st_nlink <> 1 then
              refuse p "retired output has an unknown hard link";
            (p, identity))
          (entries directory)
      in
      (Some directory_identity, files)

let is_active store id =
  let key = (store.identity, Run_id.to_string id) in
  if List.mem key !active then true
  else
    let fd = open_empty (marker store id) in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        try
          Unix.lockf fd Unix.F_TLOCK 0;
          false
        with Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) -> true)

let plan store ~replace_latest ~finishing =
  let retained = references store ~replace_latest in
  entries (store.metadata / "runs")
  |> List.filter_map (fun name ->
      let id = id_exn (store.metadata / "runs" / name) name in
      ignore (empty_regular (marker store id));
      if List.mem name retained || (Some name <> finishing && is_active store id)
      then None
      else Some (id, removable store id))

let retire store candidates =
  Unix.access (path store.root) [ Unix.W_OK; Unix.X_OK ];
  Unix.access (path (store.metadata / "runs")) [ Unix.W_OK; Unix.X_OK ];
  (* Every candidate was validated before deleting any; repeat identity checks
     before unlinking. No recursive operation can traverse an unknown object. *)
  List.iter
    (fun (id, (directory_identity, files)) ->
      let directory = run_dir store id in
      (match (directory_identity, stat directory) with
      | None, None -> ()
      | Some before, Some _ when same before (owned directory Unix.S_DIR) -> ()
      | _ -> refuse directory "retired directory identity changed");
      List.iter
        (fun (p, identity) ->
          if not (same identity (owned p Unix.S_REG)) then
            refuse p "retired output identity changed")
        files)
    candidates;
  List.iter
    (fun (id, (_, files)) ->
      List.iter (fun (p, _) -> Unix.unlink (path p)) files;
      let directory = run_dir store id in
      (match stat directory with
      | None -> ()
      | Some _ -> Unix.rmdir (path directory));
      Unix.unlink (path (marker store id)))
    candidates

let root_path root =
  let root = Fpath.v root in
  if Fpath.is_abs root then root else Fpath.(v (Sys.getcwd ()) // root)

let verify_root root expected =
  let observed = Unix.stat (path root) in
  if
    observed.Unix.st_kind <> Unix.S_DIR
    || observed.Unix.st_uid <> Unix.getuid ()
    || identity observed <> expected
  then
    let dev, ino = expected and actual_dev, actual_ino = identity observed in
    refuse root
      (Fmt.str "resolved root identity changed: expected %d:%d, observed %d:%d"
         dev ino actual_dev actual_ino)

let start ~root ~uuid ~name =
  let root = root_path root in
  mkdir_p root;
  let root_stat = Unix.stat (path root) in
  if
    root_stat.Unix.st_kind <> Unix.S_DIR
    || root_stat.Unix.st_uid <> Unix.getuid ()
  then refuse root "configured log root is not an owned directory";
  let identity = identity root_stat in
  if (not (Fpath.is_seg name)) || name = "." || name = ".." then
    refuse root "suite name is not a path segment";
  locked root (fun () ->
      verify_root root identity;
      let store = initialize root identity in
      let id = id_exn root uuid in
      let candidates =
        plan store ~replace_latest:(Some (name, id)) ~finishing:None
      in
      if stat (run_dir store id) <> None then
        refuse (run_dir store id) "execution ID already exists";
      let lease = empty (marker store id) in
      try
        Unix.lockf lease Unix.F_TLOCK 0;
        active := (identity, uuid) :: !active;
        Unix.mkdir (path (run_dir store id)) 0o770;
        let latest = store.metadata / "latest" / name in
        publish store latest (run_dir store id);
        public_alias store name latest;
        public_alias store "latest" latest;
        retire store candidates;
        { store; id; name; lease }
      with exn ->
        active := List.filter (( <> ) (identity, uuid)) !active;
        Unix.close lease;
        raise exn)

let finish t ~failed =
  let release () =
    active :=
      List.filter (( <> ) (t.store.identity, Run_id.to_string t.id)) !active;
    Unix.close t.lease
  in
  Fun.protect ~finally:release (fun () ->
      locked t.store.root (fun () ->
          verify_root t.store.root t.store.identity;
          ignore (references t.store ~replace_latest:None);
          if failed then
            publish t.store
              (t.store.metadata / "failures" / t.name)
              (run_dir t.store t.id);
          let candidates =
            plan t.store ~replace_latest:None
              ~finishing:(Some (Run_id.to_string t.id))
          in
          retire t.store candidates))

let prepare_legacy ~root ~uuid ~name =
  let root = root_path root in
  let directory = root / uuid in
  match stat directory with
  | Some _ -> ignore (owned directory Unix.S_DIR)
  | None ->
      mkdir_p directory;
      if (Sys.unix || Sys.cygwin) && Unix.has_symlink () then
        locked root (fun () ->
            List.iter
              (fun name ->
                let alias = root / name in
                match stat alias with
                | Some s when s.Unix.st_kind <> Unix.S_LNK -> ()
                | previous ->
                    if previous <> None then Unix.unlink (path alias);
                    Unix.symlink ~to_dir:true (path directory) (path alias))
              [ name; "latest" ])
