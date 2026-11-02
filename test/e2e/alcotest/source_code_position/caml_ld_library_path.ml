let ( / ) = Filename.concat
let sep = if Sys.win32 then ";" else ":"

let () =
  if Array.length Sys.argv <> 2 then exit 1;
  let test = "." / Sys.argv.(1) in
  let lib = ".." / ".." / ".." / ".." / "src" / "alcotest" in
  let paths =
    match Sys.getenv "CAML_LD_LIBRARY_PATH" with
    | paths -> lib ^ sep ^ paths
    | exception Not_found -> lib
  in
  Unix.putenv "CAML_LD_LIBRARY_PATH" paths;
  let child =
    Unix.create_process test [||] Unix.stdin Unix.stdout Unix.stderr
  in
  match Unix.waitpid [ WUNTRACED ] child with
  | _, WEXITED c -> exit c
  | _ -> exit 1
