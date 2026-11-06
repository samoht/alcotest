open! Import

type outcome =
  | Passed
  | Skipped
  | Failed of { kind : string; text : string }
  | Errored of { kind : string; text : string }

type case = {
  group : string;
  name : string;
  index : int;
  time : float;
  location : (string * int) option;
  outcome : outcome;
  output : string option;
}

type t = {
  suite : string;
  file : string option;
  timestamp : string;
  tests : int;
  failures : int;
  errors : int;
  time : float;
  names : String.Set.t;
  cases : string list;
}

let v ~suite ~file ~timestamp =
  {
    suite;
    file;
    timestamp;
    tests = 0;
    failures = 0;
    errors = 0;
    time = 0.;
    names = String.Set.empty;
    cases = [];
  }

let colour = Re.(compile (seq [ str "\027["; rep (rg ' ' '?'); rg '@' '~' ]))

(* Whether XML 1.0 allows [u]. Surrogates cannot be a [Uchar.t], so they need
   no check. *)
let is_legal u =
  let c = Uchar.to_int u in
  (c >= 0x20 && c <> 0xFFFE && c <> 0xFFFF) || c = 0x9 || c = 0xA || c = 0xD

let sanitise s =
  let s = Re.replace_string ~all:true colour ~by:"" s in
  let buf = Buffer.create (String.length s) in
  String.fold_utf8
    (fun () _ d ->
      let u = Uchar.utf_decode_uchar d in
      Buffer.add_utf_8_uchar buf (if is_legal u then u else Uchar.rep))
    () s;
  Buffer.contents buf

let escape ~attr s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '\r' -> Buffer.add_string buf "&#13;"
      | '"' when attr -> Buffer.add_string buf "&quot;"
      | '\t' when attr -> Buffer.add_string buf "&#9;"
      | '\n' when attr -> Buffer.add_string buf "&#10;"
      | c -> Buffer.add_char buf c)
    (sanitise s);
  Buffer.contents buf

let text = escape ~attr:false
let attribute (k, v) = Fmt.str " %s=\"%s\"" k (escape ~attr:true v)

let elt tag attrs body =
  let attrs = String.concat (List.map attribute attrs) in
  match body with
  | [] -> Fmt.str "<%s%s/>\n" tag attrs
  | _ -> Fmt.str "<%s%s>%s</%s>\n" tag attrs (String.concat body) tag

let seconds t = Fmt.str "%.3f" t

(* The text of a failure starts with its location, when it has one. *)
let failure_location text =
  try Scanf.sscanf text "File %S, line %d" (fun file line -> Some (file, line))
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> None

let failure_text = function
  | Passed | Skipped -> ""
  | Failed { text; _ } | Errored { text; _ } -> text

let first_line s =
  match String.cut ~sep:"\n" s with Some (l, _) -> l | None -> s

(* The first line of a failure, skipping its location. *)
let message text =
  match failure_location text with
  | Some _ -> (
      match String.cut ~sep:"\n" text with
      | Some (_, rest) -> first_line rest
      | None -> "")
  | None -> first_line text

let problem tag kind body =
  elt tag [ ("message", message body); ("type", kind) ] [ text body ]

let outcome_body = function
  | Passed -> []
  | Skipped -> [ elt "skipped" [] [] ]
  | Failed { kind; text = s } -> [ problem "failure" kind s ]
  | Errored { kind; text = s } -> [ problem "error" kind s ]

(* The logs of a failed test end with the failure, which the report already
   shows, so they are cut where it starts. *)
let output_body outcome = function
  | None -> []
  | Some logs -> (
      let logs = sanitise logs in
      let failure = sanitise (first_line (failure_text outcome)) in
      let logs =
        match String.find_sub ~rev:true ~sub:failure logs with
        | Some i when failure <> "" -> String.with_range ~len:i logs
        | _ -> logs
      in
      (* GitLab rejects a report over 30 MB: keep the end of long logs. *)
      let max = 64 * 1024 in
      let logs =
        if String.length logs <= max then logs
        else String.with_range ~first:(String.length logs - max) logs
      in
      match String.trim logs with
      | "" -> []
      | logs -> [ elt "system-out" [] [ text logs ] ])

(* GitLab keeps one test per class and name, so a repeated name gets the index
   of its test. *)
let unique_name t ~classname { name; index; _ } =
  if String.Set.mem (classname ^ "\000" ^ name) t.names then
    Fmt.str "%s #%d" name index
  else name

let add t case =
  let classname =
    if case.group = "" then t.suite else t.suite ^ "." ^ case.group
  in
  let name = unique_name t ~classname case in
  let xml =
    elt "testcase"
      ([ ("name", name); ("classname", classname); ("time", seconds case.time) ]
      @
      match
        Option.(failure_location (failure_text case.outcome) || case.location)
      with
      | None -> []
      | Some (file, line) -> [ ("file", file); ("line", string_of_int line) ])
      (outcome_body case.outcome @ output_body case.outcome case.output)
  in
  let t =
    {
      t with
      tests = t.tests + 1;
      names = String.Set.add (classname ^ "\000" ^ name) t.names;
      time = t.time +. case.time;
      cases = xml :: t.cases;
    }
  in
  match case.outcome with
  | Passed | Skipped -> t
  | Failed _ -> { t with failures = t.failures + 1 }
  | Errored _ -> { t with errors = t.errors + 1 }

let to_string t =
  let suite =
    elt "testsuite"
      ([ ("name", t.suite) ]
      @ Option.fold ~none:[] ~some:(fun f -> [ ("file", f) ]) t.file
      @ [
          ("tests", string_of_int t.tests);
          ("failures", string_of_int t.failures);
          ("errors", string_of_int t.errors);
          ("time", seconds t.time);
          ("timestamp", t.timestamp);
        ])
      (List.rev t.cases)
  in
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  ^ elt "testsuites" [ ("time", seconds t.time) ] [ suite ]
