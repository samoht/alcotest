type stdout = Format.formatter
type stderr = Format.formatter

let stdout = ref Fmt.stdout
let stderr = ref Fmt.stderr
let set_stdout stdout' = stdout := stdout'
let set_stderr stderr' = stderr := stderr'
let get_stdout () = !stdout
let get_stderr () = !stderr
let ocaml_stdout = Format.std_formatter
let ocaml_stderr = Format.err_formatter
let make_stdout () = Format.formatter_of_out_channel Stdlib.stdout
let make_stderr () = Format.formatter_of_out_channel Stdlib.stderr
let pr fmt = Format.fprintf !stdout fmt
let epr fmt = Format.fprintf !stderr fmt
