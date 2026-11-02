type dimensions = { rows : int; columns : int }

external dimensions : unit -> dimensions option
  = "ocaml_alcotest_get_terminal_dimensions"
