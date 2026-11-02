//Provides: alcotest_saved_stdout
var alcotest_saved_stdout
//Provides: alcotest_saved_stderr
var alcotest_saved_stderr

//Provides: alcotest_before_test
//Requires: caml_ml_channel_redirect
//Requires: alcotest_saved_stderr, alcotest_saved_stdout
function alcotest_before_test (voutput, vstdout, vstderr){
  alcotest_saved_stderr = caml_ml_channel_redirect(vstderr,voutput);
  alcotest_saved_stdout = caml_ml_channel_redirect(vstdout,voutput);
  return 0;
}

//Provides: alcotest_after_test
//Requires: caml_ml_channel_restore
//Requires: alcotest_saved_stderr, alcotest_saved_stdout
function alcotest_after_test (vstdout, vstderr){
  caml_ml_channel_restore(vstdout, alcotest_saved_stdout);
  caml_ml_channel_restore(vstderr, alcotest_saved_stderr);
  return 0;
}

//Provides: ocaml_alcotest_get_terminal_dimensions
function ocaml_alcotest_get_terminal_dimensions(unit) {
  var p = globalThis.process
  if(p && p.stdout && p.stdout.columns && p.stdout.rows) {
    return [0, [0, p.stdout.rows, p.stdout.columns]];
  }
  return 0;
}
