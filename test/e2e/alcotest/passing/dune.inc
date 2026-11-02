(test
 (name and_exit_false)
 (package alcotest)
 (libraries alcotest fmt)
 (modules and_exit_false)
 (modes exe)
 (deps and_exit_false.expected)
 (action
  (progn
   (with-outputs-to and_exit_false.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} and_exit_false.actual))))

(test
 (name and_exit_true)
 (package alcotest)
 (libraries alcotest fmt)
 (modules and_exit_true)
 (modes exe)
 (deps and_exit_true.expected)
 (action
  (progn
   (with-outputs-to and_exit_true.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} and_exit_true.actual))))

(test
 (name assert_and_verbose)
 (package alcotest)
 (libraries alcotest fmt)
 (modules assert_and_verbose)
 (modes exe)
 (deps assert_and_verbose.expected)
 (action
  (progn
   (with-outputs-to assert_and_verbose.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} assert_and_verbose.actual))))

(test
 (name assert_not_printed)
 (package alcotest)
 (libraries alcotest fmt)
 (modules assert_not_printed)
 (modes exe)
 (deps assert_not_printed.expected)
 (action
  (progn
   (with-outputs-to assert_not_printed.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} assert_not_printed.actual))))

(test
 (name basic)
 (package alcotest)
 (libraries alcotest)
 (modules basic)
 (modes exe)
 (deps basic.expected)
 (action
  (progn
   (with-outputs-to basic.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} basic.actual))))

(test
 (name check_basic)
 (package alcotest)
 (libraries alcotest)
 (modules check_basic)
 (modes exe)
 (deps check_basic.expected)
 (action
  (progn
   (with-outputs-to check_basic.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} check_basic.actual))))

(test
 (name cli_verbose)
 (package alcotest)
 (libraries alcotest fmt)
 (modules cli_verbose)
 (modes exe)
 (deps cli_verbose.expected)
 (action
  (progn
   (with-outputs-to cli_verbose.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test} --verbose)))
   (run %{dep:../../strip_randomness.exe} cli_verbose.actual))))

(test
 (name compact)
 (package alcotest)
 (libraries alcotest alcotest.stdlib_ext fmt)
 (modules compact)
 (modes exe)
 (deps compact.expected)
 (action
  (progn
   (with-outputs-to compact.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test} --compact)))
   (run %{dep:../../strip_randomness.exe} compact.actual))))

(test
 (name empty_test_name)
 (package alcotest)
 (libraries alcotest)
 (modules empty_test_name)
 (modes exe)
 (deps empty_test_name.expected)
 (action
  (progn
   (with-outputs-to empty_test_name.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} empty_test_name.actual))))

(test
 (name explicit_command_options)
 (package alcotest)
 (libraries alcotest)
 (modules explicit_command_options)
 (modes exe)
 (deps explicit_command_options.expected)
 (action
  (progn
   (with-outputs-to explicit_command_options.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} explicit_command_options.actual))))

(test
 (name filter_name)
 (package alcotest)
 (libraries alcotest)
 (modules filter_name)
 (modes exe)
 (deps filter_name.expected)
 (action
  (progn
   (with-outputs-to filter_name.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} filter_name.actual))))

(test
 (name filter_name_regex)
 (package alcotest)
 (libraries alcotest)
 (modules filter_name_regex)
 (modes exe)
 (deps filter_name_regex.expected)
 (action
  (progn
   (with-outputs-to filter_name_regex.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} filter_name_regex.actual))))

(test
 (name filter_range)
 (package alcotest)
 (libraries alcotest alcotest.stdlib_ext fmt)
 (modules filter_range)
 (modes exe)
 (deps filter_range.expected)
 (action
  (progn
   (with-outputs-to filter_range.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test} test main 0..1,1-3,5)))
   (run %{dep:../../strip_randomness.exe} filter_range.actual))))

(test
 (name isatty)
 (package alcotest)
 (libraries alcotest alcotest.engine)
 (modules isatty)
 (modes exe)
 (deps isatty.expected)
 (action
  (progn
   (with-outputs-to isatty.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} isatty.actual))))

(test
 (name json_output)
 (package alcotest)
 (libraries alcotest)
 (modules json_output)
 (modes exe)
 (deps json_output.expected)
 (action
  (progn
   (with-outputs-to json_output.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} json_output.actual))))

(test
 (name list_tests)
 (package alcotest)
 (libraries alcotest)
 (modules list_tests)
 (modes exe)
 (deps list_tests.expected)
 (action
  (progn
   (with-outputs-to list_tests.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} list_tests.actual))))

(test
 (name match_raises)
 (package alcotest)
 (libraries alcotest)
 (modules match_raises)
 (modes exe)
 (deps match_raises.expected)
 (action
  (progn
   (with-outputs-to match_raises.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} match_raises.actual))))

(test
 (name only_monadic_effects)
 (package alcotest)
 (libraries alcotest fmt alcotest.engine)
 (modules only_monadic_effects)
 (modes exe)
 (deps only_monadic_effects.expected)
 (action
  (progn
   (with-outputs-to only_monadic_effects.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} only_monadic_effects.actual))))

(test
 (name quick_only)
 (package alcotest)
 (libraries alcotest)
 (modules quick_only)
 (modes exe)
 (deps quick_only.expected)
 (action
  (progn
   (with-outputs-to quick_only.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} quick_only.actual))))

(test
 (name quick_only_regex)
 (package alcotest)
 (libraries alcotest)
 (modules quick_only_regex)
 (modes exe)
 (deps quick_only_regex.expected)
 (action
  (progn
   (with-outputs-to quick_only_regex.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} quick_only_regex.actual))))

(test
 (name separator_testname)
 (package alcotest)
 (libraries alcotest)
 (modules separator_testname)
 (modes exe)
 (deps separator_testname.expected)
 (action
  (progn
   (with-outputs-to separator_testname.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} separator_testname.actual))))

(test
 (name skip_in_test)
 (package alcotest)
 (libraries alcotest)
 (modules skip_in_test)
 (modes exe)
 (deps skip_in_test.expected)
 (action
  (progn
   (with-outputs-to skip_in_test.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} skip_in_test.actual))))

(test
 (name unicode_testname)
 (package alcotest)
 (libraries alcotest)
 (modules unicode_testname)
 (modes exe)
 (deps unicode_testname.expected)
 (action
  (progn
   (with-outputs-to unicode_testname.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} unicode_testname.actual))))

(test
 (name verbose_newlines)
 (package alcotest)
 (libraries alcotest fmt)
 (modules verbose_newlines)
 (modes exe)
 (deps verbose_newlines.expected)
 (action
  (progn
   (with-outputs-to verbose_newlines.actual
    (with-accepted-exit-codes (or 0 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} verbose_newlines.actual))))
