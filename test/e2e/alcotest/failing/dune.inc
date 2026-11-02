(test
 (name bail)
 (package alcotest)
 (libraries alcotest)
 (modules bail)
 (modes exe)
 (deps bail.expected)
 (action
  (progn
   (with-outputs-to bail.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} bail.actual))))

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
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} check_basic.actual))))

(test
 (name check_located)
 (package alcotest)
 (libraries alcotest)
 (modules check_located)
 (modes exe)
 (deps check_located.expected)
 (action
  (progn
   (with-outputs-to check_located.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} check_located.actual))))

(test
 (name check_long)
 (package alcotest)
 (libraries alcotest alcotest.stdlib_ext)
 (modules check_long)
 (modes exe)
 (deps check_long.expected)
 (action
  (progn
   (with-outputs-to check_long.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} check_long.actual))))

(test
 (name compact)
 (package alcotest)
 (libraries alcotest)
 (modules compact)
 (modes exe)
 (deps compact.expected)
 (action
  (progn
   (with-outputs-to compact.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} compact.actual))))

(test
 (name duplicate_test_names)
 (package alcotest)
 (libraries alcotest)
 (modules duplicate_test_names)
 (modes exe)
 (deps duplicate_test_names.expected)
 (action
  (progn
   (with-outputs-to duplicate_test_names.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} duplicate_test_names.actual))))

(test
 (name empty_suite_name)
 (package alcotest)
 (libraries alcotest)
 (modules empty_suite_name)
 (modes exe)
 (deps empty_suite_name.expected)
 (action
  (progn
   (with-outputs-to empty_suite_name.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} empty_suite_name.actual))))

(test
 (name exception_in_test)
 (package alcotest)
 (libraries alcotest)
 (modules exception_in_test)
 (modes exe)
 (deps exception_in_test.expected)
 (action
  (progn
   (with-outputs-to exception_in_test.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} exception_in_test.actual))))

(test
 (name filter_all_tests)
 (package alcotest)
 (libraries alcotest)
 (modules filter_all_tests)
 (modes exe)
 (deps filter_all_tests.expected)
 (action
  (progn
   (with-outputs-to filter_all_tests.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test} test bar 1)))
   (run %{dep:../../strip_randomness.exe} filter_all_tests.actual))))

(test
 (name invalid_arg_in_test)
 (package alcotest)
 (libraries alcotest)
 (modules invalid_arg_in_test)
 (modes exe)
 (deps invalid_arg_in_test.expected)
 (action
  (progn
   (with-outputs-to invalid_arg_in_test.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} invalid_arg_in_test.actual))))

(test
 (name long_test_case_name)
 (package alcotest)
 (libraries alcotest)
 (modules long_test_case_name)
 (modes exe)
 (deps long_test_case_name.expected)
 (action
  (progn
   (with-outputs-to long_test_case_name.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} long_test_case_name.actual))))

(test
 (name outside_runner)
 (package alcotest)
 (libraries alcotest)
 (modules outside_runner)
 (modes exe)
 (deps outside_runner.expected)
 (action
  (progn
   (with-outputs-to outside_runner.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} outside_runner.actual))))

(test
 (name tail_errors_limit)
 (package alcotest)
 (libraries alcotest fmt)
 (modules tail_errors_limit)
 (modes exe)
 (deps tail_errors_limit.expected)
 (action
  (progn
   (with-outputs-to tail_errors_limit.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} tail_errors_limit.actual))))

(test
 (name tail_errors_unlimited)
 (package alcotest)
 (libraries alcotest fmt)
 (modules tail_errors_unlimited)
 (modes exe)
 (deps tail_errors_unlimited.expected)
 (action
  (progn
   (with-outputs-to tail_errors_unlimited.actual
    (with-accepted-exit-codes (or 1 2 124 125)
     (run %{test})))
   (run %{dep:../../strip_randomness.exe} tail_errors_unlimited.actual))))
