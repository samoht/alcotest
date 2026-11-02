(lang dune 3.24)
(using mdx 0.4)
(implicit_transitive_deps false)
(generate_opam_files true)

(name alcotest)
(source (uri https://github.com/samoht/alcotest))
(license ISC)
(authors "Thomas Gazagnaire")
(maintainers "thomas@gazagnaire.org")

(package
 (name alcotest)
 (tags (org:blacksun test))
 (synopsis "Alcotest is a lightweight and colourful test framework")
 (documentation "https://mirage.github.io/alcotest")
 (description "\
Alcotest exposes simple interface to perform unit tests. It exposes
a simple TESTABLE module type, a check function to assert test
predicates and a run function to perform a list of unit -> unit
test callbacks.

Alcotest provides a quiet and colorful output where only faulty runs
are fully displayed at the end of the run (with the full logs ready to
inspect), with a simple (yet expressive) query language to select the
tests to run.
")
 (depends
  (ocaml (>= 4.08))
  (fmt (>= 0.8.7))
  astring
  fpath
  base-unix
  (mdx :with-test)
  (cmdliner (>= 1.2.0))
  (re (>= 1.7.2))
  stdlib-shims
  (uutf (>= 1.0.1))
  ocaml-syntax-shims)
 (conflicts
  (result (< 1.5)) ;; Ensure `Result.result` = `Stdlib.result` when possible
  ))
