(lang dune 3.20)

(name Lab2)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name Lab2)
 (synopsis "Immutable polymorphic HashMap implementation")
 (description "A functional HashMap data structure with monoid properties, implemented using open addressing. Includes comprehensive unit and property-based tests.")
 (depends 
  (ocaml (>= 4.14))
  dune
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test))
 (tags
  ("functional programming" "data structures" "hashmap" "immutable")))