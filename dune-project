(lang dune 3.20)

(name lumen)

(generate_opam_files true)

(source
 (github bxrne/lumen))

(authors "Adam Byrne <adamrbyrne@gmail.com>")

(maintainers "Adam Byrne <adamrbyrne@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lumen)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml ounit2)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html