(lang dune 3.11)

(name ocawk)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Wojciech Orański")

(maintainers "Wojciech Orański")

(package
 (name ocawk)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (allow_empty)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(using menhir 2.1)
