(lang dune 3.11)

(name ocawk)

(generate_opam_files true)

(source
 (github PWok/Ocawk))

(authors "Wojciech Orański")

(maintainers "Wojciech Orański")

(package
 (name ocawk)
 (depends ocaml dune)
 (allow_empty)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(using menhir 2.1)
